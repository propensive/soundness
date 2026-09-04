"""Synchronize ~/.ivy2/local with the Soundness jars on GitHub Releases.

Invoked by `etc/ci/sync-releases.sh`; see its header for usage. Each released jar embeds its
POM and ivy.xml under `META-INF/maven/<group>/<artifactId>/`, so the jar alone reconstructs
the ivy2 layout coursier expects:

    <ivy-local>/<group>/<artifactId>/<version>/jars/<artifactId>.jar
                                             /poms/<artifactId>.pom
                                             /ivys/ivy.xml
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import sys
import tempfile
import urllib.request
import zipfile
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

GROUP = "dev.propensive"
VERSION_TAG = re.compile(r"^\d+\.\d+\.\d+$")
DIGEST_PREFIX = "sha256:"
WORKERS = 8


@dataclass
class Asset:
    artifact: str
    version: str
    source: str            # URL, or a local path when syncing a staged directory
    digest: str | None     # lowercase SHA-256 hex, when known in advance


def log(message: str) -> None:
    print(f"sync-releases: {message}", flush=True)


def fail(message: str) -> None:
    print(f"sync-releases: {message}", file=sys.stderr)
    sys.exit(1)


# ---- GitHub ------------------------------------------------------------------

def api(url: str) -> object:
    request = urllib.request.Request(url, headers={"Accept": "application/vnd.github+json"})
    token = os.environ.get("GITHUB_TOKEN")
    if token:
        request.add_header("Authorization", f"Bearer {token}")
    with urllib.request.urlopen(request) as response:
        return json.load(response)


def releases(repo: str) -> list[dict]:
    """Every release of `repo`, paging until the API returns an empty page."""
    found: list[dict] = []
    page = 1
    while True:
        batch = api(f"https://api.github.com/repos/{repo}/releases?per_page=100&page={page}")
        if not batch:
            return found
        found.extend(batch)
        page += 1


def release_assets(release: dict) -> list[Asset]:
    version = release["tag_name"]
    suffix = f"-{version}.jar"
    assets: list[Asset] = []
    for asset in release.get("assets", []):
        name = asset["name"]
        if not name.endswith(suffix):
            continue
        digest = asset.get("digest") or ""
        digest = digest[len(DIGEST_PREFIX):].lower() if digest.startswith(DIGEST_PREFIX) else None
        assets.append(Asset(name[: -len(suffix)], version, asset["browser_download_url"], digest))
    return assets


def staged_assets(directory: Path) -> list[Asset]:
    """The jars of a local `./mill release.stage`, named `<artifactId>-<version>.jar`."""
    assets: list[Asset] = []
    for jar in sorted(directory.glob("*.jar")):
        match = re.match(r"^(.+)-(\d+\.\d+\.\d+(?:-[\w.]+)?)\.jar$", jar.name)
        if match is None:
            fail(f"staged jar {jar.name} is not named <artifactId>-<version>.jar")
        assets.append(Asset(match.group(1), match.group(2), str(jar), None))
    return assets


# ---- ivy2 layout -------------------------------------------------------------

def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as file:
        for chunk in iter(lambda: file.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def fetch(asset: Asset, destination: Path) -> None:
    if asset.source.startswith("http"):
        with urllib.request.urlopen(asset.source) as response, destination.open("wb") as file:
            shutil.copyfileobj(response, file)
    else:
        shutil.copy(asset.source, destination)


def install(asset: Asset, ivy_local: Path) -> str:
    """Installs one asset into the ivy2 layout; returns `fresh`, `current` or `repaired`."""
    home = ivy_local / GROUP / asset.artifact / asset.version
    jar = home / "jars" / f"{asset.artifact}.jar"
    pom = home / "poms" / f"{asset.artifact}.pom"
    ivy = home / "ivys" / "ivy.xml"

    if jar.exists() and pom.exists() and ivy.exists():
        if asset.digest is None or sha256(jar) == asset.digest:
            return "current"
        outcome = "repaired"
    else:
        outcome = "fresh"

    with tempfile.TemporaryDirectory() as scratch:
        downloaded = Path(scratch) / jar.name
        fetch(asset, downloaded)
        actual = sha256(downloaded)
        if asset.digest is not None and actual != asset.digest:
            raise RuntimeError(f"{asset.artifact}-{asset.version}.jar: SHA-256 mismatch "
                               f"(expected {asset.digest}, got {actual})")

        prefix = f"META-INF/maven/{GROUP}/{asset.artifact}/"
        with zipfile.ZipFile(downloaded) as archive:
            names = set(archive.namelist())
            for entry in (prefix + "pom.xml", prefix + "ivy.xml"):
                if entry not in names:
                    raise RuntimeError(f"{asset.artifact}-{asset.version}.jar has no embedded "
                                       f"{entry}; it predates self-describing releases")
            pom_bytes = archive.read(prefix + "pom.xml")
            ivy_bytes = archive.read(prefix + "ivy.xml")

        for path in (jar, pom, ivy):
            path.parent.mkdir(parents=True, exist_ok=True)
        pom.write_bytes(pom_bytes)
        ivy.write_bytes(ivy_bytes)
        shutil.move(str(downloaded), str(jar))

    return outcome


# ---- entry point -------------------------------------------------------------

def main(arguments: list[str]) -> None:
    repo = os.environ.get("SOUNDNESS_RELEASE_REPO", "propensive/soundness")
    ivy_local = Path(os.environ.get("IVY_LOCAL", str(Path.home() / ".ivy2" / "local")))

    if arguments and arguments[0] == "--staged":
        staged = Path(arguments[1]) if len(arguments) > 1 else Path("out/release/stage.dest")
        if not staged.is_dir():
            fail(f"{staged} does not exist; run `./mill release.stage` first")
        assets = staged_assets(staged)
        log(f"syncing {len(assets)} staged jars from {staged}")
    else:
        wanted = arguments[0] if arguments else None
        if wanted is not None and not VERSION_TAG.match(wanted):
            fail(f"version must be X.Y.Z (got '{wanted}')")
        selected = [release for release in releases(repo)
                    if VERSION_TAG.match(release["tag_name"])
                    and (wanted is None or release["tag_name"] == wanted)]
        if wanted is not None and not selected:
            fail(f"{repo} has no release {wanted}")
        assets = [asset for release in selected for asset in release_assets(release)]
        versions = ", ".join(release["tag_name"] for release in selected)
        log(f"syncing {len(assets)} jars from {repo} ({versions})")

    if not assets:
        fail("nothing to sync")

    outcomes: dict[str, int] = {"fresh": 0, "current": 0, "repaired": 0}
    failures: list[str] = []

    def work(asset: Asset) -> None:
        try:
            outcome = install(asset, ivy_local)
            outcomes[outcome] += 1
            if outcome != "current":
                log(f"{outcome}: {asset.artifact} {asset.version}")
        except Exception as error:  # report every failure, then exit non-zero below
            failures.append(str(error))

    with ThreadPoolExecutor(max_workers=WORKERS) as pool:
        list(pool.map(work, assets))

    log(f"{outcomes['fresh']} installed, {outcomes['repaired']} repaired, "
        f"{outcomes['current']} already current, in {ivy_local}")
    if failures:
        for failure in failures:
            print(f"sync-releases: {failure}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main(sys.argv[1:])

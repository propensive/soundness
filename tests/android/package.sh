#!/usr/bin/env bash
# Packages the linked DEX archive into a signed, installable APK.
# Usage: tests/android/package.sh <directory containing main.dex.jar>
# Requires ANDROID_HOME with `build-tools` and `platforms` installed, and `keytool` (JDK).
set -euo pipefail

DEX_DIR="$(cd "$1" && pwd)"
HERE="$(cd "$(dirname "$0")" && pwd)"
BUILD_TOOLS="$(ls -d "$ANDROID_HOME"/build-tools/* | sort -V | tail -1)"
PLATFORM="$(ls -d "$ANDROID_HOME"/platforms/android-* | sort -V | tail -1)"
WORK="$(mktemp -d)"

unzip -o -q "$DEX_DIR/main.dex.jar" -d "$WORK"

"$BUILD_TOOLS/aapt" package -f -M "$HERE/AndroidManifest.xml" -I "$PLATFORM/android.jar" \
    -F "$WORK/app.unsigned.apk"

(cd "$WORK" && "$BUILD_TOOLS/aapt" add app.unsigned.apk classes*.dex >/dev/null)

"$BUILD_TOOLS/zipalign" -f 4 "$WORK/app.unsigned.apk" "$WORK/app.aligned.apk"

# The standard persistent debug keystore (shared with Android Studio), created on first use:
# reusing one key keeps successive builds mutually updatable on the device.
KEYSTORE="$HOME/.android/debug.keystore"
if [ ! -f "$KEYSTORE" ]; then
  mkdir -p "$HOME/.android"
  keytool -genkeypair -keystore "$KEYSTORE" -storepass android -keypass android \
      -alias androiddebugkey -dname "CN=Android Debug,O=Android,C=US" \
      -keyalg RSA -keysize 2048 -validity 10000 2>/dev/null
fi

"$BUILD_TOOLS/apksigner" sign --ks "$KEYSTORE" --ks-pass pass:android \
    --key-pass pass:android --out "$DEX_DIR/dice.apk" "$WORK/app.aligned.apk"

echo "APK: $DEX_DIR/dice.apk"

# The Android sample: Dice Roller

A Soundness build of [Google's *Dice Roller*
codelab](https://developer.android.com/codelabs/basic-android-kotlin-compose-build-a-dice-roller-app)
(the classic first-steps Android app: a die face and a *Roll* button): a single activity,
written in Scala (`src/dice.scala`), which reaches Android's widgets and Kotlin's
`kotlin.random.Random` through xenophile's Kotlin facades, compiled with anthology's `Scalac`
and linked as `Artifact.Dex` (`builder/build.scala` drives both).

The UI is built programmatically, so the app needs no XML layouts and no resource compilation
beyond its manifest.

## Prerequisites

- A JDK (for the build, and `keytool`).
- The Android SDK command-line tools, with:

  ```sh
  sdkmanager "platform-tools" "platforms;android-34" "build-tools;34.0.0" "emulator" \
      "system-images;android-34;google_apis;arm64-v8a"    # x86_64 on Intel machines
  ```

  and `ANDROID_HOME` pointing at the SDK root.

## Build the DEX

```sh
ANDROID_HOME=~/Library/Android/sdk ./mill android.dex
```

This compiles `src/dice.scala` against the newest installed platform's `android.jar` plus the
facade runtime, then links the classfiles *and their whole runtime classpath* (the Scala and
Kotlin standard libraries included) into `out/android/dex.dest/main.dex.jar` — an archive of
`classes*.dex` files (multidex; API level 26+). The platform itself is never dexed: on the
device, the operating system provides it. To build without a full SDK, point `ANDROID_JAR` at
any platform `android.jar` (Robolectric's `android-all` jar from Maven Central also works).

## Package the APK

There are two ways to turn the dexed code into an installable APK.

### With the Android SDK build tools

```sh
ANDROID_HOME=~/Library/Android/sdk tests/android/package.sh out/android/dex.dest
```

This uses `aapt` (binary manifest), `zipalign` (alignment) and `apksigner` (v2 signature) to
produce `out/android/dex.dest/dice.apk`.

### Purely, with no SDK build tool

```sh
ANDROID_HOME=~/Library/Android/sdk ./mill android.apk
```

This links `Artifact.Apk` directly: anthology dexes the code, encodes the binary manifest itself
(`Axml` — no `aapt2`), assembles the archive with 4-byte-aligned dex entries (zeppelin's
alignment — no `zipalign`), and applies an APK Signature Scheme v2 signature (gastronomy SHA-256
digests + `java.security` RSA — no `apksigner`). The only external requirement is a JDK and the
debug keystore at `~/.android/debug.keystore` (a PKCS#12 store, as `keytool` and Android Studio
create). The result is `out/android/apk.dest/app.apk` — an installable, signed application built
without a single Android SDK tool. The platform `android.jar` (from `ANDROID_HOME`, or `ANDROID_JAR`)
is needed only to *compile* against, never to package.

## Run it in the emulator

```sh
avdmanager create avd --name soundness --package "system-images;android-34;google_apis;arm64-v8a"
emulator -avd soundness &
adb wait-for-device
adb install -r out/android/dex.dest/dice.apk        # or out/android/apk.dest/app.apk
adb shell am start -n dev.soundness.dice/dice.DiceActivity
```

Tap *Roll*.

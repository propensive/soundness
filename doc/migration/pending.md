# Changes since 0.64.0 (pending release)

This file is read by an LLM agent to upgrade code that consumes Soundness libraries. Each
entry states precisely what changed; see `AGENTS.md` for the format. Entries are grouped
by module, most-recently-added last within a module.

Entries for changes merged between the 0.64.0 release and the introduction of this file have
not yet been recorded here.

## ambience

- `ambience.systems.javaSystem` renamed to `javaBaseSystem`. (#PR)
- `ambience.environments.javaEnvironment` renamed to `javaBaseEnvironment`. (#PR)
- `ambience.workingDirectories.javaWorkingDirectory` renamed to `javaBaseWorkingDirectory`. (#PR)
- `ambience.workingDirectories.defaultWorkingDirectory` removed; use `javaBaseWorkingDirectory`,
  which reads the `user.dir` property (the removed given read `java.nio.file.Paths.get("")`,
  which resolves to the same directory). (#PR)
- `ambience.temporaryDirectories.javaTemporaryDirectory` renamed to `javaBaseTemporaryDirectory`. (#PR)

## turbulence

- `turbulence.stdios.virtualMachineStdio` (wrapping `java.io.FileDescriptor.{in,out,err}`) renamed
  to `fileDescriptorStdio`. (#PR)
- `turbulence.stdios.systemStdio` (wrapping `java.lang.System.{in,out,err}`) renamed to
  `javaLangSystemStdio`. (#PR)
- `turbulence.lineSeparation.virtualMachineLineSeparation` renamed to `javaBaseLineSeparation`. (#PR)

## coaxial

- `coaxial.socketBackends.virtualMachineSockets` renamed to `javaBaseSockets`. (#PR)
- `coaxial.socketBackends.native` (Scala Native module) renamed to `scalaNativeSockets`, and now
  re-exported as `soundness.socketBackends.scalaNativeSockets`. (#PR)

## galilei

- `galilei.filesystemBackends.virtualMachineFilesystem` renamed to `javaBaseFilesystem`. (#PR)
- `galilei.filesystemBackends.native` (Scala Native module) renamed to `scalaNativeFilesystem`,
  and now re-exported as `soundness.filesystemBackends.scalaNativeFilesystem`. (#PR)
- The nested objects under `galilei.filesystemOptions` are flattened into givens of the same
  types: `dereferenceSymlinks.enabled` → `dereferenceSymlinks`, `dereferenceSymlinks.disabled` →
  `preserveSymlinks`, `moveAtomically.enabled` → `moveAtomically`, `moveAtomically.disabled` →
  `moveNonAtomically`, `copyAttributes.enabled` → `copyAttributes`, `copyAttributes.disabled` →
  `discardAttributes`, `deleteRecursively.enabled` → `deleteRecursively`,
  `deleteRecursively.disabled` → `deleteOnlyEmpty`, `overwritePreexisting.enabled` →
  `overwritePreexisting`, `overwritePreexisting.disabled` → `failOnPreexisting`,
  `createNonexistentParents.enabled` → `createNonexistentParents`,
  `createNonexistentParents.disabled` → `requireParents`. The objects no longer exist. (#PR)

## telekinesis

- `telekinesis.httpBackends.virtualMachineHttp` renamed to `javaNetHttp`. (#PR)
- `telekinesis.httpBackends.nativeHttp` renamed to `soundnessHttp`. (#PR)

## scintillate

- `scintillate.httpServers.stdlibHttpd` renamed to `jdkHttpserver`; `stdlibPublicHttpd` renamed
  to `jdkHttpserverPublic`. (#PR)
- `scintillate.httpServers.nativeHttpServer` renamed to `soundnessHttpd`; `nativePublicHttpServer`
  renamed to `soundnessHttpdPublic`; both are now also re-exported as
  `soundness.httpServers.{soundnessHttpd, soundnessHttpdPublic}`. (#PR)
- `scintillate.frontends.threadPerConnection` renamed to `threadPerConnectionFrontend`;
  `frontends.reactive` renamed to `reactiveFrontend`. (#PR)
- `scintillate.webserverErrorPages.standardErrorPage` renamed to `styledErrorPage`. (#PR)

## surveillance

- `surveillance.watchers.nativeWatcher` renamed to `javaBaseWatcher`; `surveillance.NativeWatcher`
  (object) renamed to `JavaBaseWatcher`. (#PR)

## gastronomy

- `gastronomy.Provider.JavaStdlib` renamed to `Provider.JavaBase`; `gastronomy.JavaStdlibHashing`
  renamed to `JavaBaseHashing`; `gastronomy.Hashing.javaStdlibHashing` renamed to
  `javaBaseHashing`; `gastronomy.providers.javaStdlibProvider` renamed to `javaBaseProvider`. (#PR)
- Choice package `gastronomy.crypto` renamed to `gastronomy.cryptoPermits` (members
  `permitUnauthenticatedCrypto`, `permitDeprecatedCrypto`, `permitLegacyCrypto`,
  `permitDisallowedCrypto`, `permitCryptoThrough2014`, `permitCryptoThrough2024`,
  `permitCryptoThrough2030`, `permitLegacyTls`, `permitUntrustedCertificates`,
  `permitUncheckedRevocation`, `permitNonCryptographicHashes` unchanged). (#PR)

## enigmatic

- `enigmatic.JavaStdlibCrypto` renamed to `JavaBaseCrypto`; `enigmatic.Crypto.javaStdlibCrypto`
  renamed to `javaBaseCrypto`. (#PR)
- `enigmatic.cloaks.{cloakHeap, cloakOffHeap, cloakVeiledHeap, cloakVeiledOffHeap}` renamed to
  `{heapCloak, offHeapCloak, veiledHeapCloak, veiledOffHeapCloak}`. (#PR)
- Choice packages `enigmatic.blockCipherMode` and `enigmatic.blockCipherPadding` renamed to
  `blockCipherModes` and `blockCipherPaddings` (members `cbc`, `ctr`, `cfb`, `ofb`, `pkcs7`,
  `iso10126` unchanged). (#PR)

## kaleidoscope

- `kaleidoscope.Jur` (type and companion) renamed to `JavaBaseRegex`;
  `kaleidoscope.regexBackends.jur` renamed to `javaBaseRegex`. (#PR)

## diuretic

- Top-level `diuretic.javaNioFilePath` renamed to `javaNioPathRepresentative` and top-level
  `diuretic.javaIoFile` renamed to `javaIoFileRepresentative` (the `Representative of Paths`
  markers; `anticipation.interfaces.paths.{javaNioPath, javaIoFile}` are unchanged). (#PR)

## harlequin

- `punctuation.formattables.{scala, java}` (the `CommonFormattable` givens in `harlequin.md`)
  renamed to `{scalaFormattable, javaFormattable}`. (#PR)

## aviation

- `aviation.chronometries.unix` renamed to `unixChronometry`; `chronometries.atomic` renamed to
  `taiChronometry`. (#PR)
- `aviation.leapModes.exact` renamed to `exactLeapMode`. (#PR)
- `aviation.gapPolicies.pushBackward` renamed to `pushBackwardGapPolicy`; `gapPolicies.rejectGap`
  renamed to `rejectGapPolicy`. (#PR)
- `aviation.timespanFormats.{englishRelative, frenchRelative, germanRelative, spanishRelative}`
  renamed to `{englishRelativeTimespan, frenchRelativeTimespan, germanRelativeTimespan,
  spanishRelativeTimespan}`. (#PR)
- Newly re-exported into `soundness`: `hourFormats.{twelveHourSecondsClock,
  twentyFourHourSecondsClock}`, new family `timeSpecificities.{minutesSpecificity,
  secondsSpecificity}` (library package `aviation.timeFormats.specificity`), and
  `interfaces.instants.aviationInstant` / `interfaces.durations.aviationDuration`. (#PR)

## probably

- `probably.harnesses.threadLocal` renamed to `threadLocalHarness`. (#PR)
- `probably.autopsies.none` renamed to `noAutopsy`; `autopsies.contrastExpectations` renamed to
  `contrastAutopsy`. (#PR)

## iridescence

- Every member of `iridescence.mixing` gains the suffix `Mixing`: `proportional` →
  `proportionalMixing`, `multiply` → `multiplyMixing`, `screen` → `screenMixing`, `darken` →
  `darkenMixing`, `lighten` → `lightenMixing`, `difference` → `differenceMixing`, `exclusion` →
  `exclusionMixing`, `linearDodge` → `linearDodgeMixing`, `linearBurn` → `linearBurnMixing`,
  `hardLight` → `hardLightMixing`, `overlay` → `overlayMixing`, `softLight` → `softLightMixing`,
  `colorDodge` → `colorDodgeMixing`, `colorBurn` → `colorBurnMixing`. (#PR)
- Every member of `iridescence.colorimetry` gains the suffix `Colorimetry`: `incandescentTungsten`,
  `oldDirectSunlightAtNoon`, `oldDaylight`, `iccProfilePcs`, `midMorningDaylight`, `daylight`,
  `srgb`, `adobeRgb`, `northSkyDaylight`, `equalEnergy`, `daylightFluorescentF1`,
  `coolFluorescent`, `whiteFluorescent`, `warmWhiteFluorescent`, `daylightFluorescentF5`,
  `liteWhiteFluorescent`, `daylightFluorescentF7`, `d65Simulator`, `sylvaniaF40`, `d50Simulator`,
  `coolWhiteFluorescent`, `philipsTl85`, `ultralume50`, `philipsTl84`, `ultralume40`,
  `philipsTl83`, `ultralume30` become `incandescentTungstenColorimetry`, …,
  `ultralume30Colorimetry`. (#PR)

## gossamer

- `gossamer.collations.unicode` renamed to `unicodeCollation`; `collations.codepoints` renamed to
  `codepointCollation`. (#PR)

## rudiments

- Top-level `rudiments.populatedEquality2` renamed to `populatedEqualityReversed`. (#PR)

## zephyrine

- `zephyrine.lineation.linefeedChars` renamed to `linefeedChar`. The `lineation` family
  (`linefeedChar`, `carriageReturnChar`, `linefeedByte`, `carriageReturnByte`) is now re-exported
  as `soundness.lineation`. (#PR)

## hypotenuse

- The nested objects under `hypotenuse.arithmeticOptions` are flattened into givens of the same
  types: `division.checked` → `checkedDivision`, `division.unchecked` → `uncheckedDivision`,
  `overflow.checked` → `checkedOverflow`, `overflow.unchecked` → `uncheckedOverflow`,
  `rationalDivision.q64` → `q64RationalDivision`, `rationalDivision.q32` → `q32RationalDivision`.
  The objects no longer exist. (#PR)

## honeycomb

- `honeycomb.formatting.flatHtmlFormatting` renamed to `compactHtmlFormatting`. (#PR)
- `honeycomb.recoveries.permissiveRecovery` is now re-exported as `soundness.recoveries`. (#PR)

## cataclysm

- `cataclysm.formatting.standardCssFormatting` renamed to `indentedCssFormatting`. (#PR)

## dendrology

- `dendrology.treeStyles.defaultTreeStyle` renamed to `squareTreeStyle`;
  `dagStyles.defaultDagStyle` renamed to `boxDrawingDagStyle`; `laneDagStyles.defaultLaneDagStyle`
  renamed to `boxDrawingLaneDagStyle`. (#PR)

## escritoire

- `escritoire.tableStyles.defaultTableStyle` renamed to `thickTableStyle`. (#PR)

## eucalyptus

- `eucalyptus.logFormats.standardLogFormat` renamed to `timestampedLogFormat`;
  `logFormats.ansiStandardLogFormat` renamed to `ansiTimestampedLogFormat`. (#PR)
- `eucalyptus.logFormats.lightweightLogFormat` removed; it was identical to
  `untimestampedLogFormat`. (#PR)

## legerdemain

- `legerdemain.formulations.defaultFormulation` renamed to `postFormulation`. (#PR)

## octogenarian

- `octogenarian.gitCommands.environmentDefaultGitCommand` renamed to `searchpathGitCommand`. (#PR)

## caesura

- `caesura.optics.{cellLens, rowOptical, rowEach, rowFilter}` renamed to `{dsvCellLens,
  dsvRowOptical, dsvRowEachOptical, dsvRowFilterOptical}`. (#PR)
- `caesura.dynamicDsvAccess.enabled` (object member) replaced by
  `caesura.dynamicAccess.dynamicDsv` (choice package); the object `dynamicDsvAccess` no longer
  exists. (#PR)

## capricious

- `capricious.randomization.sizes.{uniformUpto10, uniformUpto100, uniformUpto1000,
  uniformUpto10000, uniformUpto100000}` renamed to `{uniformSizeUpto10, uniformSizeUpto100,
  uniformSizeUpto1000, uniformSizeUpto10000, uniformSizeUpto100000}`. (#PR)
- `capricious.randomization.text.bigListOfNaughtyStrings` renamed to `naughtyStringsText`, and
  now re-exported as `soundness.randomization.text.naughtyStringsText`. (#PR)

## exoskeleton

- `exoskeleton.executives.completions` renamed to `completionsExecutive`. (#PR)

## caduceus

- `caduceus.couriers.resend` renamed to `resendCourier`. (#PR)

## ultimatum

- `ultimatum.inlineAnchoring.{bottomDocked, topAnchored, topAfterResize, fullscreen, inline}`
  renamed to `{bottomDockedAnchoring, topAnchoring, topAfterResizeAnchoring, fullscreenAnchoring,
  flowAnchoring}`; enum case `ultimatum.InlineAnchoring.Inline` renamed to `Flow`. (#PR)
- `ultimatum.inlineGrowth.{scrollIntoScrollback, clampToScreen}` renamed to `{scrollbackGrowth,
  clampedGrowth}`. (#PR)
- `ultimatum.inlineShrink.{redockBottom, keepTop}` renamed to `{redockBottomShrink,
  keepTopShrink}`. (#PR)

## jacinta

- `jacinta.dynamicJsonAccess.enabled` (object member) replaced by `jacinta.dynamicAccess.dynamicJson`
  (choice package); the object `dynamicJsonAccess` no longer exists. (#PR)
- `jacinta.jsonConversion.encodable` (object member) replaced by
  `jacinta.conversions.encodableToJson` (choice package); the object `jsonConversion` no longer
  exists. (#PR)

## xylophone

- `xylophone.dynamicXmlAccess.enabled` replaced by `xylophone.dynamicAccess.dynamicXml`; the
  object `dynamicXmlAccess` no longer exists. (#PR)

## ypsiloid

- `ypsiloid.dynamicYamlAccess.enabled` replaced by `ypsiloid.dynamicAccess.dynamicYaml`; the
  object `dynamicYamlAccess` no longer exists. (#PR)
- `ypsiloid.yamlConversion.encodable` replaced by `ypsiloid.conversions.encodableToYaml`; the
  object `yamlConversion` no longer exists. (#PR)

## stratiform

- `stratiform.dynamicTelAccess.enabled` replaced by `stratiform.dynamicAccess.dynamicTel`; the
  object `dynamicTelAccess` no longer exists. (#PR)
- `stratiform.telConversion.encodable` replaced by `stratiform.conversions.encodableToTel`; the
  object `telConversion` no longer exists. (#PR)

## breviloquence

- `breviloquence.dynamicCborAccess.enabled` replaced by `breviloquence.dynamicAccess.dynamicCbor`;
  the object `dynamicCborAccess` no longer exists. (#PR)
- `breviloquence.cborConversion.encodable` replaced by `breviloquence.conversions.encodableToCbor`;
  the object `cborConversion` no longer exists. (#PR)

## locomotion

- `locomotion.protobufConversion.encodable` replaced by
  `locomotion.conversions.encodableToProtobuf`; the object `protobufConversion` no longer
  exists. (#PR)

## superlunary

- `superlunary.embeddings.automatic` (object member) replaced by
  `superlunary.embeddings.automaticEmbedding` (choice package); `embeddings` is no longer an
  object. (#PR)

## escapade

- `escapade.writables.{out, err}` renamed to `{outTeletypeWritable, errTeletypeWritable}`. (#PR)
- `escapade.teletypeables.graphical` renamed to `graphicalTeletype`. (#PR)

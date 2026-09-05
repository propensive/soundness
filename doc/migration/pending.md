# Changes since 0.64.0 (pending release)

This file is read by an LLM agent to upgrade code that consumes Soundness libraries. Each
entry states precisely what changed; see `AGENTS.md` for the format. Entries are grouped
by module, most-recently-added last within a module.

Entries for changes merged between the 0.64.0 release and the introduction of this file have
not yet been recorded here.

## ambience

- `ambience.systems.javaSystem` renamed to `javaBaseSystem`. (#1939)
- `ambience.environments.javaEnvironment` renamed to `javaBaseEnvironment`. (#1939)
- `ambience.workingDirectories.javaWorkingDirectory` renamed to `javaBaseWorkingDirectory`. (#1939)
- `ambience.workingDirectories.defaultWorkingDirectory` removed; use `javaBaseWorkingDirectory`,
  which reads the `user.dir` property (the removed given read `java.nio.file.Paths.get("")`,
  which resolves to the same directory). (#1939)
- `ambience.temporaryDirectories.javaTemporaryDirectory` renamed to `javaBaseTemporaryDirectory`. (#1939)

## turbulence

- `turbulence.stdios.virtualMachineStdio` (wrapping `java.io.FileDescriptor.{in,out,err}`) renamed
  to `fileDescriptorStdio`. (#1939)
- `turbulence.stdios.systemStdio` (wrapping `java.lang.System.{in,out,err}`) renamed to
  `javaLangSystemStdio`. (#1939)
- `turbulence.lineSeparation.virtualMachineLineSeparation` renamed to `javaBaseLineSeparation`. (#1939)

## coaxial

- `coaxial.socketBackends.virtualMachineSockets` renamed to `javaBaseSockets`. (#1939)
- `coaxial.socketBackends.native` (Scala Native module) renamed to `scalaNativeSockets`, and now
  re-exported as `soundness.socketBackends.scalaNativeSockets`. (#1939)

## galilei

- `galilei.filesystemBackends.virtualMachineFilesystem` renamed to `javaBaseFilesystem`. (#1939)
- `galilei.filesystemBackends.native` (Scala Native module) renamed to `scalaNativeFilesystem`,
  and now re-exported as `soundness.filesystemBackends.scalaNativeFilesystem`. (#1939)
- The nested objects under `galilei.filesystemOptions` are flattened into givens of the same
  types: `dereferenceSymlinks.enabled` → `dereferenceSymlinks`, `dereferenceSymlinks.disabled` →
  `preserveSymlinks`, `moveAtomically.enabled` → `moveAtomically`, `moveAtomically.disabled` →
  `moveNonAtomically`, `copyAttributes.enabled` → `copyAttributes`, `copyAttributes.disabled` →
  `discardAttributes`, `deleteRecursively.enabled` → `deleteRecursively`,
  `deleteRecursively.disabled` → `deleteOnlyEmpty`, `overwritePreexisting.enabled` →
  `overwritePreexisting`, `overwritePreexisting.disabled` → `failOnPreexisting`,
  `createNonexistentParents.enabled` → `createNonexistentParents`,
  `createNonexistentParents.disabled` → `requireParents`. The objects no longer exist. (#1939)

## telekinesis

- `telekinesis.httpBackends.virtualMachineHttp` renamed to `javaNetHttp`. (#1939)
- `telekinesis.httpBackends.nativeHttp` renamed to `soundnessHttp`. (#1939)

## scintillate

- `scintillate.httpServers.stdlibHttpd` renamed to `jdkHttpserver`; `stdlibPublicHttpd` renamed
  to `jdkHttpserverPublic`. (#1939)
- `scintillate.httpServers.nativeHttpServer` renamed to `soundnessHttpd`; `nativePublicHttpServer`
  renamed to `soundnessHttpdPublic`; both are now also re-exported as
  `soundness.httpServers.{soundnessHttpd, soundnessHttpdPublic}`. (#1939)
- `scintillate.frontends.threadPerConnection` renamed to `threadPerConnectionFrontend`;
  `frontends.reactive` renamed to `reactiveFrontend`. (#1939)
- `scintillate.webserverErrorPages.standardErrorPage` renamed to `styledErrorPage`. (#1939)

## surveillance

- `surveillance.watchers.nativeWatcher` renamed to `javaBaseWatcher`; `surveillance.NativeWatcher`
  (object) renamed to `JavaBaseWatcher`. (#1939)

## gastronomy

- `gastronomy.Provider.JavaStdlib` renamed to `Provider.JavaBase`; `gastronomy.JavaStdlibHashing`
  renamed to `JavaBaseHashing`; `gastronomy.Hashing.javaStdlibHashing` renamed to
  `javaBaseHashing`; `gastronomy.providers.javaStdlibProvider` renamed to `javaBaseProvider`. (#1939)
- Choice package `gastronomy.crypto` renamed to `gastronomy.cryptoPermits` (members
  `permitUnauthenticatedCrypto`, `permitDeprecatedCrypto`, `permitLegacyCrypto`,
  `permitDisallowedCrypto`, `permitCryptoThrough2014`, `permitCryptoThrough2024`,
  `permitCryptoThrough2030`, `permitLegacyTls`, `permitUntrustedCertificates`,
  `permitUncheckedRevocation`, `permitNonCryptographicHashes` unchanged). (#1939)

## enigmatic

- `enigmatic.JavaStdlibCrypto` renamed to `JavaBaseCrypto`; `enigmatic.Crypto.javaStdlibCrypto`
  renamed to `javaBaseCrypto`. (#1939)
- `enigmatic.cloaks.{cloakHeap, cloakOffHeap, cloakVeiledHeap, cloakVeiledOffHeap}` renamed to
  `{heapCloak, offHeapCloak, veiledHeapCloak, veiledOffHeapCloak}`. (#1939)
- Choice packages `enigmatic.blockCipherMode` and `enigmatic.blockCipherPadding` renamed to
  `blockCipherModes` and `blockCipherPaddings` (members `cbc`, `ctr`, `cfb`, `ofb`, `pkcs7`,
  `iso10126` unchanged). (#1939)

## kaleidoscope

- `kaleidoscope.Jur` (type and companion) renamed to `JavaBaseRegex`;
  `kaleidoscope.regexBackends.jur` renamed to `javaBaseRegex`. (#1939)

## diuretic

- Top-level `diuretic.javaNioFilePath` renamed to `javaNioPathRepresentative` and top-level
  `diuretic.javaIoFile` renamed to `javaIoFileRepresentative` (the `Representative of Paths`
  markers; `anticipation.pathInterfaces.{javaNioPath, javaIoFile}` are unchanged). (#1939)

## harlequin

- `punctuation.formattables.{scala, java}` (the `CommonFormattable` givens in `harlequin.md`)
  renamed to `{scalaFormattable, javaFormattable}`. (#1939)

## aviation

- `aviation.chronometries.unix` renamed to `unixChronometry`; `chronometries.atomic` renamed to
  `taiChronometry`. (#1939)
- `aviation.leapModes.exact` renamed to `exactLeapMode`. (#1939)
- `aviation.gapPolicies.pushBackward` renamed to `pushBackwardGapPolicy`; `gapPolicies.rejectGap`
  renamed to `rejectGapPolicy`. (#1939)
- `aviation.timespanFormats.{englishRelative, frenchRelative, germanRelative, spanishRelative}`
  renamed to `{englishRelativeTimespan, frenchRelativeTimespan, germanRelativeTimespan,
  spanishRelativeTimespan}`. (#1939)
- Newly re-exported into `soundness`: `hourFormats.{twelveHourSecondsClock,
  twentyFourHourSecondsClock}`, new family `timeSpecificities.{minutesSpecificity,
  secondsSpecificity}` (library package `aviation.timeSpecificities`), and
  `instantInterfaces.aviationInstant` / `durationInterfaces.aviationDuration`. (#1939)

## probably

- `probably.harnesses.threadLocal` renamed to `threadLocalHarness`. (#1939)
- `probably.autopsies.none` renamed to `noAutopsy`; `autopsies.contrastExpectations` renamed to
  `contrastAutopsy`. (#1939)

## iridescence

- Every member of `iridescence.mixing` gains the suffix `Mixing`: `proportional` →
  `proportionalMixing`, `multiply` → `multiplyMixing`, `screen` → `screenMixing`, `darken` →
  `darkenMixing`, `lighten` → `lightenMixing`, `difference` → `differenceMixing`, `exclusion` →
  `exclusionMixing`, `linearDodge` → `linearDodgeMixing`, `linearBurn` → `linearBurnMixing`,
  `hardLight` → `hardLightMixing`, `overlay` → `overlayMixing`, `softLight` → `softLightMixing`,
  `colorDodge` → `colorDodgeMixing`, `colorBurn` → `colorBurnMixing`. (#1939)
- Every member of `iridescence.colorimetry` gains the suffix `Colorimetry`: `incandescentTungsten`,
  `oldDirectSunlightAtNoon`, `oldDaylight`, `iccProfilePcs`, `midMorningDaylight`, `daylight`,
  `srgb`, `adobeRgb`, `northSkyDaylight`, `equalEnergy`, `daylightFluorescentF1`,
  `coolFluorescent`, `whiteFluorescent`, `warmWhiteFluorescent`, `daylightFluorescentF5`,
  `liteWhiteFluorescent`, `daylightFluorescentF7`, `d65Simulator`, `sylvaniaF40`, `d50Simulator`,
  `coolWhiteFluorescent`, `philipsTl85`, `ultralume50`, `philipsTl84`, `ultralume40`,
  `philipsTl83`, `ultralume30` become `incandescentTungstenColorimetry`, …,
  `ultralume30Colorimetry`. (#1939)

## gossamer

- `gossamer.collations.unicode` renamed to `unicodeCollation`; `collations.codepoints` renamed to
  `codepointCollation`. (#1939)

## rudiments

- Top-level `rudiments.populatedEquality2` renamed to `populatedEqualityReversed`. (#1939)

## zephyrine

- `zephyrine.lineation.linefeedChars` renamed to `linefeedChar`. The `lineation` family
  (`linefeedChar`, `carriageReturnChar`, `linefeedByte`, `carriageReturnByte`) is now re-exported
  as `soundness.lineation`. (#1939)

## hypotenuse

- The nested objects under `hypotenuse.arithmeticOptions` are flattened into givens of the same
  types: `division.checked` → `checkedDivision`, `division.unchecked` → `uncheckedDivision`,
  `overflow.checked` → `checkedOverflow`, `overflow.unchecked` → `uncheckedOverflow`,
  `rationalDivision.q64` → `q64RationalDivision`, `rationalDivision.q32` → `q32RationalDivision`.
  The objects no longer exist. (#1939)

## honeycomb

- `honeycomb.formatting.flatHtmlFormatting` renamed to `compactHtmlFormatting`. (#1939)
- `honeycomb.recoveries.permissiveRecovery` is now re-exported as `soundness.recoveries`. (#1939)

## cataclysm

- `cataclysm.formatting.standardCssFormatting` renamed to `indentedCssFormatting`. (#1939)

## dendrology

- `dendrology.treeStyles.defaultTreeStyle` renamed to `squareTreeStyle`;
  `dagStyles.defaultDagStyle` renamed to `boxDrawingDagStyle`; `laneDagStyles.defaultLaneDagStyle`
  renamed to `boxDrawingLaneDagStyle`. (#1939)

## escritoire

- `escritoire.tableStyles.defaultTableStyle` renamed to `thickTableStyle`. (#1939)

## eucalyptus

- `eucalyptus.logFormats.standardLogFormat` renamed to `timestampedLogFormat`;
  `logFormats.ansiStandardLogFormat` renamed to `ansiTimestampedLogFormat`. (#1939)
- `eucalyptus.logFormats.lightweightLogFormat` removed; it was identical to
  `untimestampedLogFormat`. (#1939)

## legerdemain

- `legerdemain.formulations.defaultFormulation` renamed to `postFormulation`. (#1939)

## octogenarian

- `octogenarian.gitCommands.environmentDefaultGitCommand` renamed to `searchpathGitCommand`. (#1939)

## caesura

- `caesura.optics.{cellLens, rowOptical, rowEach, rowFilter}` renamed to `{dsvCellLens,
  dsvRowOptical, dsvRowEachOptical, dsvRowFilterOptical}`. (#1939)
- `caesura.dynamicDsvAccess.enabled` (object member) replaced by
  `caesura.dynamicAccess.dynamicDsv` (choice package); the object `dynamicDsvAccess` no longer
  exists. (#1939)

## capricious

- `capricious.randomSizes.{uniformUpto10, uniformUpto100, uniformUpto1000,
  uniformUpto10000, uniformUpto100000}` renamed to `{uniformSizeUpto10, uniformSizeUpto100,
  uniformSizeUpto1000, uniformSizeUpto10000, uniformSizeUpto100000}`. (#1939)
- `capricious.randomTexts.bigListOfNaughtyStrings` renamed to `naughtyStringsText`, and
  now re-exported as `soundness.randomTexts.naughtyStringsText`. (#1939)

## exoskeleton

- `exoskeleton.executives.completions` renamed to `completionsExecutive`. (#1939)

## caduceus

- `caduceus.couriers.resend` renamed to `resendCourier`. (#1939)

## ultimatum

- `ultimatum.inlineAnchoring.{bottomDocked, topAnchored, topAfterResize, fullscreen, inline}`
  renamed to `{bottomDockedAnchoring, topAnchoring, topAfterResizeAnchoring, fullscreenAnchoring,
  flowAnchoring}`; enum case `ultimatum.InlineAnchoring.Inline` renamed to `Flow`. (#1939)
- `ultimatum.inlineGrowth.{scrollIntoScrollback, clampToScreen}` renamed to `{scrollbackGrowth,
  clampedGrowth}`. (#1939)
- `ultimatum.inlineShrink.{redockBottom, keepTop}` renamed to `{redockBottomShrink,
  keepTopShrink}`. (#1939)

## jacinta

- `jacinta.dynamicJsonAccess.enabled` (object member) replaced by `jacinta.dynamicAccess.dynamicJson`
  (choice package); the object `dynamicJsonAccess` no longer exists. (#1939)
- `jacinta.jsonConversion.encodable` (object member) replaced by
  `jacinta.conversions.encodableToJson` (choice package); the object `jsonConversion` no longer
  exists. (#1939)

## xylophone

- `xylophone.dynamicXmlAccess.enabled` replaced by `xylophone.dynamicAccess.dynamicXml`; the
  object `dynamicXmlAccess` no longer exists. (#1939)

## ypsiloid

- `ypsiloid.dynamicYamlAccess.enabled` replaced by `ypsiloid.dynamicAccess.dynamicYaml`; the
  object `dynamicYamlAccess` no longer exists. (#1939)
- `ypsiloid.yamlConversion.encodable` replaced by `ypsiloid.conversions.encodableToYaml`; the
  object `yamlConversion` no longer exists. (#1939)

## stratiform

- `stratiform.dynamicTelAccess.enabled` replaced by `stratiform.dynamicAccess.dynamicTel`; the
  object `dynamicTelAccess` no longer exists. (#1939)
- `stratiform.telConversion.encodable` replaced by `stratiform.conversions.encodableToTel`; the
  object `telConversion` no longer exists. (#1939)

## breviloquence

- `breviloquence.dynamicCborAccess.enabled` replaced by `breviloquence.dynamicAccess.dynamicCbor`;
  the object `dynamicCborAccess` no longer exists. (#1939)
- `breviloquence.cborConversion.encodable` replaced by `breviloquence.conversions.encodableToCbor`;
  the object `cborConversion` no longer exists. (#1939)

## locomotion

- `locomotion.protobufConversion.encodable` replaced by
  `locomotion.conversions.encodableToProtobuf`; the object `protobufConversion` no longer
  exists. (#1939)

## superlunary

- `superlunary.embeddings.automatic` (object member) replaced by
  `superlunary.embeddings.automaticEmbedding` (choice package); `embeddings` is no longer an
  object. (#1939)

## escapade

- `escapade.writables.{out, err}` renamed to `{outTeletypeWritable, errTeletypeWritable}`. (#1939)
- `escapade.teletypeables.graphical` renamed to `graphicalTeletype`. (#1939)

## anticipation

- The nested interface-selector families are flattened to one level: `interfaces.paths` →
  `pathInterfaces` (members `pathOnLinux`, `pathOnWindows`, `pathOnMacOs`, `pathOnLocal`,
  `pathOnPosix` from galilei, `textPath` from serpentine, `javaNioPath`, `javaIoFile` from
  diuretic), `interfaces.instants` → `instantInterfaces` (`javaTimeInstant`, `javaLongInstant`,
  `javaUtilDate`, `aviationInstant`), `interfaces.durations` → `durationInterfaces`
  (`javaLongDuration`, `aviationDuration`), `interfaces.urls` → `urlInterfaces` (`javaNetUrl`).
  The `anticipation.interfaces` package no longer exists. `textPath` is now also re-exported as
  `soundness.pathInterfaces.textPath`. (#1943)

## aviation

- The nested families under `aviation.dateFormats`, `aviation.timeFormats` and
  `aviation.calendars` are now top-level packages of `aviation`, named as the umbrella already
  named them: `dateFormats.endianness` → `dateEndianness`, `dateFormats.numerics` →
  `dateNumerics`, `dateFormats.separators` → `dateSeparators`, `dateFormats.years` →
  `yearFormats`, `dateFormats.weekdays` → `weekdays`, `dateFormats.months` → `monthFormats`,
  `timeFormats.meridiems` → `timeMeridiems`, `timeFormats.hours` → `hourFormats`,
  `timeFormats.specificity` → `timeSpecificities`, `timeFormats.numerics` → `timeNumerics`,
  `timeFormats.separators` → `timeSeparators`, `calendars.nonexistentLeapDays` →
  `nonexistentLeapDays`. Members are unchanged. In the umbrella, `soundness.endianness` is
  renamed to `soundness.dateEndianness` and `soundness.meridiems` to `soundness.timeMeridiems`.
  (#1943)

## capricious

- `capricious.randomization.sizes` renamed to `capricious.randomSizes` and
  `capricious.randomization.text` renamed to `capricious.randomTexts`, both now top-level
  packages (members unchanged); the umbrella paths change accordingly. (#1943)

## honeycomb

- `honeycomb.doms.html` (`whatwg`, `html4Transitional`) renamed to `honeycomb.htmlDoms`; the
  `doms` package no longer exists. (#1943)

## frontier

- `frontier.context.explainMissingContext` and `soundness.explainMissingContext` no longer
  succeed as an implicit candidate when the search resolves without them; the candidate now
  always fails (its diagnostic is used only if the whole search fails), leaving the compiler to
  select the instance itself. Effect: an implicit search made while type parameters are still
  undetermined (e.g. `join`'s `element`/`textual`) is no longer resolved with those parameters
  instantiated to `Any`. Inferred types of code that already compiled are unchanged. (#1944)

# ChangeLog for yaml

## 0.11.8.0

* Export `Parse` and `StringStyle` [#204](https://github.com/snoyberg/yaml/pull/204)

## 0.11.7.0

* Support `aeson` 2 [#202](https://github.com/snoyberg/yaml/pull/202)

## 0.11.6.0

* `yaml2json`: add `--help` and `--version` options [#197](https://github.com/snoyberg/yaml/pull/197)
* `json2yaml`: add `--help` and `--version` options [#198](https://github.com/snoyberg/yaml/pull/198)
* Add the `-o` options to both `yaml2json` and `json2yaml` [#200](https://github.com/snoyberg/yaml/pull/200)

## 0.11.5.0

* New functions capable of parsing YAML streams containing multiple documents into a list of results:
  * `decodeAllEither'`
  * `decodeAllFileEither`
  * `decodeAllFileWithWarnings`
  * `decodeAllThrow`
  * `decodeAllFileThrow`

## 0.11.4.0

* add `ToYaml` instance for `String` [#186](https://github.com/snoyberg/yaml/pull/186)

## 0.11.3.0

* Don't wrap up async exceptions [#185](https://github.com/snoyberg/yaml/issues/185)

## 0.11.2.0

* Reduces some of the code duplication between the `encode` and `encodePretty` functions
* The output of `encodePretty` has been improved:
    - Multiline strings now use `Literal` style instead of `SingleQuoted`
    - Special keys are now quoted in mappings [#179](https://github.com/snoyberg/yaml/issues/179)
* Support for complex keys in mappings: [#182](https://github.com/snoyberg/yaml/issues/182)
    - Adds `complexMapping` function to `Data.Yaml.Builder`
    - Decode functions now return a `NonStringKey` error when attempting to decode a mapping with a complex key as it is not possible to decode these to an Aeson `Value`
* Adds missing `ToYaml` instances

## 0.11.1.2

* Compiles with GHC 8.8.1 (`MonadFail` split)

## 0.11.1.1

* Use the appropriate `Scientific` rendering function to avoid a memory overflow when rendering. The previously used function from `aeson` would not use scientific notation, and could use large amounts of memory for values such as `1e9999999999999`.

## 0.11.1.0

* Better error messages in the `Data.Yaml.Config` module [#168](https://github.com/snoyberg/yaml/issues/168)
* Add `LoadSettingsException` exception and remove error printing from `loadYamlSettings` [#172](https://github.com/snoyberg/yaml/pull/172)

## 0.11.0.0

* Split out the `libyaml` and `Text.Libyaml` code into its own package. [#145](https://github.com/snoyberg/yaml/issues/145)

## 0.10.4.0

* Add `decodeMarked` and `decodeFileMarked` functions to `Text.Libyaml`, and
  extend native bindings to extract mark information. [#157](https://github.com/snoyberg/yaml/issues/157)

## 0.10.3.0

* Add support for anchors and aliases to Data.Yaml.Builder [#155](https://github.com/snoyberg/yaml/pull/155)
* Fix test suite for 32 bit machines [#158](https://github.com/snoyberg/yaml/issues/158)

## 0.10.2.0

* Add `EncodeOptions` and `FormatOptions` to control the style of the encoded YAML. [#153](https://github.com/snoyberg/yaml/pull/153)
* Default to using literal style for multiline strings [#152](https://github.com/snoyberg/yaml/issues/152)

## 0.10.1.1

* Correctly declare libyaml dependency on system-libyaml flag [#151](https://github.com/snoyberg/yaml/pull/151)

## 0.10.1

* Avoid incurring a `semigroups` dependency on recent GHCs.
* Fix a space leak that was introduced with `0.10.0` [#147](https://github.com/snoyberg/yaml/issues/147)

## 0.10.0

* Add `decodeFileWithWarnings` which returns warnings for duplicate fields

## 0.9.0

* Expose style and tags on mappings and sequences in Text.Libyaml [#141](https://github.com/snoyberg/yaml/pull/141)

## 0.8.32

* Escape keys as necessary [#137](https://github.com/snoyberg/yaml/issues/137)
* Support hexadecimal and octal number values [#135](https://github.com/snoyberg/yaml/issues/135)
* More resilient `isNumeric` (should reduce cases of unneeded quoting)
* hpackify
* src subdir

## 0.8.31.1

* Add a workaround for a cabal bug [haskell-infra/hackage-trustees#165](https://github.com/haskell-infra/hackage-trustees/issues/165)

## 0.8.31

* Add `decodeThrow` and `decodeFileThrow` convenience functions.
* Upgrade libyaml versions
* Deprecate `decode` and `decodeEither`

## 0.8.30

* Removed `AppSettings` mentioned in `loadYamlSettings` error message.

## 0.8.29

* Deprecated `decodeFile` [#129](https://github.com/snoyberg/yaml/issues/129)
* Turn off executables by default [#103](https://github.com/snoyberg/yaml/issues/103)

## 0.8.28

* Add `Data.Yaml.TH.yamlQQ`

## 0.8.27

* Support conduit 1.3

## 0.8.26

* Add `Semigroup` instance [#123](https://github.com/snoyberg/yaml/pull/123)

## 0.8.25.2

* Use `throwM` instead of `monadThrow`

## 0.8.25.1

* Drop aeson-qq dep (incompatible with Stackage Nightly)

## 0.8.25

* Tweaks to the executable `yaml2json` [#119](https://github.com/snoyberg/yaml/pull/119): 
    - Add command-line option `-h` and `--help` to show help message
    - Error messages are now written to `stderr` instead of `stdout`

## 0.8.24

* New encodePretty option `setConfDropNull` to drop null values from objects [#116](https://github.com/snoyberg/yaml/issues/116)

## 0.8.23.3

* Avoid over-escaping `*` [#113](https://github.com/snoyberg/yaml/issues/113)

## 0.8.23.2

* Update libyaml [#110](https://github.com/snoyberg/yaml/issues/110)

## 0.8.23.1

* Update CPP `MIN_VERSION_*` checks [#109](https://github.com/snoyberg/yaml/pull/109)

## 0.8.23

* Re-export the with helpers from aeson

## 0.8.22.1

* Make numeric string detection slightly smarter so, e.g., `.` does
  not get quoted

## 0.8.22

* Update to libyaml hosted on Github [#105](https://github.com/snoyberg/yaml/issues/105)

## 0.8.21.2

* Fix wrong file not found exception in `Data.Yaml.Include` with pre-1.2.3.0 `directory` [#104](https://github.com/snoyberg/yaml/pull/104)

## 0.8.21.1

* Add missing test files [#102](https://github.com/snoyberg/yaml/pull/102)

## 0.8.21

* Decode empty inputs as Null [#101](https://github.com/snoyberg/yaml/pull/101)

## 0.8.20

* Upgrade to libyaml 0.1.7

## 0.8.19.0

* Add `Data.Yaml.TH` module

## 0.8.18.7

* Add `O_TRUNC` when opening files

## 0.8.18.6

* s/fdopen/_fdopen on Windows [#96](https://github.com/snoyberg/yaml/issues/96)

## 0.8.18.5

* Properly fix previous bug (fixes #94)

## 0.8.18.4

* Remove file with non-ASCII name due to Stack/cabal-install/Hackage
  restrictions (see [#92](https://github.com/snoyberg/yaml/issues/92))

## 0.8.18.2

* Handle non-ASCII filenames correctly on Windows [#91](https://github.com/snoyberg/yaml/pull/91)

## 0.8.18.1

* Improve prettyPrintParseException when context is empty [#89](https://github.com/snoyberg/yaml/pull/89)

## 0.8.18

* Switched yaml decode function for config file readers in `Data.Yaml.Config` to
  the one from `Data.Yaml.Include` that supports `!include` syntax.

## 0.8.17.2

* Fix pretty-printing order of UnexpectedEvent's fields (fixes [#84](https://github.com/snoyberg/yaml/issues/84)) [#85](https://github.com/snoyberg/yaml/pull/85)

## 0.8.17.1

* Avoid bug in Cabal [#83](https://github.com/snoyberg/yaml/pull/83)

## 0.8.17

* `loadYamlSettingsArgs`

## 0.8.16.1

* Slight doc improvement

## 0.8.16

Add env variable parsing. `loadYamlSettings` can read config values from the environment with Yaml that specifies an env var.
The syntax is

`var: _env:ENV_VAR:default`

## 0.8.15.3

* Give a warning when compiling with GHCJS

## 0.8.15.2

* Canonicalise Monad instances [#76](https://github.com/snoyberg/yaml/pull/76)

## 0.8.15.1

* Compile with aeson below 0.7 [#70](https://github.com/snoyberg/yaml/pull/70)

## 0.8.15

* Parse `Scientific` directly, avoiding loss in precision. [#68](https://github.com/snoyberg/yaml/pull/68)

## 0.8.14

* Pretty print improvements for exceptions [#67](https://github.com/snoyberg/yaml/pull/67)

## 0.8.13

* Pretty module [#66](https://github.com/snoyberg/yaml/pull/66)

## 0.8.12

* Proper handling of `String "+123"` [#64](https://github.com/snoyberg/yaml/issues/64)

## 0.8.11

* Function to print prettier parse exceptions [#59](https://github.com/snoyberg/yaml/pull/59)

## 0.8.10

Add the Data.Yaml.Include module

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

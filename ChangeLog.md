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

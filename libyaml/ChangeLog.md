# Changelog for libyaml

## 0.1.2

* Upgrade `libyaml` to 0.2.2

## 0.1.1.1

* Work around GHC 8.6.5 issue on Windows preventing truncated files.  [#178](https://github.com/snoyberg/yaml/issues/178)

## 0.1.1.0

* Add options to `FormatOptions` to govern when tags rendered explicitly and when they are left implicit. [#165](https://github.com/snoyberg/yaml/issues/165)

## 0.1.0.0

* Split `libyaml` into a separate package from `yaml`. This split occurred at
  `yaml` version 0.10.4.0.

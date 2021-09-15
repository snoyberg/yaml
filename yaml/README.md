## yaml

[![GitHub build and test status](https://github.com/snoyberg/yaml/actions/workflows/tests.yml/badge.svg?branch=master)](https://github.com/snoyberg/yaml/actions/workflows/tests.yml)
[![Appveyor build status](https://ci.appveyor.com/api/projects/status/hqy2jketp8m502so/branch/master?svg=true)](https://ci.appveyor.com/project/snoyberg/yaml/branch/master)

Provides support for parsing and emitting Yaml documents.

`Data.Yaml` provides a high-level interface based around the JSON datatypes provided by the `aeson` package. It uses `Text.Libyaml` from `libyaml` in its implementation of the low-level yaml encoder/decoder.

### Examples

Usage examples can be found in the `Data.Yaml` documentation or in the [examples](https://github.com/snoyberg/yaml/tree/master/yaml/examples) directory.

### Additional `yaml` modules

* `Data.Yaml.Include` supports adding `!include` directives to your YAML files.
* `Data.Yaml.Builder` and `Data.Yaml.Parser` allow more fine-grained control of parsing an rendering, as opposed to just using the aeson typeclass and datatype system for parsing and rendering.
* `Data.Yaml.Aeson` is currently a re-export of `Data.Yaml` to explicitly choose to use the aeson-compatible API.

### Executables

Converters `json2yaml` and `yaml2json` can be built by disabling flag `no-exe`, e.g., one of:
```
cabal install yaml -f-no-exe
stack install yaml --flag yaml:-no-exe
```

## yaml

[![Build Status](https://travis-ci.org/snoyberg/yaml.svg?branch=master)](https://travis-ci.org/snoyberg/yaml)
[![Build status](https://ci.appveyor.com/api/projects/status/hqy2jketp8m502so/branch/master?svg=true)](https://ci.appveyor.com/project/snoyberg/yaml/branch/master)

Provides support for parsing and emitting Yaml documents.

This project includes a `libyaml` package, which has the [full libyaml C library version 0.2.1 by Kirill Simonov](https://github.com/yaml/libyaml) in the package so you don't need to worry about any non-Haskell dependencies.

The project is broken down into two primary packages/modules. `yaml` contains `Data.Yaml`, which provides a high-level interface based around the JSON datatypes provided by the `aeson` package. `libyaml` contains `Text.Libyaml`, which provides a lower-level, streaming interface. For most users, `yaml` and `Data.Yaml` is recommended.

### Examples

Usage examples can be found in the `Data.Yaml` documentation or in the [examples](https://github.com/snoyberg/yaml/tree/master/examples) directory.

### Additional `yaml` modules

* `Data.Yaml.Include` supports adding `!include` directives to your YAML files.
* `Data.Yaml.Builder` and `Data.Yaml.Parser` allow more fine-grained control of parsing an rendering, as opposed to just using the aeson typeclass and datatype system for parsing and rendering.
* `Data.Yaml.Aeson` is currently a re-export of `Data.Yaml` to explicitly choose to use the aeson-compatible API.

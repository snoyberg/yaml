## yaml

[![Build Status](https://travis-ci.org/snoyberg/yaml.svg?branch=master)](https://travis-ci.org/snoyberg/yaml)
[![Build status](https://ci.appveyor.com/api/projects/status/hqy2jketp8m502so/branch/master?svg=true)](https://ci.appveyor.com/project/snoyberg/yaml/branch/master)

Provides support for parsing and emitting Yaml documents.

This package includes the [full libyaml C library version 0.2.1 by Kirill Simonov](https://github.com/yaml/libyaml) in the package so you don't need to worry about any non-Haskell dependencies.

The package is broken down into two primary modules. `Data.Yaml` provides a high-level interface based around the JSON datatypes provided by the `aeson` package. `Text.Libyaml` provides a lower-level, streaming interface. For most users, `Data.Yaml` is recommended.

### Examples

Usage examples can be found in the `Data.Yaml` documentation or in the [examples](https://github.com/snoyberg/yaml/tree/master/examples) directory.

### Additional modules

* `Data.Yaml.Include` supports adding `!include` directives to your YAML files.
* `Data.Yaml.Builder` and `Data.Yaml.Parser` allow more fine-grained control of parsing an rendering, as opposed to just using the aeson typeclass and datatype system for parsing and rendering.
* `Data.Yaml.Aeson` is currently a re-export of `Data.Yaml` to explicitly choose to use the aeson-compatible API.

### Alternative YAML implementations

* [HsYAML](https://hackage.haskell.org/package/HsYAML) - A portable pure Haskell implementation of YAML 1.2
* [HsSyck](https://hackage.haskell.org/package/HsSyck) - Fast, lightweight YAML loader and dumper
* [YamlReference](https://hackage.haskell.org/package/YamlReference) - YAML 1.2 reference implementation

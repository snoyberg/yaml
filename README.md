## yaml

Provides support for parsing and emitting Yaml documents.

This package includes the [full libyaml C library version 0.1.7 by Kirill Simonov](http://pyyaml.org/wiki/LibYAML) in the package so you don't need to worry about any non-Haskell dependencies.

The package is broken down into two primary modules. `Data.Yaml` provides a high-level interface based around the JSON datatypes provided by the `aeson` package. `Text.Libyaml` provides a lower-level, streaming interface. For most users, `Data.Yaml` is recommended.

### Examples

Usage examples can be found in the `Data.Yaml` documentation or in the [examples](./examples) directory.

### Additional modules

* `Data.Yaml.Include` supports adding `!include` directives to your YAML files.
* `Data.Yaml.Builder` and `Data.Yaml.Parser` allow more fine-grained control of parsing an rendering, as opposed to just using the aeson typeclass and datatype system for parsing and rendering.
* `Data.Yaml.Aeson` is currently a re-export of `Data.Yaml` to explicitly choose to use the aeson-compatible API.

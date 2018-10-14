# YAML Haskell Library

This project contains two haskell packages: `yaml` for higher-level parsing and writing of yaml documents, and `libyaml`
for lower-level event-based streaming.

## `yaml` Package

`yaml` provides a high-level interface based around the JSON datatypes provided by the `aeson` package. This allows
using JSON and YAML interchangeably in your code with the `aeson` typeclasses. See the
[yaml README](https://github.com/snoyberg/yaml/tree/master/yaml/README.md) for more details.

## `libyaml` Package

`libyaml` is a wrapper over the libyaml C library (and includes the source so no external library is needed). It is an
event-based streaming API. See the [libyaml README](https://github.com/snoyberg/yaml/tree/master/libyaml/README.md) for
more details.

## License

This project is licensed with the BSD 3-Clause license. Copies of the license can be found in both the `yaml` and
`libyaml` subdirectories

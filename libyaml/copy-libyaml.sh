#!/usr/bin/env bash

set -ex

rm -rf libyaml_src
mkdir -p libyaml_src
cp vendor/libyaml/LICENSE libyaml_src
cp vendor/libyaml/src/*.c libyaml_src
cp vendor/libyaml/src/*.h libyaml_src
cp vendor/libyaml/include/*.h libyaml_src

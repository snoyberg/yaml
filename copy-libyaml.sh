#!/usr/bin/env bash

set -ex

rm -rf libyaml
mkdir -p libyaml
cp vendor/libyaml/LICENSE libyaml
cp vendor/libyaml/src/*.c libyaml
cp vendor/libyaml/src/*.h libyaml
cp vendor/libyaml/include/*.h libyaml

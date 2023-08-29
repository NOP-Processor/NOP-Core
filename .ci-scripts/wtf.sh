#! /usr/bin/env bash

set -ex

for i in `seq 1 10`; do
    set +e
    false
    set -e
done
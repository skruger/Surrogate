#!/bin/bash

./rebar compile
./rebar generate force=1

./release/surrogate/bin/surrogate start
sleep 4

./release/surrogate/bin/surrogate ping || exit 1

python2.7 tests/surrogate_tests.py

sleep 2

./release/surrogate/bin/surrogate stop


#!/bin/sh

rm -fv ebin/*.beam
erl -pz ebin -make


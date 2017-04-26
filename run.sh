#!/bin/bash

ghc -O2 Main.hs
time cat $1.in | ./Main

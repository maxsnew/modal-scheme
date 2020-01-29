#!/bin/bash

echo "making challenge $1 directory"
mkdir $1 && cp template/* $1

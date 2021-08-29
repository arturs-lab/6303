#!/bin/bash

grep -v ^# ${1} | sed -n -e 'N;N;N;N;N;N;N;N;N;N;N;s/\n/,/g;s/^/\tdc.w /;p'


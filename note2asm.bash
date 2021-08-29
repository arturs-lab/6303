#!/bin/bash

cat "${1}" | tr -d '\r' | ./sn76notes2db.bash 

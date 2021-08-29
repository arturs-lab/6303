#!/bin/bash

declare -A map
declare -A d

map["P0"]=0
map["b1"]=1
map["B1"]=2
offset=3
for octave in 2 3 4 5 6 7; do
  for note in C d D e E F g G a A b B; do
    ref=${note}${octave}
    map[${ref}]=${offset}
    offset=$(( ${offset} + 1 ))
  done
done

d[0]=1
d[1]=2
d["1."]=3
d[2]=4
d["2."]=6
d[3]=8
d["3."]=12
d[4]=16
d["4."]=24
d[5]=32
d["5."]=48
d[6]=64
d["6."]=96
d[7]=128
d["7."]=192

#cat << EOF
#song    dc.w \$2000      ; playback speed
#        dc.b 7          ; default attenuation - 1, 3, 7, 15
#        dc.w chc
#        dc.w chb
#EOF

while read line; do
  first_char=$(echo ${line:0:1})
  if [ "x${first_char}" == "x" ]; then
    continue
  elif [ "x${first_char}" == "x:" ]; then
    echo "${line:1}"
  elif [ "x${first_char}" == "x;" ]; then
    echo "${line}"
  else
    printf "\tdc.b %s,%s,0\r\n" ${map[${line:0:2}]} ${d[${line:3}]}
    #printf "\t%s %s" ${line:0:2} ${d[${line:3}]}
  fi
done


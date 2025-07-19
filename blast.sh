#!/bin/bash

# blast parallel jenga builds, looking for odd behaviour
JOBS=2

scope=examples/01-two-files
#scope=examples/02-discover-deps
#scope=src

rm -rf /tmp/.cache/jenga
rm -f /tmp/blast.log*

install-jenga

expect=$(jenga build -fp $scope -j1 | grep ran | cut -d' ' -f3 | paste -sd+ | bc)
echo "expect:" $expect

max=0 # 0 means forever

i=0
while true; do
  i=$((i+1))
  echo -n "$i:"
  jenga build --debug -fp $scope -j$JOBS > /tmp/blast.log
  got=$(cat /tmp/blast.log | grep ran | cut -d' ' -f3 | paste -sd+ | bc)
  echo -n $got
  if [ $got != $expect ]; then
      echo ' **WRONG**';
      for pid in $(cat /tmp/blast.log | grep ran | cut -d[ -f2- | cut -d] -f1); do
          cat /tmp/blast.log | sed "s|^.$pid. .*$||" > /tmp/blast.log.$pid
      done
      #cat /tmp/blast.log
      cat /tmp/blast.log | grep locked: | cut -d' ' -f2- | sort | uniq -c
      exit
  else
      echo
  fi
  if [ $i = $max ]; then
      exit
  fi
done


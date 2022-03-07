#!/bin/bash

shadow-cljs compile node-test && node out/node_test.js
ecode=$?

if [ $ecode -eq 0 ]
then
  echo -e "\\033[0;32m\n💧 Droplet: All node tests passed!\n\033[0m"
else
  echo -e "\\033[0;31m\n💧 Droplet: Node tests failed! See errors above.\033[0m"
  exit 1
fi


#!/bin/bash

shadow-cljs compile node-test && node out/node_test.js
ecode=$?

if [ $ecode -eq 0 ]
then
  echo -e "\\033[0;32m\nDroplet: Node tests passed!\n\033[0m"
else
  echo -e "\\033[0;31m\nDroplet: Node tests failed! See errors above.\033[0m"
  exit 1
fi


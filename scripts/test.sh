#!/bin/bash
# Compiles and runs all the tests for Droplet.
bash $(dirname $0)/node_test.sh
ecode=$?

if [ $ecode -eq 0 ]
then
  echo -e "\033[0;36mDroplet: Running browser tests...\n\033[0m"
  bash $(dirname $0)/browser_test.sh &&  echo -e "\033[0;32m💧 Droplet: All tests passed!\n\033[0m"
fi


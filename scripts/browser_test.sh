#!/bin/bash

echo -e "\033[0;36mDroplet: Compiling browser tests...\n\033[0m"
shadow-cljs compile browser-test
echo -e "\033[0;36mDroplet: Compilation done, running browser tests with Karma...\n\033[0m"
karma start --single-run
ecode=$?

if [ $ecode -eq 0 ]
then
  echo -e "\033[0;32m\nDroplet: Browser tests passed!\033[0m"
else
  echo -e "\033[0;31m\nDroplet: Browser tests failed! See errors above.\033[0m"
  exit 1
fi


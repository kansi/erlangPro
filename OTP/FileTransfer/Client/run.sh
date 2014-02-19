#!/usr/bin/env bash

if [ "$#" -ne 1 ]; then
  echo "Usage: ./run.sh <path to file>" >&2
  exit 1
fi

echo "[+] Checking if erlang is installed on the system"
command -v erl >/dev/null 3>&1 || { echo "Erlang not installed on the system. Use README.txt file" >&2; exit 1;}
sleep 0.5
echo "[+] Success ..."
sleep 0.5

erl -sname clientNode@localhost -run client write $1 -run init stop -noshell

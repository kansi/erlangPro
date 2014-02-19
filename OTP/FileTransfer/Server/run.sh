#!/usr/bin/env bash

#erl -sname clientNode@localhost -run run main -run init stop -noshell

echo "[+] Checking if erlang is installed on the system"
command -v erl >/dev/null 3>&1 || { echo "[-] Erlang not installed on the system. Use README.txt file" >&2; exit 2;}
erl -sname serverNode@localhost -run readWriteServer start_link

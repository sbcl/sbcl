#!/bin/bash

inotifywait -e close_write,moved_to,create -m . |
while read -r directory events filename; do
  if [ "$filename" = "$1" ]; then
      dot -Tsvg $1 > $2
  fi
done

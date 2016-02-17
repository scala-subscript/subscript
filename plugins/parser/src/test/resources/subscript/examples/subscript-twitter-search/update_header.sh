#!/bin/sh

export SOURCE="/Users/anatolii/Projects/SubScript/examples/standard/subscript-twitter-search"
export DESTINATION="/Users/anatolii/Projects/SubScript/parser-plugin/subscript-parser/src/test/resources/subscript/examples/subscript-twitter-search"

export ORIGINAL_SRC="src/main/scala"
export REWRITTEN_SRC="target/scala-2.11/src_managed/main"


FILES=($(ls -1 "$SOURCE/$ORIGINAL_SRC/subscript/twitter/app/controller"))

for i in ${!FILES[*]}; do
  FILES[i]=${FILES[i]%.*}
  PREFIXES[i]="subscript/twitter/app/controller/"
done

PAYLOAD=("${FILES[*]}" "${PREFIXES[*]}")

# echo ${PAYLOAD[*]}

../../update.sh "${PAYLOAD[*]}"

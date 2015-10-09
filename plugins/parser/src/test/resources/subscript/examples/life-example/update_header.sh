#!/bin/sh

export SOURCE="/Users/anatolii/Projects/SubScript/examples/standard/life-example"
export DESTINATION="/Users/anatolii/Projects/SubScript/parser-plugin/subscript-parser/src/test/resources/subscript/examples/life-example"

export ORIGINAL_SRC="src/main/scala"
export REWRITTEN_SRC="target/scala-2.11/src_managed/main"


FILES=($(ls -1 "$SOURCE/$ORIGINAL_SRC/subscript/example/life"))

for i in ${!FILES[*]}; do
  FILES[i]=${FILES[i]%.*}
  PREFIXES[i]="subscript/example/life/"
done

PAYLOAD=("${FILES[*]}" "${PREFIXES[*]}")

# echo ${PAYLOAD[*]}

../../update.sh "${PAYLOAD[*]}"
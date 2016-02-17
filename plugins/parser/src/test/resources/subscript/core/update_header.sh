#!/bin/sh

export SOURCE="/Users/anatolii/Projects/SubScript/subscript/core"
export DESTINATION="/Users/anatolii/Projects/SubScript/parser-plugin/subscript-parser/src/test/resources/subscript/core"

export ORIGINAL_SRC="src/test/scala"
export REWRITTEN_SRC="target/scala-2.11/src_managed/test"


FILES=(
  "OperatorsSuite"
  "Test"
)

for i in ${!FILES[*]}; do
  PREFIXES[i]="subscript/"
done

PAYLOAD=("${FILES[*]}" "${PREFIXES[*]}")

# echo ${PAYLOAD[*]}

../update.sh "${PAYLOAD[*]}"

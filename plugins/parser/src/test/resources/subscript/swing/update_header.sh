#!/bin/sh

export SOURCE="/Users/anatolii/Projects/SubScript/subscript/swing"
export DESTINATION="/Users/anatolii/Projects/SubScript/parser-plugin/subscript-parser/src/test/resources/subscript/swing"

export ORIGINAL_SRC="src/main/scala"
export REWRITTEN_SRC="target/scala-2.11/src_managed/main"


FILES=(
  "Scripts"
  "SubScriptDebugger"
)

for i in ${!FILES[*]}; do
  PREFIXES[i]="swing/"
done

PAYLOAD=("${FILES[*]}" "${PREFIXES[*]}")

# echo ${PAYLOAD[*]}

../update.sh "${PAYLOAD[*]}"

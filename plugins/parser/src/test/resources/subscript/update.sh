#!/bin/sh

PAYLOAD=($*)
PAYLOAD_size=${#PAYLOAD[*]}

# echo ${PAYLOAD[*]}

FILES=(${PAYLOAD[*]:0:PAYLOAD_size/2})
PREFIXES=(${PAYLOAD[*]:PAYLOAD_size/2})

# echo ${FILES[*]}
# echo ${PREFIXES[*]}
# echo $PAYLOAD_size

for i in ${!FILES[*]}; do
       file=${FILES[i]}
  full_file="${PREFIXES[i]}${FILES[i]}"

   original="$SOURCE/$ORIGINAL_SRC/$full_file.scala"
  rewritten="$SOURCE/$REWRITTEN_SRC/$full_file.scala"

  original_destination="$DESTINATION/$file.scala"
  rewritten_destination="$DESTINATION/${file}_rewritten.scala"

  echo "$original -> $original_destination; $rewritten -> $rewritten_destination"

  cp $original $original_destination
  cp $rewritten $rewritten_destination
done
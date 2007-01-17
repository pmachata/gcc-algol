#!/bin/bash

if [ "$#" != "2" ]; then
    echo "usage: $0 file.ps file.pdf"
    echo "will attempt to modify file.pdf so as to have the same page size as file.ps"
    exit 1
fi

FILEPS="$1"
FILEPDF="$2"

case "$FILEPS" in
    *.ps) ;;
    *) echo "source file has to end with .ps"; exit 1;;
esac

case "$FILEPDF" in
    *.pdf) ;;
    *) echo "target file has to end with .pdf"; exit 1;;
esac

PAGESIZE=`sed -n '1,10{/^%%BoundingBox:/{s/^.*: //;p;}}' "$FILEPS"`

if [ -z "$PAGESIZE" ]; then
    echo "couldn\'t extract page size from $FILEPS"
    exit 1
fi

sed -ri 's/MediaBox \[([0-9]+ ?){4}\]/MediaBox ['"$PAGESIZE"']/' "$FILEPDF"

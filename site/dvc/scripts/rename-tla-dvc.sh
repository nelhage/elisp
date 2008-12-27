#! /bin/sh

if [ $# -ne 1 ]
then
    echo "usage: $(basename $0) string"
    exit 1
fi

perl -pi -e "s/tla-($1)/dvc-\$1/g" *.el

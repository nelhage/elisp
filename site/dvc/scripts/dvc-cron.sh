#! /bin/bash

# Generates the tarball and html documentation, upload it to gna.org
# This file is currently used only by Matthieu MOY, and is provided
# here only as an example. Copy it and modify it if you wish to use it.

export PATH=${HOME}/bin/local/verimag:${PATH}

cd `dirname $0`/..
mkdir -p tmp

echo "Executing $0 on $(date)."

rm -f dvc-snapshot.tar.gz
make MKDIR_P='mkdir -p' tarball || rm -f dvc-snapshot.tar.gz
if [ ! -f dvc-snapshot.tar.gz ]; then
    echo "Error creating tarball"
    exit 1
fi
mkdir -p www/download/
cp dvc-snapshot.tar.gz www/download/
make MKDIR_P='mkdir -p' -C texinfo dvc.html
mkdir -p www/docs/
cp texinfo/dvc.html www/docs/dvc-snapshot.html

# upload source and non-source at the same time
rsync -av www/ moy@download.gna.org:/upload/dvc/

echo "Finished $0 on $(date)"

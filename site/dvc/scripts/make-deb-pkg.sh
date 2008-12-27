#!/bin/sh

upload=no

while test $# -ne 0; do
    case "$1" in
        "--upload")
            shift
            upload="yes"
            ;;
        *)
            break
            ;;
    esac
done


echo "building package ..."
dpkg-buildpackage -rfakeroot -d
echo "building package ... done"

mkdir -p ++apt-repository
cd ++apt-repository
rsync -avr moy@download.gna.org:/upload/xtla-el/apt .
mv ../../*.deb apt/unstable
if which lintian > /dev/null; then
    lintian -c -vi apt/unstable/*.deb
fi

cd apt
apt-ftparchive packages . | gzip > unstable/Packages.gz
if [ "x$upload" = "xyes" ] ; then
    rsync -avr . moy@download.gna.org:/upload/xtla-el/apt
    echo
    echo
    echo "Files uploaded:"
    find .
else
    echo "No file uploaded (use --upload to upload)"
fi

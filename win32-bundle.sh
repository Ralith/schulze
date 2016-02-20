#!/usr/bin/env bash
set -ex

PKGDIR=votecount-`git describe --tags`-win32-x86_64
mkdir -p $PKGDIR/bin
mkdir $PKGDIR/lib
mkdir $PKGDIR/share

mv vote*.exe $PKGDIR/bin
cp -r /mingw64/lib/gdk-pixbuf-2.0 $PKGDIR/lib/
cp -r /mingw64/share/glib-2.0 $PKGDIR/share/
cp -r /mingw64/share/icons $PKGDIR/share/

7z a $PKGDIR.zip $PKGDIR

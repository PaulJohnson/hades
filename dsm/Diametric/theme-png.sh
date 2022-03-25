#! /bin/sh

# Generate 48x48 PNG versions of the SVG icons. The argument is the top level theme directory.

find $1/scalable -type d | sed -e "s/^/mkdir -p -v /" -e "s/scalable/48x48/" | bash

find $1/scalable -name "*.svg" | sed -e "s:$1/scalable/\\([^.]*\\).svg:convert -background none -resize 48x48 $1/scalable/\\1.svg $1/48x48/\\1.png:" | bash

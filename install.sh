#! /bin/sh

# Red Hat family prerequisites:
#
# In CentOS: dnf config-manager --set-enabled PowerTools
#
# dnf install gtk3-devel atk-devel gobject-introspection-devel cairo-gobject-devel librsvg2-devel
# dnf install pango-devel cairo-devel libicu-devel libpq-devel glib2-devel zlib-devel

# Debian family prerequisites:
#
# apt-get install libgtk-3-dev librsvg2-dev libpq-dev gobject-introspection libgirepository1.0-dev

# The Haskell Stack:
#
# wget -qO- https://get.haskellstack.org/ | sh

set -e
export THISDIR=`pwd`

echo -n "hades: "
git pull
echo -n "banana-ui-gtk: "
git -C ../banana-ui-gtk pull
echo -n "licensing: "
git -C ../licensing pull
echo -n "reactive-banana: "
git -C ../reactive-banana pull
echo  -n "blank-canvas: "
git -C ../blank-canvas pull

stack install


gtk-update-icon-cache share/hades/Diametric/
echo "Updated Diametric icon cache."
gtk-update-icon-cache "share/hades/Windows 10"
echo "Updated Windows 10 icon cache."

rm -r ~/.local/share/hades
echo "Deleted old data files."
mkdir -p ~/.local/share/hades/hicolor
cp -r gsn/hicolor/* ~/.local/share/hades/hicolor
cp -r causality/hicolor/* ~/.local/share/hades/hicolor
cp -r evidence/hicolor/* ~/.local/share/hades/hicolor
cp -r share/hades/* ~/.local/share/hades
echo "Copied data files to $HOME/.local/share/hades"

( cd ~/.local/ ; tar cfz $THISDIR/Docker/local/dsm.tgz share/hades bin/dsm )
( cd Fonts ; tar cfz $THISDIR/Docker/local/fonts.tgz *.?tf )
echo "Created docker files"

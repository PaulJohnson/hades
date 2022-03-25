#! /bin/bash

# Run this if any icons are modified. It regenerates the index.theme and icon-theme.cache files.

function createTheme () {

   THEME_NAME=$(basename "$1")
   THEME_COMMENT="$THEME_NAME Icon Theme"

   OUTPUT=$1



   echo "Creating icon theme '$THEME_NAME' in '$OUTPUT'"

   pushd "$OUTPUT"

   echo "Creating index.theme"

   echo -e "[Icon Theme]\nName=$THEME_NAME\nComment=$THEME_COMMENT\n" > index.theme
   echo -n "Directories=" >> index.theme

   DIRS=`find * -type d | grep -v git | grep "/" | sort -r`
   for foo in $DIRS
   do
   	echo -n "$foo," >> index.theme
   done

   DIRS=`find * -type d | grep -v git | grep -v scalable | grep "/" | sort -r`
   for foo in $DIRS
   do
     size=`echo $foo | sed 's/\x.*//'`
     type="Fixed"
     maxsize="MaxSize=512"
     echo -en "\n\n[$foo]\nSize=$size\nContext=`basename $foo`\nType=$type\n$maxsize" >> index.theme
   done

   DIRS=`find * -type d | grep -v git | grep scalable | grep "/" | sort -r`
   for foo in $DIRS
   do
     type="Scalable"
     maxsize="MaxSize=512"
     echo -en "\n\n[$foo]\nSize=scalable\nContext=`basename $foo`\nType=$type\n$maxsize" >> index.theme
   done
   echo "Updating cache"
   gtk-update-icon-cache .
   echo "Done"

   popd
}


createTheme "causality/hicolor"
createTheme "evidence/hicolor"
createTheme "gsn/hicolor"
createTheme "share/hades/hicolor"
createTheme "dsm/Diametric"

#!/bin/sh

if [ "$#" != 2 ]; then
  echo USAGE: $0 SRC/ DST/
  echo
  echo "Ignores files/directories from each of SRC\'s subdirectory\'s .gitignore files,"
  echo "as well as target/, node_modules/, and rsync\'s --cvs option:"
  echo "RCS SCCS CVS CVS.adm RCSLOG cvslog.* tags TAGS .make.state .nse_depinfo *~ #* .#*  ,*  _$*  *$"
  echo "                    *.old *.bak *.BAK *.orig *.rej .del-* *.a *.olb *.o *.obj *.so *.exe *.Z *.elc *.ln core .svn/"
  echo "                    .git/ .hg/ .bzr/"

  exit 1
fi

set -x
RSYNC_OPTS="--exclude=target --exclude=node_modules --cvs-exclude --archive --human-readable --progress --delete-after --verbose --recursive"


for srcdir in $1/*; do
  if [ -f "$srcdir"/.gitignore ]; then
    GITIGNORE_OPT=--exclude-from="$srcdir"/.gitignore
  fi
  rsync "$GITIGNORE_OPT" $RSYNC_OPTS --include=.git "$srcdir" "$2"
done

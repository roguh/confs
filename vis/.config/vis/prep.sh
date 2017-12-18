VIS=$HOME/src/vis

for F in $VIS/lua/* ; do ln -s $F ; done

git clone https://github.com/hucal/vis-backup local-plugins/vis-backup
git clone https://github.com/erf/vis-cursors local-plugins/vis-cursors

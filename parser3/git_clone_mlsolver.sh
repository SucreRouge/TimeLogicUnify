set -e
set -x
get () {
  [ -e "$1" -a ! -d "$1/.git" ] && rm -r "$1" || true
  [ -d "$1" ] || git clone https://github.com/tcsprojects/"$1"
  # Work around lack of Sting.lowercase_ascii in Ubuntu 16.04's ocaml:
  # And lack of "-pgs" option to output result
  (cd $1 && git checkout `git rev-list -n 1 --before="2015-09-01 01:00" master` && (make clean || true ))
  ls -l "$1"
  sleep 2
}
cd work
get mlsolver
cd mlsolver/
get pgsolver
cd  pgsolver/
get satsolversforocaml
get tcslib
(cd tcslib && (make -j4 || make))
(cd satsolversforocaml && (make -j4 || make))
#make -j5
cd ..
make -j4 || make

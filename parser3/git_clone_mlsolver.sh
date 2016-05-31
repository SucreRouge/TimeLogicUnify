set -e
set -x
get () {
  [ -d "$1" ] || git clone https://github.com/tcsprojects/"$1"
}
cd work
get mlsolver
cd mlsolver/
get pgsolver
cd  pgsolver/
get satsolversforocaml
get tcslib
make -j5
cd ..
make -j5

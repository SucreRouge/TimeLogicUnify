set -e
set -x
cd work
git clone https://github.com/tcsprojects/mlsolver.git
cd mlsolver/
git clone https://github.com/tcsprojects/pgsolver
cd  pgsolver/
git clone https://github.com/tcsprojects/satsolversforocaml
git clone https://github.com/tcsprojects/tcslib
make -j5
cd ..
make -j5

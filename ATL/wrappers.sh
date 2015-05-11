#ocamlc -g atl.ml -o atl; OCAMLRUNPARAM=l=100M  ocamlrun -b atl '{1}(XXp|XXq) & {}X{2}X-p & {}X{2} X-q'  2>&1 | head

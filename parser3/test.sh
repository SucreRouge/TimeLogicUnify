make && echo "U(p, q) : <>p" | (OCAMLRUNPARAM="b1" ./main) > 1 ; less 1
exit
echo "S(S(p, q), p) : (><p+>p)" | (OCAMLRUNPARAM="b1" ./main)
make && ./make_random_formulas.ml 10 200 1 | OCAMLRUNPARAM="b1" ./main

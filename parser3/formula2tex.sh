2tex () {
echo '\begin{eqnarray*}'
awk '{ print length(), $0 | "sort -n" }'  | sed 's/.* //'  | head -n 37 |
sed -f formula2tex.sed
echo '\end{eqnarray*}'
}

mkdir -p TeX
grep urules_manual_then_auto.txt -v -F -f urules_manual.txt | 2tex |tee TeX/unify_rules_newauto.tex
< urules_manual.txt 2tex |tee TeX/unify_rules_manual.tex 
< urules_auto.txt 2tex |tee TeX/unify_rules_auto.tex 

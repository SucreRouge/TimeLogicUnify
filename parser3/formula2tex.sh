2tex () {
echo '\begin{eqnarray*}'
awk '{ print length(), $0 | "sort -n" }'  | sed 's/.* //'  | head -n 35 |
sed -f formula2tex.sed
echo '\end{eqnarray*}'
}

grep urules_manual_then_auto.txt -v -F -f urules_manual.txt | 2tex |tee ~/uni/PhD/unify_rules_newauto.tex
< urules_manual.txt 2tex |tee ~/uni/PhD/unify_rules_manual.tex 
< urules_auto.txt 2tex |tee ~/uni/PhD/unify_rules_auto.tex 

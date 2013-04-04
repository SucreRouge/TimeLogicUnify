2tex () {
echo '\begin{eqnarray*}'
awk '{ print length(), $0 | "sort -n" }'  /var/www/urules.txt | sed 's/.* //'  | head -n 20
sed -f formula2tex.sed
echo '\end{eqnarray*}'
}

grep urules_manual_then_auto.txt -v -F -f urules_manual.txt | 2tex > ~/uni/PhD/unify_rules_newauto.tex
< urules_manual.txt wc 2tex > ~/uni/PhD/unify_rules_newauto.tex

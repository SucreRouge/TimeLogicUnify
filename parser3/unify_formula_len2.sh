mkdir -p TeX
(
cat <<EOF
\begin{tabular}{|l|r|r|r|r|r|}
\hline
rules & 1 atom & 2 atoms & 5 atoms & 10 atoms & 20 atoms \\\\
\hline
EOF
#./csv2tex.pl < results/unify_len.out | grep S
#./csv2tex.pl < results/unify_len.out | grep -v S
< results/unify_len.out sort -rnk2 | ./csv2tex.pl 
cat <<EOF
\hline
\end{tabular}
EOF
) | sed 's/S/	/
s/[.] /+CiME    /
s/null[+]//
s/\(\w*\)[+]\(\w*\)/\2+\1/g' |tee TeX/unify_len.tex
#cp results/unify_len.tex ~/uni/PhD || true

echo "% produced by `pwd`/$0"
(
cat <<EOF
\begin{tabular}{|l|r|r|r|r|r|}
\hline
rules & 1 atom & 2 atoms & 5 atoms & 10 atoms & 20 atoms \\\\
\hline
EOF
(
cat results/unify_len_TRS.out | head -n1 | sed 's/auto/TRS only/'
cat results/unify_len_both.out | sed s/^/TRS+/
) | ./csv2tex.pl 
cat <<EOF
\hline
\end{tabular}
EOF
) | tee results/unify_len_TRS.tex
cp results/unify_len.tex ~/uni/PhD || true

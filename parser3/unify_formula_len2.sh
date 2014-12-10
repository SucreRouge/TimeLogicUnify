(
cat <<EOF
\begin{tabular}{|l|r|r|r|r|r|}
\hline
rules & 1 atom & 2 atoms & 5 atoms & 10 atoms & 20 atoms \\\\
\hline
EOF
./csv2tex.pl < results/unify_len.out  #> results/unify_len.out 
cat <<EOF
\hline
\end{tabular}
EOF
) | tee results/unify_len.tex
cp results/unify_len.tex ~/uni/PhD || true

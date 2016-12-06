#under development
awk '{ print length($0) " " $0; }' urules_manual.txt | sort -n | cut -d ' ' -f 2- | perl scripts/lines2matrix.pl > ~/uni/pdf/urules_manual_matrix.tex


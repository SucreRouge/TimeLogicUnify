#< confluent.trs tr -d ' ' | sed 's/(\([01abcpqr]\))/\1/g' | awk '{ print length, $0 }' | sort -n | cut -d" " -f2- | sed -f trs2tex.sed | perl ../scripts/lines2matrix.pl > ~/uni/PhD/unify_trs_table2.tex
mkdir -p ../TeX
< confluent.trs tr -d ' ' | sed 's/(\([01abcpqr]\))/\1/g' | awk '{ print length, $0 }' | sort -n | cut -d" " -f2- | sed -f trs2tex.sed | perl ../scripts/lines2matrix.pl > ../TeX/unify_trs_table2.tex

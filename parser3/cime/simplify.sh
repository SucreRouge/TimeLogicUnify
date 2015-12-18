cd `dirname "$0"`
#echo "DODODODO $1" 1>&2

CIME_FILE=out/simplify.cime.$$.tmp
FORMULA=$(echo "$1" | 
	sed 's/(\([^()]*\)>\([^()]*\))/(~\1|\2)/g' | sed '
s/[*]/&/g
s/\(.\)/\1 /g
s/-/~/g
s/>/ I /g
'
)
#echo "EHCO $FORMULA"

(
grep -v '[-]>' < cime_header.txt 
echo '  x I y -> (~x|y);'
cat confluent.trs
cat <<EOF
";
normalize R_CTL (term A_CTL "$FORMULA");
EOF
#) | less # ./c3_2605_stat2.opt 
) > $CIME_FILE 

./c3_2605_stat2.opt -v 99 -icime $CIME_FILE 2> /dev/null | tr '\n' ' ' | sed 's/.*- : F_CTL term = //
s/Cime worked for.*//
s/let F_CTL =.*//' | tr -d ' '
rm $CIME_FILE

#| ./c3_2605_stat2.opt -v 9 #| grep "\- : F_CTL term =" | sed 's/- : F_CTL term = //
#s/^CiME> //' 

##################################
exit
#confluent.trs was generated with:
cat t | grep .... | sed 's/x0/b/g
s/U/ U /g
' > confluent.trs 




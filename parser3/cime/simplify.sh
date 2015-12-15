cd `dirname "$0"`
echo "DODODODO $1" 1>&2

FORMULA=$(echo "$1" | 
	sed 's/(\([^()]*\)>\([^()]*\))/(~\1|\2)/g' | grep -v '[<>]' | sed '
s/[*]/&/g
s/\(.\)/\1 /g
s/-/~/g
s/>/ I /g
'
)

(
grep -v '[-]>' < cime_header.txt 
cat confluent.trs
cat <<EOF
";
normalize R_CTL (term A_CTL "$FORMULA");
EOF
#) | less # ./c3_2605_stat2.opt 
) | ./c3_2605_stat2.opt | grep "\- : F_CTL term =" | sed 's/- : F_CTL term = //
s/^CiME> //' 

##################################
exit
#confluent.trs was generated with:
cat t | grep .... | sed 's/x0/b/g
s/U/ U /g
' > confluent.trs 




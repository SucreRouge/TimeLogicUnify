#< ../sample_rewrite_rules.txt sed 's/[+]/|/g
#grep -v '>' < sample_rewrite_rules.in | sed '
#sed 's/(\([^()]*\)>\([^()]*\))/(~\1|\2)/g' < sample_rewrite_rules.in | grep -v '[<>XN]' | sed '
sed 's/(\([^()]*\)>\([^()]*\))/(~\1|\2)/g' < sample_rewrite_rules.in | grep -v '[<>]' | sed '
s/^[(]//g
s/[)]$//g
s/[*]/&/g
s/\(.\)/\1 /g
s/-/~/g
s/>/ I /g
s/=/ -> /g
s/ $/;/g
'

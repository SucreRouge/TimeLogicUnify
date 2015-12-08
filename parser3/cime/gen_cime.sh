#< ../sample_rewrite_rules.txt sed 's/[+]/|/g
grep -v '>' < sample_rewrite_rules.in | sed '
s/^[(]//g
s/[)]$//g
s/[*]/&/g
s/\(.\)/\1 /g
s/-/~/g
s/>/ I /g
s/=/ -> /g
s/ $/;/g
'

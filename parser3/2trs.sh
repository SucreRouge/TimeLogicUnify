echo '(VAR x y z)'
echo '(RULES'
sed 's/^/  /
s/=/ -> /
s/0/0()/g
s/1/1()/g
' 
echo ')'

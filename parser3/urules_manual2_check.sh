export UNIFY_DO_NEG=N
export UNIFY_SOLVERS=*
#export UNIFY_TIMEOUT=3600
export UNIFY_TIMEOUT=60
(echo '<'
 sed s/^/-/ < urules_manual2.txt
 sed < urules_manual2.txt 's/^/-/
s/a/Xa/g
s/b/Xb/g
s/b/Xb/g'
) | ./unify



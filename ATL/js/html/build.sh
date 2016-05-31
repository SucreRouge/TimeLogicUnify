rm bctl.ml batl.ml
cp ../../bctl_memo.ml bctl.ml
cp ../../batl.ml batl.ml
../webworker/ml2js.sh bctl '(p&AG(p>EXp))&AF-p'
../webworker/ml2js.sh batl '(P^-{1}P)'
#scp b?tl{.js,.html,-plain.html} my_www:/var/www/html/app/
cp b?tl{.js,.html,-plain.html} /var/www ||
ls b?tl{.js,.html,-plain.html} | rsync -a --files-from=- . my_www:/var/www/html/app

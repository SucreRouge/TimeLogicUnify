#!/bin/bash
set -e
rm bctl.ml batl.ml || true
cp ../../bctl_memo.ml bctl.ml
cp ../../batl.ml batl.ml
../webworker/ml2js.sh bctl '(p&AG(p>EXp))&AF-p'
../webworker/ml2js.sh batl '(P^-{1}P)'
echo Attempting to Install
#scp b?tl{.js,.html,-plain.html} my_www:/var/www/html/app/
cp b?tl{.js,.html,-plain.html} /var/www/html/app ||
ls b?tl{.js,.html,-plain.html} | rsync -a --files-from=- . my_www:/var/www/html/app

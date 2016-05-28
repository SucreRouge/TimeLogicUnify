tar --transform "s,^,ml2js/," -c JsFooter.ml JsHeader.ml index.html ml2js.sh sample_cli.ml | gzip-99 ml2js.tgz
ssh root@us.dansted.org "mkdir -p /var/www/html/ml2js; ln -s /var/www/html/ml2js /var/www/ml2js"
scp index.html sample_cli.js ml2js.tgz root@us.dansted.org:/var/www/html/ml2js
#ssh root@us.dansted.org "chmomkdir -p /var/www/html/ml2js; ln -s /var/www/html/ml2js /var/www/ml2js"
firefox http://us.dansted.org/ml2js

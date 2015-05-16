rm atl/index.html
perl -e 'while(<>){if (/atlweb.js/) {print "<script>\n".`cat atlweb.js`."\n</script>"} else {print $_}}' <  index.html > atl/index.html
rsync -Lrp atl gp04:WWW/
echo http://www.csse.uwa.edu.au/~john/atl/

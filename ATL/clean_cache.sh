for f in cache.txt cache2.txt
do
	cp $f $f.bak
	grep -v signal.15 < $f.bak > $f
done

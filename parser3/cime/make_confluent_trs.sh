< out/good.output sed '0,/^Completion succeeded/d' | sed '0,/trs A_CTL/g' | grep '[-][>]' > t2
< t2 sed 's/x0/p/g
s/U/ U /g
' > confluent.trs

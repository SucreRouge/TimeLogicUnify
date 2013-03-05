o="$1.orig"
if [ -e "$o" ]
 then
	(sed 's/\s*$//' | sed 's/        /\t/g' | sed 's/       /\t/g' | sed 's/      /\t/g' | sed 's/     /\t/g' | sed 's/    /\t/g') < "$1.orig" > "$1"
else
	echo "cannot find $o"
fi

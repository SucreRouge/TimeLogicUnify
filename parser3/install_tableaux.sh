mkdir -p work
mkdir -p work/out
[ -e work/mlsolver ] || ./git_clone_mlsolver.sh
[ -e work/mark ] || (
	if ! command -v javac
	then
		echo "You will need to install a java compiler (javac) to compile the pure and BCTL* tableaux."
		exit
	fi
	cd work
	if [ -e ~/prj/CTLStarTab ]
	then
		ln -s ~/prj/CTLStarTab mark
	else
		git clone git@github.com:gmatht/CTLStarTab.git
		mv CTLStarTab mark
		cp -ra mark mark_v1
		(cd mark_v1 && git checkout v1.x)
		for d in mark mark_v1
		do (
			cd $d
			javac *java formulas/*java
		)
		done
		#mv mark_v1 mark/src/v1.0 # really v1.x., should fix, but not now
	fi
)
(
	cd cime &&
	[ -e c3_2605_stat2.opt ] || wget http://a3pat.ensiie.fr/pub/c3_2605_stat2.opt
	chmod +x c3_2605_stat2.opt
)

mkdir -p work
mkdir -p work/out
[ -e work/mlsolver ] || ./git_clone_mlsolver.sh
[ -e work/mark ] || (
	if ! command -v javac
	then
		echo "You will need to install a java compiler (javac) to compile the pure and BCTL* tableaux."
	fi
	cd work
	if [ -e ~/prj/CTLStarTab ]
	then
		ln -s ~/prj/CTLStarTab mark
	else
		git clone git@github.com:gmatht/CTLStarTab.git
		mv CTLStarTab mark
		cd mark
		javac *java formulas/*java
	fi
)
(
	cd cime &&
	[ -e c3_2605_stat2.opt ] || wget http://a3pat.ensiie.fr/pub/c3_2605_stat2.opt
)

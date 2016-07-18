mkdir -p work
mkdir -p work/out
command -v firejail || apt-get install firejail || (
	cd work/ 
	git clone https://github.com/netblue30/firejail.git 
	cd firejail/ 
	git branch 9.40 
	./configure  
	make 
	sudo make install
)
#Used as:
#firejail --net=none --shell=none --private-dev=none --private=.  --private-bin=true ./ctlrp21_x86_64 example.01.dfg
[ -e work/mlsolver/bin/mlsolver ] || ./git_clone_mlsolver.sh
[ -e work/mark/src/JApplet.class ] || (
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
		[ -e mark/.git ] || (
		(rm -r mark || true; git clone git@github.com:gmatht/CTLStarTab.git) ||
			git clone https://github.com/gmatht/CTLStarTab.git
		mv CTLStarTab mark
		cp -ra mark mark_v1
		(cd mark_v1 && git checkout v1.x)
                )
		for d in mark mark_v1
		do (
			cd $d/src
			javac *java formulas/*java
		)
		done
		#mv mark_v1 mark/src/v1.0 # really v1.x., should fix, but not now
	fi
)
[ -e work/graph ] || (
	wget http://rsise.anu.edu.au/~rpg/CTLProvers/ctlgraph.tar
	cd work
	tar -xf ../ctlgraph.tar 
	cd graph
	sudo apt install ocaml-native-compilers
	make
)

(
	cd cime &&
	[ -e c3_2605_stat2.opt ] || wget http://a3pat.ensiie.fr/pub/c3_2605_stat2.opt
	chmod +x c3_2605_stat2.opt
)
cd ../ATL; make

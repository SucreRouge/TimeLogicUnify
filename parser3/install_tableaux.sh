#!/bin/bash
set -x
mkdir -p work
mkdir -p work/out
trap "exit" INT
command -v firejail || (
	if apt-get --dry-run install firejail
	then
		sudo apt-get install firejail
	else 
		cd work/ 
		git clone https://github.com/netblue30/firejail.git 
		cd firejail/ 
		git branch 9.40 
		./configure  
		make 
		sudo make install
	fi
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
		)
		#mv mark_v1 mark/src/v1.0 # really v1.x., should fix, but not now
	fi
)
[ -e work/mark_v1/src/JApplet.class ] || (
	cd work
	cp -ra mark/ mark_v1/
	(cd mark_v1 && git checkout v1.x)
	for d in mark mark_v1
	do (
		cd $d/src
		javac *java formulas/*java
	)
	done
)
[ -e work/graph ] || (
	wget http://rsise.anu.edu.au/~rpg/CTLProvers/ctlgraph.tar
	cd work
	tar -xf ../ctlgraph.tar 
	cd graph
	sudo apt install ocaml-native-compilers
	make
)

[ -e work/bddctl/bddctl ] || (
	[ -e work/buddy  ] || (
		cd work
		git clone https://github.com/utwente-fmt/buddy.git 
		cd buddy 
		sudo apt install libtool-bin 
		autoreconf --install 
		autoconf
		./configure  
		make 
		sudo make install 
	)
	[ -e bddctl.tgz ] || wget http://rsise.anu.edu.au/~rpg/CTLProvers/bddctl.tgz
	cd work
	tar zxf ../bddctl.tgz
	cd bddctl
	sed -i.bak 's/unique_ptr<CTLFormula::CTLFormula>/unique_ptr<typename CTLFormula::CTLFormula>/g' CTLMisc.cpp
	make
)

(
	cd cime &&
	[ -e c3_2605_stat2.opt ] || wget http://a3pat.ensiie.fr/pub/c3_2605_stat2.opt
	chmod +x c3_2605_stat2.opt
)
cd ../ATL; make

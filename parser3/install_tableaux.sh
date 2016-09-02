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
[ -e work/graph/ctl ] || (
	[ -e ctlgraph.tar ] || wget http://rsise.anu.edu.au/~rpg/CTLProvers/ctlgraph.tar
	cd work
	tar -xf ../ctlgraph.tar 
	cd graph
	sudo apt install ocaml-native-compilers
	make
)
[ -e work/ctlProver/ctl ] || (
	[ -e ctlProver_r1368.tar ] || wget http://rsise.anu.edu.au/~rpg/CTLProvers/ctlProver_r1368.tar
	cd work
	tar -xf ../ctlProver_r1368.tar
	cd ctlProver
	make
)

[ -e work/bddctl/bddctl ] || (
	command -v g++ || sudo apt-get install g++
	[ -e /usr/lib/libbdd.so.0  ] || (
		cd work
		git clone https://github.com/utwente-fmt/buddy.git 
		cd buddy 
		sudo apt install libtool-bin 
		autoreconf --install 
		autoconf
		./configure  
		make 
		sudo make install 
		ln -s /usr/local/lib/libbdd.so.0 /usr/lib/
	)
	[ -e bddctl.tgz ] || wget http://rsise.anu.edu.au/~rpg/CTLProvers/bddctl.tgz
	cd work
	tar zxf ../bddctl.tgz
	cd bddctl
	sed -i.bak 's/unique_ptr<CTLFormula::CTLFormula>/unique_ptr<typename CTLFormula::CTLFormula>/g' CTLMisc.cpp
	make
)

[ -e work/ctl-rp.jail/ctlrp21_x86_64 ] || (
	[ `arch` = x86_64 ]
	cd work/ 
	mkdir -p ctl-rp.jail 
	cd ctl-rp.jail 
	wget http://101.200.212.235:8080/01_lan_html/ctlrp/ctlrp21_x86_64 || 
		echo try: https://sourceforge.net/projects/ctlrp/

	#Check that the binary hasn't been corrupted
	#ddee03557477de1d4714237f0ed7f1be98b4c02b083fd61aa4b81da1eaabf99cc9a31b9b93c23735fc6f8db0e8c42680f36b549f1f67247a79bedff5aa98f for i686
	[ "`sha512sum < ctlrp21_x86_64`" == '818e09cf00d0c23e0fc91bed8554783c4aa5f8af660443728220c9418f98e2b78d30d8403b50134de60552c6845e4c3ecf5820fc58fef19abc46d100629af46c  -' ] &&
	[ "`sha256sum < ctlrp21_x86_64`" == '4a1c07aca80bd25ab80287a678671924662ecbb3edf54a76d6af24cdfd79e954  -' ] &&
	[ "`sha1sum < ctlrp21_x86_64`"   == 'f81e5ddeb4d2ee5aa67eccf308952ce7f6da32ad  -' ] &&
	[ "`md5sum < ctlrp21_x86_64`"    == '1f3e037bbdb20724b23e3acf5f50dea6  -' ] &&
	chmod +x ctlrp21_x86_64  
)

(
	cd cime &&
	[ -e c3_2605_stat2.opt ] || wget http://a3pat.ensiie.fr/pub/c3_2605_stat2.opt
	chmod +x c3_2605_stat2.opt
)
cd ../ATL; make

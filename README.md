# TimeLogicUnify
A Collection of Theorem Provers for Temporal Logics.

The applications can also be used via online web apps without having to download or compile anyting. This git repository contains:

1. [unify](./rewrite3) [[cgibin preview](http://us.dansted.org/unified.html)] A unified UI around various CTL* and BCTL* theorem provers, and an NL-BCTL* based rewrite system.
2. [mechecker](./rewrite3) [[cgibin preview](http://staffhome.ecm.uwa.edu.au/~00061811/papers/mechecker.html)] A Model Checker for General Linear Time
3. [batl](./ATL/batl.ml) [[js preview](http://www.dansted.org/app/batl.html)] A simple OCaml Tableau for BATL* (and NL-BATL*)
4. [bctl](./ATL/bctl.ml) [[js preview](http://www.dansted.org/app/bctl.html)] A simple OCaml Tableau for BCTL* (and NL-BCTL*)

The unified UI uses (4) above, and can also use:

1. [CTLStarTab](https://github.com/gmatht/CTLStarTab): Various Java based Tableaux for CTL\*, BCTL\* and NL-BCTL\*. These are the tableaux we gave uppercase names: CTL, BCTLOLD, BCTLNEW, BCTLHUE, BPATHUE and BPATH 
2. [mlsolver](https://github.com/tcsprojects/mlsolver): Hybrid Tableau for CTL* that requires a [pgsolver](https://github.com/tcsprojects/pgsolver)

To fetch all dependances and compile on Ubuntu just enter `./build.sh`. This was last tested on Ubuntu 14.04.

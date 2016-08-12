#!/bin/bash
cd unify_benchmarks || true

(
cd /home/john/Downloads/tmp/ctlcomparison/benchmarks
echo '<'
grep -v 'p25\|px\|q1' `find | grep ctl` | perl -pe 's/.*://;s/p([0-9]*)/chr(ord(a)+$1)/eg;s/ => / > /g;'
) | (
	cd ..
	#UNIFY_CPUS=1 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW BCTLHUE" ./unify
	#UNIFY_CPUS=1 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW mlsolve" ./unify
	UNIFY_TIMEOUT=60 UNIFY_SOLVERS="mlsolver_simple_fair" nice ./unify
) | tee ../results/anu_benchmark1.log 

(
cd /home/john/Downloads/tmp/ctlcomparison/benchmarks
echo '<'
grep -v 'p25\|px\|q1' `find | grep ctl` | perl -pe 's/.*://;s/p([0-9]*)/chr(ord(a)+$1)/eg;s/ => / > /;s/^/AFGa>(/;s/$/)/;'
) | (
	cd ..
	#UNIFY_CPUS=1 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW BCTLHUE" ./unify
	#UNIFY_CPUS=1 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW mlsolve" ./unify
	UNIFY_TIMEOUT=60 UNIFY_SOLVERS="BSHADES BCTLNEW mlsolver" nice ./unify
) | tee ../result/anu_benchmark2.log 

(
cd /home/john/Downloads/tmp/ctlcomparison/benchmarks
echo '<'
grep -v 'p25\|px\|q1' `find | grep ctl` | perl -pe 's/.*://;s/p([0-9]*)/chr(ord(a)+$1)/eg;s/ => / > /g;'
) | (
	cd ..
	#UNIFY_CPUS=1 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW BCTLHUE" ./unify
	#UNIFY_CPUS=1 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW mlsolve" ./unify
	UNIFY_TIMEOUT=60 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW mlsolver mlsolver_simple_fair" nice ./unify
) | tee ../results/anu_benchmark3.log 

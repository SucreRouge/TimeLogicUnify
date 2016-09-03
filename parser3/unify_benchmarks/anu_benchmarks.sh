#!/bin/bash
cd unify_benchmarks || true
export UNIFY_CPUS=1
export UNIFY_TIMEOUT=60
cd .. && make unify
[ -e ctlcomparison ] || (
	[ -e CTLComparisonBenchmarksNew.tgz ] || wget http://rsise.anu.edu.au/~rpg/CTLComparisonBenchmarks/CTLComparisonBenchmarksNew.tgz
	tar -zxf CTLComparisonBenchmarksNew.tgz
)
unify_benchmarks/convert_anu.sh|grep -v ERROR|
	UNIFY_SOLVERS="mlsolver_simple_fair" nice ./unify|
	tee results/anu_benchmark1.log

	unify_benchmarks/convert_anu.sh|grep -v ERROR|perl -pe 'if (/^[^<]/){s/^/AFGa>(/;s/$/)/}'|
	UNIFY_DO_POS="N" UNIFY_SOLVERS="BSHADES BCTLNEW BCTLHUE mlsolver" nice ./unify|
	tee results/anu_benchmark2.log

unify_benchmarks/convert_anu.sh|grep -v ERROR|
	UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW BCTLHUE mlsolver mlsolver_simple_fair" nice ./unify|
	 #UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW mlsolver mlsolver_simple_fair" nice ./unify|
	 tee results/anu_benchmark3.log

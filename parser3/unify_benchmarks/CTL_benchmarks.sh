cd unify_benchmarks || true
(
	echo '<'
	ocaml random_AXEX_formulas.ml 500 2 1 1000
) | (
	cd ..
	UNIFY_CPUS=1 UNIFY_TIMEOUT=60 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW BCTLHUE mlsolver BCTLOLD" ./unify
) | tee ../results/unify_CTL.log

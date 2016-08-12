cd unify_benchmarks || true
(
	echo '<'
	ocaml random_AXEX_formulas.ml 500 2 1 10
) | (
	cd ..
	UNIFY_CPUS=1 UNIFY_SOLVERS="ctl-rp anu-tr anu-bdd BSHADES BCTLNEW BCTLHUE mlsolver" ./unify
)

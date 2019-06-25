time nice E/PROVER/eprover -s --auto-schedule --proof-object --tptp3-format -m 9047 bs.tptp > results/e.ac &
time nice E/PROVER/eprover -s --auto --proof-object --tptp3-format -m 9047 bs.tptp > results/e.a &
time nice E/PROVER/eprover -s --proof-object --tptp3-format -m 9047 bs.tptp > results/e &
time nice Vampire---4.0/vampire --mode casc -m 10000 -t 6h bs.tptp > results/v.c &
time nice Vampire---4.0/vampire -m 10000 -t 6h bs.tptp > results/v &

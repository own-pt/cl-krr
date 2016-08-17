~/Vampire---4.0/vampire --mode casc -m 30000 -t 1D output.tptp > vamp-consistency.txt &

~/E/PROVER/eproof --auto-schedule --tptp3-format --memory-limit=30000 output.tptp > e-auto-consistency.txt &

~/E/PROVER/eproof --tptp3-format --memory-limit=30000 output.tptp > e-consistency.txt &

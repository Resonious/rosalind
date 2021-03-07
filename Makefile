
hamm: hamm.hs
	stack ghc $?
	./$@


gc: gc.hs
	stack ghc $?
	./$@ < $@_in.txt | tee $@_out.txt


fib: fib.hs
	stack ghc $?
	./$@


revc: revc.hs
	stack ghc $?
	./$@

rna: rna.hs
	stack ghc $?
	./$@

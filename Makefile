all: ghc jcsr embed run

ghc: 
	 ghc --make Main.hs

jcsr: 
	hastec Main.hs

embed: 
	./Main --embed=Main.js

run:
	./Main

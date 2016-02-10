all: ghc jcsr embed run

clean:
	rm Main Main.js Main.jsmod Main.hi Main.o

ghc: 
	ghc --make Main.hs

jcsr: 
	hastec Main.hs

embed: 
	./Main --embed=Main.js

run:
	./Main

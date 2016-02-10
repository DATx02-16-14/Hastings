all: ghc jcsr embed run

clean:
	rm main main.js main.jsmod main.hi main.o

ghc: 
	ghc --make Main.hs

jcsr: 
	hastec Main.hs

embed: 
	./Main --embed=Main.js

run:
	./Main

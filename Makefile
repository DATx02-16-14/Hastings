all: stack haste-cabal embed run

clean:
	rm -rf dist .stack-work

stack:
	stack build

haste-cabal:
	haste-cabal configure && haste-cabal build && mv app/Main.js dist/build/Main.js

embed:
	stack exec -- Hastings-exe --embed dist/build/Main.js --force

run:
	stack exec Hastings-exe

all: stack haste-cabal embed run

clean:
	rm -rf dist .stack-work

stack:
	stack build

haste-cabal:
	haste-cabal configure && haste-cabal build && mv app/StandaloneApp.js dist/build/StandaloneApp.js

embed:
	stack exec -- Hastings-exe --embed dist/build/StandaloneApp.js --force

run:
	stack exec Hastings-exe

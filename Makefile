standalone:
	stack build --flag Hastings:HasteStandalone
	haste-cabal configure -f HasteStandalone
	haste-cabal build
	stack exec -- Hastings-exe --embed app/StandaloneApp.js --force

server:
	stack build
	haste-cabal configure
	haste-cabal build

run:
	stack exec Hastings-exe

clean:
	rm -rf dist .stack-work

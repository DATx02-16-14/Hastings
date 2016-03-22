all: stack-standalone haste-cabal embed run
server: stack haste-cabal run

clean:
	rm -rf dist .stack-work

stack-standalone:
	stack build --flag Hastings:HasteStandalone

stack:
	stack build --flag Hastings:NoHasteStandalone

haste-cabal:
	haste-cabal configure && haste-cabal build

embed:
	mv app/StandaloneApp.js dist/build/StandaloneApp.js && stack exec -- Hastings-exe --embed dist/build/StandaloneApp.js --force

run:
	stack exec Hastings-exe

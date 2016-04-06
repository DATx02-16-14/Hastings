all: stack-standalone haste-cabal embed run
server: stack haste-cabal run

clean:
	rm -rf dist .stack-work

stack-standalone:
	stack build --flag Hastings:HasteStandalone

stack:
	stack build

haste-cabal:
	haste-cabal configure && haste-cabal build

embed:
	mv app/StandaloneApp.js dist/build/StandaloneApp.js && stack exec -- Hastings-exe --embed dist/build/StandaloneApp.js --force

run:
	stack exec Hastings-exe

html:
	mkdir tmp
	cp -r app/Main.hs src/* tmp
	(cd tmp; hastec --output-html Main.hs)
	mv tmp/Main.html index.html
	chmod 644 index.html
	rm -rf tmp

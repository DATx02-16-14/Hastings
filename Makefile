standalone:
	stack build --flag Hastings:HasteStandalone
	mkdir tmp
	cp -r app/StandaloneApp.hs src/* tmp
	(cd tmp; hastec StandaloneApp.hs)
	stack exec -- Hastings-exe --embed tmp/StandaloneApp.js --force
	rm -rf tmp

server:
	stack build
	mkdir tmp
	cp -r app/Main.hs src/* tmp
	(cd tmp; hastec --output-html Main.hs)
	mv tmp/Main.html index.html
	chmod 644 index.html
	rm -rf tmp

run:
	stack exec Hastings-exe

clean:
	rm -rf dist .stack-work

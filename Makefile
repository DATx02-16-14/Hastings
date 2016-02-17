all: stack haste-cabal embed run

clean:
	rm -rf dist .stack-work

stack:
	stack build

haste-cabal:
	haste-cabal configure && haste-cabal build

embed:
	stack exec -- Hastings-exe --embed dist/build/Hastings-exe/Hastings-exe --force

run:
	stack exec Hastings-exe

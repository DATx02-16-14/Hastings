# Hastings [![Build Status](https://travis-ci.org/DATx02-16-14/Hastings.svg?branch=travis-setup)](https://travis-ci.org/DATx02-16-14/Hastings)
A project for the course DATX02 at CHALMERS UNIVERSITY OF TECHNOLOGY. 

The project aims to assess the suitability of programmig client-server applications in Haskell using Haste.App, supplying feedback to the devloper and demonstrating advantages (if any). 

More concretely the project aims to create a simple game lobby and an implementation of chinese checkers. 

#### Members 
Benjamin Block, Joel Gustafsson, Michael Milakovic, Mattias Nilsen, André Samuelsson 


### Setup
The project uses stack and haste-cabal to compile the server and the client respectivly.

#### Stack install
See https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md<br>
#### haste-cabal
See https://github.com/valderman/haste-compiler

#### Build project
Run `stack setup` in the project root.

Then compile the server and run it by using `make all`, this will execute the following commands:
```bash
# Builds the project using GHC
stack build
# Builds the project with the haste compiler
haste-cabal configure && haste-cabal build
# Embeds the javascript files in the runnable, force so the command will run even if no changes has been made.
stack exec -- Hastings-exe --embed dist/build/Hastings-exe/Hastings-exe --force
# Run the server
stack exec Hastings-exe
```


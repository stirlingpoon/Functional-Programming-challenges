# Challenge 3
Drawing Art in JS

## Art
A Conway's Game of Life animation in a 100x100 toroidal grid with a preset pattern.

Rather than track 100x100 states, each board only stores the live cells. In each evolution, candidates cells must be next to currently alive cells and have the needed number of alive neighbour cells.

## Building
Install Elm
Run elm-make Main.elm --output=index.html
# LightsOutSolver

## Dependencies
* GHCI

## Usage
Run the following command to start GHCI:
```
ghci
```

To load in the Lights Out Solver run:
```
:l src/Main.hs
```

A solvable input can be run against the solve function as an example. The solvable input is:
[[1,0,0,0,0],[1,0,1,0,1],[1,0,0,0,1],[1,0,1,0,1],[0,0,0,0,1]]
To solve this input run the following command:
```
solve solvableInput
```

The solve function can be run against any 5x5 matrix input as long as the input is in the same format as the solvableInput variable above. A list of 5 list of 5 integers. Each list in the top level list represents a row in the matrix.
```
solve [[1,0,0,0,0],[1,0,1,0,1],[1,0,0,0,1],[1,0,1,0,1],[0,0,0,0,1]]
```
The output is either
```
Nothing
```
if the input is not solvable, or
```
Just [[0,0,0,1,1],[1,0,1,0,0],[0,0,0,0,0],[0,0,1,0,1],[1,1,0,0,0]]
```
If the input is solvabel, with the output matrix being the optimal solution to the inputted matrix.

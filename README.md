# gcakka

A solution provider for the graph coloring problem, based on simulated annealing
and implemented with Scala and Akka.


## Quick Start

1. To use the application one needs a file in the standard DIMACS format which
describes a graph.
2. One needs to adjust the file src/main/resources/application.conf by providing
the input and output file names at psa.inFileName and psa.outFileName.
3. Run it with
```
sbt run
```
and choose 1 for de.csmath.gc.akka.GCAkka.


## Graph Coloring

Graph coloring is the problem to find the lowest possible number of colors to
color every vertex of the graph such that there is no pair of adjacent vertices
which are colored equally. Since graph coloring is an NP complete problem, one
uses heuristics to compute a relatively small number of colors that is close to
the minimum.


## Simulated Annealing

Simulated annealing is a metaheuristic. It is therefore possible to use this
heuristic on many other problems, too. In the quest for the minimum costs it
generates, alters and evaluates solutions. It accepts worse solutions dependent
on the temperature of that time. The higher the temperature is the higher the
probability to accept worse solutions. By cooling down, this probability
decreases so that eventually only better solutions are accepted.
GCAkka provides parameters in the file application.conf to control the algorithm
of the simulated annealing.
- startTemp (a high temperature, e.g. 100.0, 1000.0 or 100000.0)
- endTemp (a low positive temperature, e.g. 0.1, 0.01 or 0.00001)
- numIter (number of iterations, meaning the number of generated solution
    per temperature state)
- coolingRate (a number lower, but close to 1, e.g. 0.98, 0.99)


## Akka Implementation

For GCAkka there are actors which generate solutions (solvers) and actors which
evaluate solutions (evaluators). By increasing the number of solvers it is
possible to search for the minimum in different places of the solution space.
By increasing the number of evaluators it is possible to parallelize the
evaluation of solutions. The following parameters can be changed in the file
application.conf.
- numSolvers - number of actors which generate solutions
- numEvaluators - number of evaluators (per solver) to parallelize the
    evaluation of a solution

Tests showed that only graphs with a high number of edges benefit from a higher
number of evaluators. There are too many factors to present actual numbers, but
it is recommended to start with numEvaluators set to 1 or 2 and increase the
number of solvers to produce more alternative solutions.

It is possible to create those actors on remote actor systems, such that scaling
is just a matter of changing a configuration file. There are two applications.
GCAkka is the main system which reads the input file, starts the algorithm and
writes the solution to the output file. RemoteAS is an actor system which
provides the possibility for the main system to create actors on it.

# gcakka

A graph coloring solution provider based on simmulated annealing implemented
with Scala and Akka.


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

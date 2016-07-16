package de.csmath.gc.akka

import de.csmath.psa._
import de.csmath.gc._


/**
 *  This is the application object to solve the graph coloring problem
 *  with simulated annealing, implemented with akka actors.
 */
object GCAkka {

    /**
     *  The main method of the application. It reads the file with a
     *  graph description, calls the solver based on simulated annealing
     *  and writes the computed solution to the output file.
     *  @param args The arguments array.
     */
    def main(args: Array[String]) {
        PSAConfig(args) flatMap {
            GCGraphIO.readFile(_)
        } map { case (graph,config) =>
            SolverActor.solve(graph,config)
        } flatMap { case (sol,config) =>
            GCGraphIO.writeFile(sol,config)
        }
    }

}

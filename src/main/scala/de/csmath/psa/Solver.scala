package de.csmath.psa

import scala.annotation.tailrec
import scala.util.Random
import scala.math.exp
import java.util.Date


/**
 *  A Solver solves a problem with the simulated annealing meta heuristic.
 *  @param conf A configuration object with parameters for the simulated
 *              annealing.
 */
abstract class Solver(conf: PSAConfig) {

    /**
     *  The start temperature of the simulated annealing process.
     */
    protected val startTemp = conf.startTemp

    /**
     *  The end temperature of the simulated annealing process.
     */
    protected val endTemp = conf.endTemp

    /**
     *  The number of solutions to create at each temperature state.
     */
    protected val numIter = conf.numIter

    /**
     *  The cooling rate. of the simulated annealing process.
     */
    protected val coolingRate = conf.coolingRate

    /**
     *  The evaluation object to evaluate created solutions.
     */
    protected def eval: Evaluator

    /**
     *  A Random object for random solution creation.
     */
    protected val rd = new Random(new Date().getTime())


    /**
     *  A pair (s,c) where s is a computed solution and c is the used
     *  configuration object. The solution is created with the simulated
     *  annealing algorithm.
     *  @param startSolution A randomly chosen first solution.
     *  @param rand          A Random object.
     *  @return A pair (s,c) where s is a computed solution and c is the used
     *          configuration object.
     */
    def solve(startSolution: Solution)(implicit rand: Random = rd): (Solution,PSAConfig) =
        (solveaux(startTemp,numIter,eval.evaluate(startSolution)),conf)


    /**
     *  The auxiliary function to create a solution with the simulated
     *  annealing algorithm.
     *  @param temp The current temperature.
     *  @param iter The number of iterations at this temperature state.
     *  @param sol  A randomly chosen first solution.
     *  @param rand A Random object.
     *  @return A computed solution.
     */
    @tailrec
    private def solveaux(temp: Double,iter: Int,sol: Solution)(implicit rand: Random = rd): Solution = {
        val devSol = eval.evaluate(sol.deviate(rand))
        val newSol = if (accept(sol,devSol,temp)) devSol else sol
        iter match {
            case 0 =>
                val newTemp = coolDown(temp)
                if (newTemp < endTemp) newSol else solveaux(newTemp,numIter,newSol)
            case _ =>
                solveaux(temp,iter - 1, newSol)
        }
    }


    /**
     *  The predicate to decide if the new solution is accepted according to
     *  the simulated annealing process.
     *  @param currSol The currently accepted solution.
     *  @param devSol  The new solution in question.
     *  @param temp    The current temperature.
     *  @param rand    A Random object.
     *  @return true if the new solution is accepted.
     */
    protected def accept(currSol: Solution, devSol: Solution, temp: Double)(implicit rand: Random): Boolean = {
        val delta = devSol.costs - currSol.costs
        delta < 0 || rand.nextDouble < exp(-delta/temp)
    }


    /**
     *  A new temperature.
     *  @param temp The current temperature.
     *  @return A new temperature.
     */
    protected def coolDown(temp: Double) = coolingRate * temp

}

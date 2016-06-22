package de.csmath.psa

import scala.util.Random


/**
 *  The abstract class of solutions of the simmulated annealing process.
 */
abstract class Solution extends Ordered[Solution] {

    /**
     *  The evaluated costs of the solution.
     */
    def costs: Double

    /**
     *  A new solution based on this solution.
     *  @param rand A Random object for random deviations.
     *  @return A new solution.
     */
    def deviate(rand: Random): Solution


    /**
     *  The implementation of the compare method for the Ordered trait.
     *  @param that Another solution
     *  @return -1 if this solution is better than that, 0 if both solutions
     *          are considered equal and 1 if that solution is better.
     */
    def compare(that: Solution) = this.costs.compare(that.costs)

}

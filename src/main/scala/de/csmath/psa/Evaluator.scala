package de.csmath.psa


/**
 *  An Evaluator evaluates a given solution.
 */
trait Evaluator {

    /**
     *  A new solution with evaluation information (a computed costs value).
     */
    def evaluate(sol: Solution): Solution

}

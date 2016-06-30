package de.csmath.gc

import scala.collection._
import scala.util._
import de.csmath.psa.Solution


/**
 *  The implementation of a solution of a simmulated annealing process
 *  adjusted to the solution of a graph coloring problem.
 *  @param colAssign Color assignment for each vertex.
 *                   colAssign(i) = j denotes that vertex i has color j.
 *  @param confNodes The set of vertices which are adjacent to a vertex with
 *                   the same color.
 *  @param costs     The evaluated costs of this solution.
 *  @param colUsage  The information of color usage.
 *                   colUsage(i) = j denotes that there are j vertices with
 *                   color i.
 */
class GCSolution (
    val colAssign: immutable.IndexedSeq[Int],
    val confNodes: Set[Int],
    val costs: Double,
    val colUsage: Map[Int,Int]
)
    extends Solution {

    /**
     *  The number of colors used to color the graph.
     */
    def numColors = colUsage.size


    /**
     *  A new solution which differs to this solution by recoloring one vertex.
     *  @param rand A Random object to choose a random vertex and a random
     *              color.
     *  @return A new Solution which is not evaluated.
     */
    def deviate(rand: Random) = {
        val (node,color) =
            if (confNodes.nonEmpty)
                (pickConfNode(rand),pickColor(1,rand))
            else
                (pickNode(rand),pickColor(0,rand))
        val devColAssign = mkColorAssign(node,color)
        val devColUsage = mkColUsage(devColAssign)
        new GCSolution(devColAssign,Set.empty[Int],0.0,devColUsage)
    }


    /**
     *  A randomly chosen color.
     *  @param delta Extends the color range to the
     *               number of used colors + delta.
     *  @param rand  A Random object to choose the random color.
     *  @return An integer between 0 and (numColors + delta)
     */
    protected def pickColor(delta: Int, rand: Random) =
        rand.nextInt(numColors + delta)


    /**
     *  A randomly chosen vertex.
     *  @param rand A Random object to choose a random vertex.
     *  @return An integer between 0 and the number of vertices.
     */
    protected def pickNode(rand: Random) =
        rand.nextInt(colAssign.size)


    /**
     *  A randomly chosen vertex from the set of vertices with a conflict.
     *  @param rand A Random object to choose a random vertex.
     *  @return The number of a vertex with a conflict.
     */
    protected def pickConfNode(rand: Random) =
        confNodes.drop(rand.nextInt(confNodes.size)).head


    /**
     *  A new color assignment with a recolored vertex for which the following
     *  invariant holds.
     *       If n colors are used then any color c with 0 <= c < n
     *                   is used at least once.
     *  @param node  The vertex which needs to be recolored.
     *  @param color The new color.
     *  @return A new color assignment.
     */
    protected def mkColorAssign(node: Int, color: Int) = {
        val oldColor = colAssign(node)
        if (oldColor == color)
            colAssign
        else if (colUsage(oldColor) > 1)
            colAssign updated (node,color)
        else {
            val newColor = colUsage.size - 1
            for (col <- colAssign) yield
                if (col == newColor) oldColor
                else if (col == oldColor) color
                else col
        }
    }


    /**
     *  A new map with color usage information of the given color assignment.
     *  @param colAssign A color assignment where
     *                   colAssign(i) = j denotes that vertex i has color j.
     *  @return A new map m such that m(i) = j denotes that j vertices are
     *          colored with color i.
     */
    protected def mkColUsage(colAssign: immutable.IndexedSeq[Int]) =
        GCSolution.mkColUsage(colAssign)

}


/**
 *  The companion object to create solutions of the graph coloring problem.
 */
object GCSolution {

    /**
     *  A new solution.
     *  @param colAssign Color assignment for each vertex.
     *                   colAssign(i) = j denotes that vertex i has color j.
     *  @param confNodes The set of vertices which are adjacent to a vertex with
     *                   the same color.
     *  @param costs     The evaluated costs of this solution.
     *  @param colUsage  The information of color usage.
     *                   colUsage(i) = j denotes that there are j vertices with
     *                   color i.
     *  @return A new solution.
     */
    def apply(colAssign: immutable.IndexedSeq[Int],
              confNodes: Set[Int],costs: Double,colUsage: Map[Int,Int]) =
        new GCSolution(colAssign,confNodes,costs,colUsage)


    /**
     *  Destruction of a solution object.
     *  @param sol A solution.
     *  @return Some(a,b,c,d) where a is a color assignment, b is a set of
     *          vertices in conflicts, c are the evaluated costs and d is
     *          the map of color utilization.
     */
    def unapply(sol: GCSolution) =
        Some((sol.colAssign,sol.confNodes,sol.costs,sol.colUsage))


    /**
     *  A new randomly created solution.
     *  @param graph The graph which needs to be colored.
     *  @param rand  A Random object to randomly assign colors.
     *  @return A new solution.
     */
    def startSolution(graph: GCGraph, rand: Random) = {
        val colAssign = Vector() ++ (1 to graph.numVertices) map { _ =>
            rand.nextInt(2)
        }
        val colUsage = mkColUsage(colAssign)
        new GCSolution(colAssign,Set.empty[Int],0.0,colUsage)
    }


    /**
     *  A new map with color usage information of the given color assignment.
     *  @param colAssign A color assignment where
     *                   colAssign(i) = j denotes that vertex i has color j.
     *  @return A new map m such that m(i) = j denotes that j vertices are
     *          colored with color i.
     */
    protected def mkColUsage(colAssign: immutable.IndexedSeq[Int]) = {
        val size = colAssign.size
        val colUsage = mutable.Map[Int,Int]()
        for (i <- (0 until size)) {
            val color = colAssign(i)
            val num = colUsage.getOrElse(color,0)
            colUsage(color) = num + 1
        }
        colUsage.toMap
    }

}

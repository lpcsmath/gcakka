package de.csmath.graph

import scala.math._
import scala.math.Ordering.Int.{min,max}


/**
 *  The representation of a (possibly directed) edge of a graph.
 *  @param v1 The first vertex.
 *  @param v2 The second vertex.
 */
case class Edge(v1: Int, v2: Int) extends Ordered[Edge] {

    /**
     *  Compares two edges according to the Ordered trait.
     *  @param that Another edge.
     *  @return A value according lexical ordering.
     */
    def compare(that: Edge): Int = {
        val cmp = this.v1 compare that.v1
        if (cmp != 0) return cmp
        this.v2 compare that.v2
    }

}


/**
 *  An object to create an undirecte edge (v1,v2) such that v1 <= v2 holds.
 */
object UndirectedEdge {

    /**
     *  A new undirected edge.
     *  @param v1 The first vertex.
     *  @param v2 The second vertex.
     *  @return Edge(v1,v2) if v1 < v2, Edge(v2,v1) otherwise.
     */
    def apply(v1: Int, v2:Int): Edge = Edge(min(v1,v2),max(v1,v2))

}

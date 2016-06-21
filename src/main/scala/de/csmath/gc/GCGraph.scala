package de.csmath.gc

import de.csmath.graph._


/**
 *  The implementation of the graph for the graph coloring problem.
 *  The vertices are the consecutive integers starting with 0.
 *  @param numVertices The number of vertices of the graph.
 *  @param edges       The list of edges.
 */
case class GCGraph(val numVertices: Int,
              val edges: List[Edge]) extends Graph {

    /**
     *  The sequence of vertices.
     */
    def vertices = (0 until numVertices)

}

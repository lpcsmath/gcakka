package de.csmath.graph


/**
 *  An abstract class to represent a graph.
 */
abstract class Graph {

    /**
     *  The number of vertices.
     */
    def numVertices: Int

    /**
     *  The list of vertices.
     */
    def vertices: Traversable[Int]

    /**
     *  The list of edges.
     */
    def edges: Traversable[Edge]

}


/**
 *  An exception class to denote invalid graph data.
 */
case class InconsistentGraphException(val desc: String)
            extends IllegalArgumentException(desc) {}

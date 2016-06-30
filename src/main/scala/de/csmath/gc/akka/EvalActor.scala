package de.csmath.gc.akka

import scala.annotation.tailrec
import scala.collection.immutable._
import akka.actor.{Actor,Props,ActorLogging}
import de.csmath.graph._
import de.csmath.gc._
import EvalActor._


/**
 *  EvalActor evaluates solutions to a Graph Coloring problem. It counts
 *  the number of conflicts. It sends a pair (c,e) back to the requester,
 *  where c is a set of vertices which are adjacent to a vertex of the same
 *  color and e is the number of edges which combines two vertices of the same
 *  color.
 */
class EvalActor(val id: String) extends Actor with ActorLogging {

    /**
     *  The (possibly partial) list of edges of the graph.
     */
    private var edges: List[Edge] = Nil


    /**
     *  The pair (c,e), where c is a set of vertices which are adjacent to a
     *  vertex of the same  color and e is the number of edges which combines
     *  two vertices of the same color.
     *  @param colAssign The color assignment of the solution, such that
     *                   colAssing(i) = j denotes that vertex i has color j.
     *  @return The pair (c,e).
     */
    protected def evaluate(colAssign:IndexedSeq[Int]) = evalAux(colAssign,edges,Nil,0)


    /**
     *  The auxiliary function to evaluate the solution.
     *  @param ca The color assignment of the solution, such that
     *            colAssing(i) = j denotes that vertex i has color j.
     *  @param el The edge list of the graph.
     *  @param cn The temporary list of conflicting vertices.
     *  @param ne The temporary number of conflicting edges.
     *  @return The pair (c,e).
     */
     @tailrec
     private def evalAux(ca:IndexedSeq[Int],el:List[Edge],cn:List[Int],ne: Int): (List[Int],Int) =
         el match {
             case Nil => (cn, ne)
             case Edge(v1,v2) :: es =>
                if (ca(v1) == ca(v2))
                    evalAux(ca, es, (v1 :: v2 :: cn), (ne + 1))
                else
                    evalAux(ca, es, cn, ne)
    }


    /**
     *  Receives the (possibly partial) edge list of the graph and
     *  solutions which need to be evaluated.
     */
    def receive = {

        /**
         *  Edges(es), where es is a (possibly partial) edge list of the graph.
         */
        case Edges(es) => edges = es

        /**
         *  Solution(colAssign), where colAssign is the color assignment of
         *  the solution such that
         *  colAssing(i) = j denotes that vertex i has color j.
         */
         case Solution(colAssign) =>
            val (conflictNodes,numConfEdges) = evaluate(colAssign)
            sender() ! EvalResult(Set[Int]() ++ conflictNodes,numConfEdges)

    }

}


/**
 *  The companion object provides the Props configuration object and the
 *  message types.
 */
object EvalActor {

    /**
     *  The Props configuration object.
     *  @param id The application internal identifier.
     *  @return The Props object.
     */
     def props(id: String) = Props(classOf[EvalActor],id)


     /**
      *  The super class of all message types.
      */
      class Msg


      /**
       *  The message type for sending the needed graph information to the
       *  EvalActor.
       *  @param edges This is a (possibly partial) edge list of the graph.
       */
       case class Edges(edges: List[Edge]) extends Msg


       /**
        *  The message type for sending a solution which needs evaluation.
        *  @param colAssign is the color assignment of the solution such that
        *                   colAssing(i) = j denotes that vertex i has color j.
        */
        case class Solution(colAssign: IndexedSeq[Int]) extends Msg


        /**
         *  The message type for returning the evaluation result.
         *  @param confNodes    The set of conflicting vertices.
         *  @param numConfEdges The number of conflicting edges.
         */
         case class EvalResult(confNodes: Set[Int], numConfEdges: Int) extends Msg

}

package de.csmath.gc.akka

import akka.actor._
import akka.pattern.AskableActorRef
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.duration._
import de.csmath._
import de.csmath.graph._
import de.csmath.psa._
import de.csmath.gc._


/**
 *  Evaluator implements the evaluation operation to evaluate solutions of
 *  the graph coloring problem. Evaluator is used in the
 *  Simulated Annealing algorithm. It creates one or more actors and distributes
 *  the evaluation load among those actors.
 *  @param numEvaluators The number of evaluation actors.
 *  @param context       The context of the parent actor.
 */
class Evaluator(val numEvaluators: Int,
                val context: ActorContext) extends psa.Evaluator {

    /**
     *  The evaluation actors.
     */
     val evalActors = Vector() ++
        (1 to numEvaluators) map { i =>
            context.actorOf(EvalActor.props(i.toString), "evaluator" + i)
        } map ( new AskableActorRef(_))


    /**
     *  The evaluation operation of the SA algorithm.
     *  @param sol The solution, which needs to be evaluated.
     *  @return The solution with added evaluation information.
     */
    def evaluate(sol: Solution): Solution = evaluate(sol.asInstanceOf[GCSolution])


    /**
     *  The evaluation operation of the SA algorithm.
     *  @param sol The solution, which needs to be evaluated.
     *  @return The solution with added evaluation information.
     */
    def evaluate(sol: GCSolution): Solution = {
        implicit val timeout = new Timeout(10 seconds)
        val ca = Vector() ++ sol.colAssign
        val answers = evalActors map { evaluator =>
            (evaluator ? EvalActor.Solution(ca)).mapTo[EvalActor.EvalResult]
        }
        val results = answers map { Await result (_, 10 seconds) }

        val (confNodes,numEdges): (Set[Int],Int) =
            ((Set[Int](),0) /: results) { (x,y) =>
                val (partConfNodes,partNumEdges) = x
                val EvalActor.EvalResult(cn,ne) = y
                (partConfNodes ++ cn, partNumEdges + ne)
            }
        val d = if (numEdges > 0) 1 else 0
        val costs = 2.0 * numEdges + d + sol.numColors
        GCSolution(sol.colAssign,confNodes,costs,sol.colUsage)
    }


    /**
     *  This operation distributes the graph's edge list among the evaluation
     *  actors.
     *  @param graph The graph (GC problem).
     */
    def distributeGraph(graph: GCGraph) {
        val edges = graph.edges
        val n = if (edges.size % evalActors.size > 0)
                    edges.size / evalActors.size + 1
                else edges.size / evalActors.size
        evalActors zip (edges grouped (n) toList) foreach { pair =>
            val (act, es) = pair
            act.actorRef ! EvalActor.Edges(es)
        }
    }

}

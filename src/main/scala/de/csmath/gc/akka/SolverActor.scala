package de.csmath.gc.akka

import akka.actor._
import akka.pattern.AskableActorRef
import akka.util.Timeout
import scala.util.Random
import java.util.Date
import scala.concurrent.Await
import scala.concurrent.duration._
import com.typesafe.config._
import de.csmath.graph._
import de.csmath.psa._
import de.csmath.gc._


/**
 *  SolverActor is an actor which extends the solver for simulated annealing.
 *  It is adjusted to the needs of the graph coloring problem.
 *  @param id            The application specific identifier.
 *  @param numEvaluators The number of evaluation actors.
 *  @param config        The parameters for the simulated annealing process.
 */
class SolverActor(val id: String,
                  val numEvaluators: Int,
                  val config: PSAConfig) extends Solver(config) with Actor {

    /**
     *  The evaluator for the simmulated annealing algorithm.
     */
    val eval = new Evaluator(numEvaluators,context)


    /**
     *  Receives the graph of the graph coloring problem and returns the
     *  computed solution.
     */
    def receive = {
        case graph: GCGraph =>
            eval.distributeGraph(graph)
            val startSolution = GCSolution.startSolution(graph, rd)
            val (solution,_) = solve(startSolution)
            sender() ! solution

    }

}


/**
 *  The companion object to configure and start the solve actors.
 */
object SolverActor {

    /**
     *  The complete configuration according to file application.conf.
     */
    val akkaConfig = ConfigFactory.load()

    /**
     *  The part of the configuration for the central system (submission node).
     */
    val mainConfig = akkaConfig.getConfig("gcakka").withFallback(akkaConfig)

    /**
     *  The actor system of the central system.
     */
    val system = ActorSystem("GCAkka", mainConfig)

    /**
     *  The number of evaluator actors needed per solver actor.
     */
    val numEvaluators = mainConfig.getInt("numEvaluators")

    /**
     *  The number of solver actors needed.
     */
    val numSolvers = mainConfig.getInt("numSolvers")


    /**
     *  The Props object for the solver actors.
     *  @param id            The application specific identifier of the
     *                       solver actor.
     *  @param numEvaluators The number of needed evaluator actors.
     *  @param config        The parameters for the SA algorithm.
     *  @return The Props object.
     */
    def props(id: String, numEvaluators: Int, config: PSAConfig) =
        Props(classOf[SolverActor], id, numEvaluators, config)


    /**
     *  This operation configures and starts the solver actors and returns
     *  the best of the computed solutions.
     *  @param graph  The graph of the graph coloring problem.
     *  @param config The parameters of the simulated annealing process.
     *  @return The pair (s,c), where s is the computed solution and c is the
     *          given PSA configuration.
     */
    def solve(graph: GCGraph,config: PSAConfig): (GCSolution, PSAConfig) = {
        implicit val timeout = new Timeout(1000 seconds)
        val solvers = createSolvers(config)

        val answers = solvers map { solver =>
            (solver ? graph).mapTo[Solution]
        }
        val solutions = answers map { Await result (_, timeout.duration) }

        val best = (solutions.head /: solutions.tail) { (x,y) => if (x<y) x else y }
        system.terminate()
        println("best.costs = " + best.costs)
        (best.asInstanceOf[GCSolution],config)
    }


    /**
     *  The helper function to create the needed amount of solver actors.
     */
    private def createSolvers(config: PSAConfig) = {
        (1 to numSolvers) map { i =>
            val id = i.toString
            new AskableActorRef(
                system.actorOf(SolverActor.props(id,numEvaluators,config),"solver" + id))
        } toList
    }

}

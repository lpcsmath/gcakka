package de.csmath.gc.akka

import akka.actor.{Actor,Props,ActorSystem}
import akka.testkit.{TestKit,ImplicitSender}
import org.scalatest.{WordSpecLike,Matchers,BeforeAndAfterAll}
import de.csmath.gc._
import de.csmath.graph._
import de.csmath.gc.akka._

class EvalActorSpec(_system:ActorSystem) extends TestKit(_system)
      with ImplicitSender
      with WordSpecLike
      with Matchers
      with BeforeAndAfterAll {

  def this() = this(ActorSystem("MySpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "An EvalActor" must {
    "send back an empty set and zero, when uninitialized" in {
      val evaluator = system.actorOf(EvalActor.props("1"))
      val sol = GCSolution(Vector(1,2,3),Set[Int](),0.0,Map[Int,Int]())
      evaluator ! EvalActor.Solution(Vector(1,2,3))
      expectMsg(EvalActor.EvalResult(Set[Int](),0))
    }
    "send back confilcts, when there are" in {
      val evaluator = system.actorOf(EvalActor.props("1"))
      evaluator ! EvalActor.Edges(List(Edge(0,1),Edge(0,2)))
      evaluator ! EvalActor.Solution(Vector(2,2,2,2))
      expectMsg(EvalActor.EvalResult(Set[Int](0,1,2),2))
    }
    "send back an empty set and zero, if there are no conflicts" in {
      val evaluator = system.actorOf(EvalActor.props("1"))
      evaluator ! EvalActor.Edges(List(Edge(0,1),Edge(0,2)))
      evaluator ! EvalActor.Solution(Vector(2,1,1,2))
      expectMsg(EvalActor.EvalResult(Set[Int](),0))
    }
  }
}

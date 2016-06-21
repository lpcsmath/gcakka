package de.csmath.gc

import org.scalatest.{FlatSpec,Matchers}
import de.csmath.graph._

class EdgeSpec extends FlatSpec with Matchers {

    "An UndirectedEdge" should "sort the connected nodes" in {
            UndirectedEdge(4,3) shouldBe Edge(3,4)
            UndirectedEdge(3,4) shouldBe Edge(3,4)
            UndirectedEdge(3,3) shouldBe Edge(3,3)
    }

    "Edges" should "be totally ordered" in {
        Edge(2,4) should be <  Edge(3,4)
        Edge(3,4) should be <  Edge(3,5)
        Edge(3,4) <  Edge(3,4) shouldBe false
        Edge(3,4) == Edge(3,4) shouldBe true
    }


}

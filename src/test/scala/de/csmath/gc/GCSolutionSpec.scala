package de.csmath.gc

import org.scalatest.{FlatSpec,Matchers}
import de.csmath.gc._


class MockSolution(a:Vector[Int],b:Set[Int],c:Double,d:Map[Int,Int]) extends GCSolution(a,b,c,d) {
    
    def testMkColUsage(colAssign: Vector[Int]): Map[Int,Int] = mkColUsage(colAssign)
    
}

class GCSolutionSpec extends FlatSpec with Matchers {

    "A GCSolution" should "make correct ColorUsage maps" in {
        val sol = new MockSolution(Vector[Int](),Set[Int](),0.0,Map[Int,Int]())
        val result = sol.testMkColUsage(Vector(1,2,3,2,0,1))
        result(0) should === (1)
        result(1) should === (2)
        result(2) should === (2)
        result(3) should === (1)
    }

}

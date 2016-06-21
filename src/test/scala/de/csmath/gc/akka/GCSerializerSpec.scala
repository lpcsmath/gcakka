package de.csmath.gc

import org.scalatest.{FlatSpec,Matchers}
import de.csmath.psa._
import de.csmath.graph._
import de.csmath.gc._
import de.csmath.gc.akka._


class GCSerializerSpec extends FlatSpec with Matchers {

    "A GCSerializer" should "serialize a GCGraph" in {
        val numVertices = 3
        val edgeList = List(new Edge(0,1), new Edge(1,2))
        val graph = GCGraph(numVertices, edgeList)

        val ser = new GCSerializer
        val bin = ser.toBinary(graph)
        GCMsgType(bin(0)) should === (GCMsgType.GCGraph)
        val res = ser.fromBinary(bin,None)
        val GCGraph(a,b) = res
        a should === (numVertices)
        b should === (edgeList)
    }

    "A GCSerializer" should "serialize a GCSolution" in {
        val colAssign = Vector(1,2,3)
        val confNodes = Set(1,2)
        val costs = 42.0
        val colUsage = Map(1 -> 2, 3 -> 4)
        val sol = GCSolution(colAssign,confNodes,costs,colUsage)

        val ser = new GCSerializer
        val bin = ser.toBinary(sol)
        GCMsgType(bin(0)) should === (GCMsgType.GCSolution)
        val res = ser.fromBinary(bin,None)
        val GCSolution(a,b,c,d) = res
        a should === (colAssign)
        b should === (confNodes)
        c should === (costs)
        d should === (colUsage)
    }

    "A GCSerializer" should "serialize a PSAConfig" in {
        val inFileName = "test.col"
        val outFileName = "test.col.sol"
        val startTemp = 100.0
        val endTemp = 0.01
        val numIter = 123
        val coolingRate = 0.98
        val config = new PSAConfig(inFileName, outFileName, startTemp,
                                    endTemp, numIter, coolingRate)

        val ser = new GCSerializer
        val bin = ser.toBinary(config)
        GCMsgType(bin(0)) should === (GCMsgType.PSAConfig)
        val res = ser.fromBinary(bin,None)
        val PSAConfig(a,b,c,d,e,f) = res
        a should === (inFileName)
        b should === (outFileName)
        c should === (startTemp)
        d should === (endTemp)
        e should === (numIter)
        f should === (coolingRate)
    }

    "A GCSerializer" should "serialize a EvalActor.Edges" in {
        val edgeList = List(new Edge(0,1), new Edge(1,2))
        val es = EvalActor.Edges(edgeList)

        val ser = new GCSerializer
        val bin = ser.toBinary(es)
        GCMsgType(bin(0)) should === (GCMsgType.EA_Edges)
        val res = ser.fromBinary(bin,None)
        val EvalActor.Edges(a) = res
        a should === (edgeList)
    }

    "A GCSerializer" should "serialize a EvalActor.Solution" in {
        val colAssign = Vector(1,2,3)
        val sol = EvalActor.Solution(colAssign)

        val ser = new GCSerializer
        val bin = ser.toBinary(sol)
        GCMsgType(bin(0)) should === (GCMsgType.EA_Solution)
        val res = ser.fromBinary(bin,None)
        val EvalActor.Solution(a) = res
        a should === (colAssign)
    }

    "A GCSerializer" should "serialize a EvalActor.EvalResult" in {
        val confNodes = Set(1,2,3)
        val numEdges = 2
        val eRes = EvalActor.EvalResult(confNodes,numEdges)

        val ser = new GCSerializer
        val bin = ser.toBinary(eRes)
        GCMsgType(bin(0)) should === (GCMsgType.EA_Result)
        val res = ser.fromBinary(bin,None)
        val EvalActor.EvalResult(a,b) = res
        a should === (confNodes)
        b should === (numEdges)
    }

}

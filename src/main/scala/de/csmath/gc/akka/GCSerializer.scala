package de.csmath.gc.akka

import scala.math._
import scala.annotation.tailrec
import akka.actor.{ ActorRef, ActorSystem }
import akka.serialization._
import com.typesafe.config.ConfigFactory
import de.csmath.psa._
import de.csmath.gc._
import de.csmath.graph._
import de.csmath.gc.akka._


/**
 *  A serializer to serialize the messages to remote actors.
 */
class GCSerializer extends Serializer {

    /**
     *  A precission factor for serialized floating point values, which
     *  are serialized as integers.
     */
    val prec = 10000

    /**
     *  The serializer doesn't need a class manifest.
     */
    def includeManifest: Boolean = false

    /**
     *  The identifier of this serializer.
     */
    def identifier = 12062208


    /**
     *  The byte array of the serialized object.
     *  @param obj The object which needs to be serialized.
     *  @return The byte array of the serialized object.
     */
    def toBinary(obj: AnyRef): Array[Byte] = obj match {
        case graph: GCGraph =>
            (GCMsgType.GCGraph.id.toByte ::
                intToBinary(graph.numVertices) :::
                edgeListToBinary(graph.edges)).toArray
        case sol: GCSolution =>
            (GCMsgType.GCSolution.id.toByte ::
                intListToBinary(sol.colAssign) :::
                intListToBinary(sol.confNodes) :::
                longToBinary(sol.costs.toLong) :::
                iPairListToBinary(sol.colUsage.toList)).toArray
        case con: PSAConfig =>
            (GCMsgType.PSAConfig.id.toByte ::
                stringToBinary(con.inFileName) :::
                stringToBinary(con.outFileName) :::
                longToBinary(round(con.startTemp * prec)) :::
                longToBinary(round(con.endTemp * prec)) :::
                intToBinary(con.numIter) :::
                longToBinary(round(con.coolingRate * prec))).toArray
        case EvalActor.Edges(el) =>
            (GCMsgType.EA_Edges.id.toByte :: edgeListToBinary(el)).toArray
        case EvalActor.Solution(ca) =>
            (GCMsgType.EA_Solution.id.toByte :: intListToBinary(ca)).toArray
        case EvalActor.EvalResult(confNodes,numEdges) =>
            (GCMsgType.EA_Result.id.toByte :: intListToBinary(confNodes) :::
                intToBinary(numEdges)).toArray
    }


    /**
     *  The deserialized object from a given byte array.
     *  @param bytes The serialized version of the object.
     *  @return The deserialized object.
     */
    def fromBinary(bytes: Array[Byte],clazz: Option[Class[_]]): AnyRef =
        GCMsgType(bytes(0)) match {
            case GCMsgType.GCGraph =>
                val (pos1,numVertices) = intFromBinary(bytes,1)
                val (_,edges) = edgeListFromBinary(bytes,pos1)
                GCGraph(numVertices,edges)
            case GCMsgType.GCSolution =>
                val (pos1,colAssign) = intListFromBinary(bytes,1)
                val (pos2,confNodes) = intListFromBinary(bytes,pos1)
                val (pos3,costs)     = longFromBinary(bytes,pos2)
                val (_,colUsage)     = iPairListFromBinary(bytes,pos3)
                GCSolution(Vector() ++ colAssign, Set() ++ confNodes,
                    costs.toDouble, Map() ++ colUsage)
            case GCMsgType.PSAConfig =>
                val (pos1,inFileName) = stringFromBinary(bytes,1)
                val (pos2,outFileName) = stringFromBinary(bytes,pos1)
                val (pos3,startTemp) = longFromBinary(bytes,pos2)
                val (pos4,endTemp) = longFromBinary(bytes,pos3)
                val (pos5,numIter) = intFromBinary(bytes,pos4)
                val (_,coolingRate) = longFromBinary(bytes,pos5)
                new PSAConfig(inFileName,outFileName,startTemp/prec.toDouble,
                              endTemp/prec.toDouble,numIter,coolingRate/prec.toDouble)
            case GCMsgType.EA_Edges =>
                val (_,el) = edgeListFromBinary(bytes,1)
                EvalActor.Edges(el)
            case GCMsgType.EA_Solution =>
                val (_,ca) = intListFromBinary(bytes,1)
                EvalActor.Solution(Vector() ++ ca)
            case GCMsgType.EA_Result =>
                val (pos1,cn) = intListFromBinary(bytes,1)
                val (_,ne) = intFromBinary(bytes,pos1)
                EvalActor.EvalResult(Set() ++ cn, ne)
            case _ => ???
    }


    /**
     *  A serialized integer.
     *  @param num The integer, which needs to be serialized.
     *  @return A byte list.
     */
    def intToBinary(num: Int) = intToBinaryAux(num, 4)


    /**
     *  The auxiliary function to serialize an integer.
     *  @param num The integer, which needs to be serialized.
     *  @param i   The number of the byte from MSB ot LSB.
     *  @return A byte list.
     */
    def intToBinaryAux(num: Int, i: Int): List[Byte] = i match {
        case 0 => List[Byte]()
        case x => (num & 255).toByte :: intToBinaryAux(num >> 8, x - 1)
    }


    /**
     *  A pair (p,i), where p is the position of data which follows the
     *  serialized integer and i is the deserialized integer.
     *  @param ar  The byte array which contains the serialized integer.
     *  @param pos The position of the serialized integer within the array.
     *  @return A pair (p,i), such that p is the position of data which follows
     *          the serialized integer and i is the deserialized integer.
     */
    def intFromBinary(ar: Array[Byte], pos: Int) = intFromBinaryAux(ar,0,pos,0)


    /**
     *  The auxiliary function which deserializes an integer.
     *  @param ar    The byte array which contains the serialized integer.
     *  @param acc   The partially deserialized integer.
     *  @param pos   The position of the serialized integer within the array.
     *  @param count The number of the byte from MSB to LSB.
     *  @return A pair (p,i), such that p is the position of data which follows
     *          the serialized integer and i is the deserialized integer.
     */
    @tailrec
    private def intFromBinaryAux(ar: Array[Byte], acc: Int, pos: Int, count: Int): (Int,Int) =
        count match {
            case 4 => (pos,acc)
            case x =>
                val t = if (ar(pos) < 0) 256 + ar(pos) else ar(pos)
                intFromBinaryAux(ar, (t << (count << 3)) ^ acc, pos + 1, count + 1)
    }


    /**
     *  A serialized long integer.
     *  @param num The integer, which needs to be serialized.
     *  @return A byte list.
     */
    def longToBinary(num: Long) = longToBinaryAux(num, 8)


    /**
     *  The auxiliary function to serialize a long integer.
     *  @param num The integer, which needs to be serialized.
     *  @param i   The number of the byte from MSB ot LSB.
     *  @return A byte list.
     */
    def longToBinaryAux(num: Long, i: Int): List[Byte] = i match {
        case 0 => List[Byte]()
        case x => (num & 255).toByte :: longToBinaryAux(num >> 8, x - 1)
    }


    /**
     *  A pair (p,i), where p is the position of data which follows the
     *  serialized integer and i is the deserialized integer.
     *  @param ar  The byte array which contains the serialized integer.
     *  @param pos The position of the serialized integer within the array.
     *  @return A pair (p,i), such that p is the position of data which follows
     *          the serialized integer and i is the deserialized integer.
     */
    def longFromBinary(ar: Array[Byte], pos: Int) = longFromBinaryAux(ar,0,pos,0)


    /**
     *  The auxiliary function which deserializes a long integer.
     *  @param ar    The byte array which contains the serialized integer.
     *  @param acc   The partially deserialized integer.
     *  @param pos   The position of the serialized integer within the array.
     *  @param count The number of the byte from MSB to LSB.
     *  @return A pair (p,i), such that p is the position of data which follows
     *          the serialized integer and i is the deserialized integer.
     */
    @tailrec
    private def longFromBinaryAux(ar: Array[Byte], acc: Long, pos: Int, count: Int): (Int,Long) =
        count match {
            case 8 => (pos,acc)
            case x =>
                val t = if (ar(pos) < 0) 256 + ar(pos) else ar(pos)
                longFromBinaryAux(ar, (t << (count << 3)) ^ acc, pos + 1, count + 1)
    }


    /**
     *  A serialized list of integers.
     *  @param li A list of integers.
     *  @return A list of bytes prefixed by the length of
     *          the list as a serialized integer.
     */
    def intListToBinary(li: Traversable[Int]) =
        intListToBinaryAux(li,List[Byte](),0)


    /**
     *  The auxiliary function to serialize a list of integers.
     *  @param li A list of integers.
     *  @param acc The partially constructed list of serialized integers.
     *  @param length The number of already serialized integers.
     *  @return A list of bytes prefixed by the length of
     *          the list as a serialized integer.
     */
    @tailrec
    private def intListToBinaryAux(li: Traversable[Int], acc: List[Byte], length: Int): List[Byte] =
        li match {
            case l if (l.isEmpty) => intToBinary(length) ::: acc
            case l =>
                intListToBinaryAux(li.tail,intToBinary(l.head) ::: acc, length + 1)
    }


    /**
     *  A pair (p,l), where p is the position of data following the serialized
     *  list of integers and l is the list of deserialized integers.
     *  @param ar  The byte array, which contains the list of integers.
     *  @param pos The position of the integer list within the byte array.
     *  @return A pair (p,l), where p is the position of data following the
     *          serialized list of integers and l is the list of deserialized
     *          integers.
     */
    def intListFromBinary(ar: Array[Byte], pos: Int) = {
        val (pos1,length) = intFromBinary(ar,pos)
        intListFromBinaryAux(ar,pos1,length,List[Int]())
    }


    /**
     *  The auxiliary function, which deserializes a list of integers.
     *  @param ar  The byte array, which contains the integer list.
     *  @param pos The position of the integer list within the array.
     *  @param len The number of integers, which need to be deserialized.
     *  @param acc The partially constructed integer list.
     *  @return A pair (p,l), where p is the position of data following the
     *          serialized list of integers and l is the list of deserialized
     *          integers.
     */
    @tailrec
    private def intListFromBinaryAux(ar: Array[Byte], pos: Int,
                                      len: Int,acc: List[Int]): (Int,List[Int]) =
        len match {
            case 0 => (pos,acc)
            case x =>
                val (pos1,x) = intFromBinary(ar,pos)
                intListFromBinaryAux(ar, pos1, len - 1, x :: acc)
    }


    /**
     *  A serialized pair of integers.
     *  @param pair (x,y), where x and y are integers.
     *  @return A list of bytes containing the deserialized integers.
     */
    def iPairToBinary(pair: (Int,Int)) = {
        val (x,y) = pair
        intToBinary(x) ::: intToBinary(y)
    }


    /**
     *  A pair (p,z), where p is the position of data following the
     *  serialized pair of integers and z being a pair (x,y) of integers.
     *  @param ar  The byte array, which contains the pair of integers.
     *  @param pos The position of the pair within the byte array.
     *  @return A pair (p,z), where p is the position of data following the
     *          serialized pair of integers and z is the pair of deserialized
     *          integers.
     */
    def iPairFromBinary(ar: Array[Byte], pos: Int) = {
        val (pos1,v1) = intFromBinary(ar,pos)
        val (pos2,v2) = intFromBinary(ar,pos1)
        (pos2,(v1,v2))
    }


    /**
     *  The byte list containing the serialized list of integer pairs.
     *  @param pl The list of pairs of integers
     *  @return The byte list containing the serialized list of integer pairs,
     *          prefixed by the length of the list as serialized integer.
     */
    def iPairListToBinary(pl: List[(Int,Int)]): List[Byte] =
        iPairListToBinaryAux(pl,List[Byte](),0)


    /**
     *  The auxiliary function to serialize a list of integer pairs.
     *  @param pl     The list of integer pairs to be serialized.
     *  @param acc    The partially constucted list of bytes.
     *  @param length The number of already serialized pairs.
     *  @return The byte list containing the serialized list of integer pairs,
     *          prefixed by the length of the list as serialized integer pairs.
     */
    @tailrec
    private def iPairListToBinaryAux(pl: List[(Int,Int)], acc: List[Byte], length: Int): List[Byte] =
        pl match {
            case Nil => intToBinary(length) ::: acc
            case ((x,y)::xs) =>
                iPairListToBinaryAux(xs, iPairToBinary((x,y)) ::: acc, length + 1)
    }


    /**
     *  A pair (p,l), where p is the position of data following a list of
     *  integer pairs within the byte array and l is the deserialized list
     *  of integer pairs.
     *  @param ar  The byte array, which contains the list of pairs of integers.
     *  @param pos The position of the pair list within the byte array.
     *  @return A pair (p,l), where p is the position of data following the
     *          serialized list of integer pairs and l is the list of
     *          deserialized integer pairs.
     */
    def iPairListFromBinary(ar: Array[Byte], pos: Int) = {
        val (pos1,length) = intFromBinary(ar,pos)
        iPairListFromBinaryAux(ar, pos1, length, List[(Int,Int)]())
    }


    /**
     *  The auxiliary function to deserialize a list of integer pairs.
     *  @param ar  The byte array, which contains the list of pairs of integers.
     *  @param pos The position of the pair list within the byte array.
     *  @param len The number of pairs to deserialize.
     *  @param acc The partially constructed list of integer pairs.
     *  @return A pair (p,l), where p is the position of data following the
     *          serialized list of integer pairs and l is the list of
     *          deserialized integer pairs.
     */
    @tailrec
    private def iPairListFromBinaryAux(ar: Array[Byte], pos: Int, len: Int,
                                       acc: List[(Int,Int)]): (Int,List[(Int,Int)]) =
        len match {
            case 0 => (pos,acc)
            case x =>
                val (pos1,(x,y)) = iPairFromBinary(ar,pos)
                iPairListFromBinaryAux(ar, pos1, len - 1, (x,y) :: acc)
    }


    /**
     *  The serialized list of edges.
     *  @param el The list of edges, which need to be serialized.
     *  @return The list of bytes containing the serialized list of edges.
     */
    def edgeListToBinary(el: List[Edge]): List[Byte] =
        edgeListToBinaryAux(el,List[Byte](),0)


    /**
     *  The auxiliary function to serialize a list of edges.
     *  @param el     The list of edges, which need to be serialized.
     *  @param acc    The partially constructed list of bytes.
     *  @param length The number of already serialized edges.
     *  @return The list of bytes containing the serialized list of edges
     *          prefixed with the number of edges.
     */
    @tailrec
    private def edgeListToBinaryAux(el: List[Edge], acc: List[Byte], length: Int): List[Byte] =
        el match {
            case Nil => intToBinary(length) ::: acc
            case (Edge(v1,v2)::xs) =>
                edgeListToBinaryAux(xs, iPairToBinary((v1,v2)) ::: acc, length + 1)
    }


    /**
     *  The pair (p,l), where p is the position of data following a list of
     *  edges within the byte array and l is the list of deserialized edges.
     *  @param ar  The byte array containing the list of edges.
     *  @param pos The position of the edge list within the array.
     *  @return The pair (p,l), where p is the position of data following a
     *          list of edges within the byte array and l is the list of
     *          deserialized edges.
     */
    def edgeListFromBinary(ar: Array[Byte], pos: Int) = {
        val (pos1,length) = intFromBinary(ar,pos)
        edgeListFromBinaryAux(ar, pos1, length, List[Edge]())
    }


    /**
     *  The auxiliary function to deserialize a list of edges.
     *  @param ar  The byte array containing the list of edges.
     *  @param pos The position of the edge list within the array.
     *  @param len The number of edges to deserialize.
     *  @param acc The partially constructed list of edges.
     *  @return The pair (p,l), where p is the position of data following a
     *          list of edges within the byte array and l is the list of
     *          deserialized edges.
     */
    @tailrec
    private def edgeListFromBinaryAux(ar: Array[Byte], pos: Int,
                                      len: Int, acc: List[Edge]): (Int,List[Edge]) =
        len match {
            case 0 => (pos,acc)
            case x =>
                val (pos1,(x,y)) = iPairFromBinary(ar,pos)
                edgeListFromBinaryAux(ar, pos1, len - 1, Edge(x,y) :: acc)
    }


    /**
     *  The list of a serialized map of integer -> integer.
     *  @param m The map, which needs to be serialized.
     *  @return The list of bytes containing the serialized map.
     */
    def mapToBinary(m: Map[Int,Int]) = iPairListToBinary(m.toList)


    /**
     *  The pair (p,m), where p is the position of data following the
     *  serialized map and m is the deserialized map of integer -> integer.
     *  @param ar  The byte array containing the serialized map.
     *  @param pos The position of the map within the array.
     *  @return The pair (p,m), where p is the position of data following the
     *          serialized map and m is the deserialized
     *          map of integer -> integer.
     */
    def mapFromBinary(ar: Array[Byte], pos: Int) = {
        val (pos1,pl) = iPairListFromBinary(ar,pos)
        (pos1,Map() ++ pl)
    }


    /**
     *  The serialized representation of a utf-8 encoded string.
     *  @param str The string, which needs to be serialized.
     *  @return A list of bytes prefixed with the number of bytes containing
     *          the string information.
     */
    def stringToBinary(str: String) = {
        val bytes = str.getBytes
        intToBinary(bytes.length) ::: bytes.toList
    }


    /**
     *  The pair (p,s), where p is the position of data following the
     *  serialized string and s is the deserialized utf-8 string.
     *  @param ar  The byte array containing the serialized utf-8 string.
     *  @param pos The position of the string within the array.
     *  @return The pair (p,s), where p is the position of data following the
     *          serialized string and s is the deserialized utf-8 string.
     */
    def stringFromBinary(ar: Array[Byte], pos: Int) = {
        val (pos1,length) = intFromBinary(ar,pos)
        val str = new String(ar.slice(pos1, pos1 + length), "utf-8")
        (pos1 + length,str)
    }

}


/**
 *  The enumeration of supported message types, which can be serialized
 *  with the GCSerializer.
 */
object GCMsgType extends Enumeration {
  val GCGraph, GCSolution, PSAConfig, EA_Edges, EA_Solution, EA_Result = Value
}

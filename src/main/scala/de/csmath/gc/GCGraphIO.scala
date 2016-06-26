package de.csmath.gc

import scala.util._
import scala.io._
import scala.collection.immutable.TreeSet
import java.io.PrintStream
import de.csmath.graph._
import de.csmath.psa._


/**
 *  GCGraphIO provides the IO operations to read and write the files in the
 *  DIMACS standard format. While the input files describe a graph, the output
 *  files present the needed number of colors and provide the color assignment
 *  of each vertex.
 */
object GCGraphIO {

    /**
     *  Intermediate type of integer pairs, which represent an edge.
     */
    type IPair = (Int,Int)

    /**
     *  Intermediate type, of a pair (l,p) where l is the line number of
     *  the input file and p is an integer pair.
     */
    type LIPair = (Int,IPair)

    /**
     *  Intermediate type of a pair (l,s) where l is the regarded line number
     *  and s is the line of the input file.
     */
    type LString = (Int,String)


    /**
     *  A pair (g,c) wrapped in a Try monad. Here g denotes the graph of the
     *  coloring problem and c is the configuration of the sa algorithm. The
     *  graph is read from a file in the DIMACS standard format.
     *  @param config The configuration object which contains the file name.
     *  @return Success((g,c)) if the input file could be read successfully and
     *          contained a valid graph description. Failure(e) with exception
     *          e otherwise.
     */
    def readFile(config: PSAConfig): Try[(GCGraph,PSAConfig)] = {
        val fileName = config.inFileName
        val lineNo = Iterator.from(1)
        Try(Source.fromFile(fileName)) map {
            lineNo zip _.getLines
        } flatMap {
            toPairs
        } flatMap {
            createGraph
        } map {
            (_,config)
        }
    }


    /**
     *  This method's side effect is writing the computed solution into a
     *  file in the DIMACS standard.
     *  @param cs A pair (s,c) where s denotes a computed solution and c is
     *            the configuration object containing the file name.
     *  @return The provided pair (s,c).
     */
    def writeFile(cs: (GCSolution,PSAConfig)): Try[(Solution,PSAConfig)] = {
        val (solution,config) = cs
        val fileName = config.outFileName
        Try {
            Console.withOut(new PrintStream(fileName)) {
                println("s col " + solution.numColors)
                (1 to solution.colAssign.length, solution.colAssign)
                .zipped // pair nodes and colors
                .map( (x,y) => "l " + x + " " + (y + 1) )
                .foreach(println)
            }
        } map { _ => (solution,config) }
    }


    /**
     *  The predicate which is true iff the provided line is a comment.
     *  @param nline A pair (l,s) where l is a line number and s is a string.
     *  @return true iff the provided line is a comment.
     */
    private def commentLine(nline: LString) = nline._2.startsWith("c")


    /**
     *  Maps the input lines to integer pairs where the first pair contains
     *  the number of vertices and the number of edges and the following pairs
     *  denote the edges of the graph.
     *  @param lines An iterator for lines paired with the line number according
     *               to the type LString.
     *  @return Success(i) if the mapping was successful, Failure(e) otherwise.
     *          Here i denotes an iterator of integer pairs combined with the
     *          line number according to the type LIPair and e is an exception.
     */
    private def toPairs(lines: Iterator[LString]): Try[Iterator[LIPair]] = Try {
        lines filterNot commentLine map toPair map (_.get)
    }


    /**
     *  Maps a line to a pair of integers.
     *  @param nline A pair (l,s) where l is a line number and s a string.
     *  @return Success((l,p)) where l is a line number and p an integer pair
     *          if the string could not be mapped to a pair of integer,
     *          Failure(e) otherwise.
     */
    private def toPair(nline: LString): Try[LIPair] = {
        val ProblemLine = """p\s+edge\s+(\d+)\s+(\d+)""".r
        val EdgeLine = """e\s+(\d+)\s+(\d+)""".r
        nline match {
            case (lno, ProblemLine(x,y)) =>
                Success(lno,(x.toInt,y.toInt))
            case (lno, EdgeLine(x,y))=>
                Success(lno,(x.toInt - 1,y.toInt - 1))
            case (lno,x) =>
                Failure(FormatError("Wrong file format. Line " + lno))
        }
    }


    /**
     *  A graph wrapped in a Try monad.
     *  @param pairs An iterator of integer pairs, combined with the line number.
     *  @return Success(g) if the list of integer pairs could be mapped to
     *          a valid graph, Failure(e) otherwise.
     *          Here g denotes a graph and e an exception.
     */
    private def createGraph(pairs: Iterator[LIPair]): Try[GCGraph] = {
        val (_,(numVertices,_)) = pairs.next
        val edges = pairs map { pair =>
            val (lineNo, (v1,v2)) = pair
            if ( v1 >= 0 && v1 < numVertices &&
                v2 >= 0 && v2 < numVertices)
                Success(UndirectedEdge(v1,v2))
            else
                Failure(new InconsistentGraphException("Illegal Edge. Line " + lineNo))
        }
        Try(edges map { _.get }) match {
            case Success(p) =>
                val ordering: Ordering[Edge] =
                    Ordering[(Int,Int)].on(Edge.unapply(_).get)
                val edgeSet: Set[Edge] = new TreeSet()(ordering) ++ p
                Success(GCGraph(numVertices, edgeSet.toList))
            case Failure(x) => Failure(x)
        }
    }

}


/**
 *  A format error exception to denote an error in the input file.
 */
case class FormatError(desc: String) extends IllegalArgumentException(desc) {}

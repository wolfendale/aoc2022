import cats.implicits._

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.chaining.scalaUtilChainingOps

object Day12 {

  def part1(input: String): Int =
    Digraph.from(Day12Parser.parse(input))
      .shortestPath(Node.Start, Node.End).length - 1

  def part2(input: String): Int = {
    val graph = Digraph.from(Day12Parser.parse(input))
    graph.shortestPath(graph.nodes.filter(_.height == 0), Node.End).length - 1
  }
}

sealed trait Node extends Product with Serializable {
  val height: Int
}

object Node {

  case object Start extends Node { val height: Int = 0 }
  case object End extends Node { val height: Int = 25 }

  final case class Space(height: Int, id: UUID = UUID.randomUUID()) extends Node {

    override def toString: String = s"Space($height)"
  }
}

final case class Diedge(from: Node, to: Node)

final case class Digraph(edges: Vector[Diedge]) {

  lazy val nodes: Set[Node] =
    edges.flatMap(e => Vector(e.from, e.to)).toSet

  private val edgeMap: Map[Node, Set[Node]] =
    nodes.map { node =>
      node -> connectedNodes(node)
    }.toMap

  private def connectedNodes(node: Node): Set[Node] =
    edges.filter(_.from == node).map(_.to).toSet

  def connections(node: Node): Set[Node] =
    if (node == Node.End) Set.empty else edgeMap(node)

  def shortestPath(from: Set[Node], to: Node): Vector[Node] = {

    @tailrec
    def bfs(explored: Set[Node], queue: Queue[Vector[Node]]): Vector[Node] =
      if (queue.isEmpty) Vector.empty else {
        val (path, queue2) = queue.dequeue
        val node = path.last
        if (node == to) path else {
          val nextNodes = connections(node)
            .filter(!explored.contains(_))
          val queue3 = queue2.enqueueAll(nextNodes.map(path :+ _))
          bfs(explored + node ++ nextNodes, queue3)
        }
      }

    val explored = from.foldLeft(Queue.empty[Vector[Node]])(_ enqueue Vector(_))

    bfs(from, explored)
  }

  def shortestPath(from: Node, to: Node): Vector[Node] =
    shortestPath(Set(from), to)
}

object Digraph {

  def from(map: Vector[Vector[Node]]): Digraph = {

    def get(x: Int, y: Int): Option[Node] =
      map.lift(y).flatMap(_.lift(x))

    def getEdges(x: Int, y: Int, node: Node): Vector[Diedge] = {
      Vector(
        get(x - 1, y).map(Diedge(node, _)),
        get(x + 1, y).map(Diedge(node, _)),
        get(x, y - 1).map(Diedge(node, _)),
        get(x, y + 1).map(Diedge(node, _))
      ).flatten.filter(_.to.height <= node.height + 1)
    }

    map.zipWithIndex.foldLeft(Vector.empty[Diedge]) { case (edges, (row, y)) =>
      row.zipWithIndex.foldLeft(edges) { case (edges, (node, x)) =>
        edges ++ getEdges(x, y, node)
      }
    }.pipe(Digraph(_))
  }
}

object Day12Parser {

  import atto._
  import Atto._

  private val newline = char('\n') | char('\r')

  private val start = char('S').as(Node.Start)
  private val end = char('E').as(Node.End)
  private val space = lower.map(height => Node.Space(height.toInt - 97))
  private val node = start | end | space

  private val parser =
    many(node).map(_.toVector)
      .sepBy(newline).map(_.toVector)

  def parse(input: String): Vector[Vector[Node]] =
    parser.parseOnly(input).done.option.get
}

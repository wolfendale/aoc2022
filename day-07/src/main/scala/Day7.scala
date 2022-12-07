import cats.data._
import cats.implicits._

import scala.util.chaining.scalaUtilChainingOps

object Day7 {

  def part1(input: String): Long =
    Day7Parser.parse(input)
      .tap(println)
      .root
      .directories
      .map(_.size)
      .filter(_ <= 100000)
      .sum

  def part2(input: String): Long = {
    val console = Day7Parser.parse(input)
    val usedSpace = console.root.size
    val freeSpace = 70000000L - usedSpace
    val minSpaceRequired = 30000000L - freeSpace
    console
      .root
      .directories
      .map(_.size)
      .filter(_ >= minSpaceRequired)
      .min
  }
}

sealed trait File extends Product with Serializable {
  def name: String
  def size: Long
}

object File {

  final case class Directory(name: String, files: Vector[File]) extends File {

    lazy val size: Long = files.map(_.size).sum

    def set(path: Vector[String], newFiles: Vector[File]): Directory =
      if (path.isEmpty) {
        this
      } else if (path.head == name && path.size == 1) {
        copy(files = newFiles)
      } else {
        copy(files = files.map {
          case d: File.Directory if d.name == path.tail.head =>
            d.set(path.tail, newFiles)
          case f => f
        })
      }

    def directories: Vector[File.Directory] =
      this +: files.collect { case d: File.Directory =>
        d.directories
      }.flatten

    override def toString: String =
      Colours.white(s"- $name (dir, size=${Colours.rated(size)})" +
        files.flatMap(_.toString.split("\r\n"))
          .mkString("\r\n\t", "\r\n\t", ""))
  }

  final case class Document(name: String, size: Long) extends File {

    override def toString: String =
      Colours.white(s"- $name (file, size=$size)")
  }
}

final case class Console(root: File.Directory, pwd: Vector[String]) {

  def cd(directory: String): Console =
    if (directory == "..") {
      copy(pwd = pwd.init)
    } else {
      copy(pwd = pwd :+ directory)
    }

  def set(files: Vector[File]): Console =
    copy(root = root.set(pwd, files))

  override def toString: String =
    Colours.white(s"current dir: ${pwd.tail.mkString("/", "/", "")}\n\r$root")
}

object Console {

  val default: Console = Console(root = File.Directory("/", Vector.empty), pwd = Vector.empty)
}

object Day7Parser {

  import atto._
  import Atto._

  private val commandStart: Parser[Unit] =
    token(char('$')).void

  private val newline: Parser[Char] = char('\r') | char('\n')

  private val fileName: Parser[String] =
    stringOf1(letter | char('.')) | string("/")

  private val cd: Parser[String] =
    commandStart ~> token(string("cd")) ~> fileName

  private val ls: Parser[Unit] =
    commandStart ~> string("ls").void

  private val document: Parser[File.Document] =
    (token(int), fileName).mapN((size, name) => File.Document(name, size))

  private val directory: Parser[File.Directory] =
    token(string("dir")) ~> fileName.map(File.Directory(_, Vector.empty))

  private val file: Parser[File] =
    document | directory

  private val cdCommand: Parser[State[Console, Unit]] =
    cd.map { directory =>
      State.modify[Console](_.cd(directory))
    }

  private val lsCommand: Parser[State[Console, Unit]] =
    (ls ~> newline ~> file.sepBy(newline)).map { items =>
      State.modify[Console](_.set(items.toVector))
    }

  private val parser: Parser[Console] =
    (cdCommand | lsCommand).sepBy1(newline).map { commands =>
      commands.reduceLeft(_ >> _).runS(Console.default).value
    }

  def parse(input: String): Console =
    parser.parseOnly(input).option.get
}

object Colours {

  def white(s: String): String = scala.Console.WHITE + s
  def green(s: String): String = scala.Console.GREEN + s
  def yellow(s: String): String = scala.Console.YELLOW + s + scala.Console.WHITE
  def red(s: String): String = scala.Console.RED + s + scala.Console.WHITE

  def rated(i: Long): String = {
    if (i > 100000) red(i.toString)
    else if (i > 50000) yellow(i.toString)
    else green(i.toString)
  }
}
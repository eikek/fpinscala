package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] = {
    val x = map2(p, many(p)) { (a, la) => a :: la }
    val z = succeed(List[A]())
    or(x, z)
  }

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(x => succeed(f(x)))

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def numChars(c: Char): Parser[String] = map("[0-9]+".r)(_.toInt).flatMap { i =>
    listOfN(i, char(c)).map(_.mkString)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    def loop(n: Int, rp: Parser[List[A]]): Parser[List[A]] =
      if (n == 0) rp
      else loop(n - 1, map2(p, rp){_ :: _})

    loop(n, succeed(List()))
  }    

  def numA: Parser[Int] = char('a').many.map(_.size)

  def succeed[A](a: A) = string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p)) { (a, la) => a :: la }

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a,b)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice = self.slice(p)
    def **[B](p2: Parser[B]) = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in) {s => run(p1)(s) == run(p2)(s) }

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](as: Gen[A], in: Gen[String]): Prop =
      forAll(in) { s =>
        run(succeed(1))(s) == Right(1)
      }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}


trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val spaces = char(' ').many.slice
    val digits = regex("[0-9]+".r).map(_.toInt)
    val double = regex("[0-9]+\\.[0-9]+".r).map(_.toDouble)

    val jnum = double.map(JNumber.apply)
    val jstr = (string("\"") ** regex("[^\"]+".r) ** string("\"")).map {
      case ((_, s), _) => JString(s)
    }
    val jbool = or(string("true"), string("false")).map { s => JBool(s.toBoolean) }

    def jarr = (string("[") ** many(jsonParser(P)) ** string("]")).map {
      case (("_", ljs), "_") => JArray(ljs.toVector)
    }

    ???
  }
}



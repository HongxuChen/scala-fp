package cats

import cats.syntax.either._

object EitherStyle {
  def main(args: Array[String]): Unit = {
    magic("123") match {
      case Left(_: NumberFormatException) => println("not a number!")
      case Left(_: IllegalArgumentException) =>
        println("can't take reciprocal of 0!")
      case Left(_) => println("got unknown exception")
      case Right(s) => println(s"Got reciprocal: ${s}")
    }
  }

  def magic(s: String): Either[Exception, String] = {
    for {
      p <- parse(s)
      r <- reciprocal(p)
    } yield stringify(r)
  }

  def parse(s: String): Either[Exception, Int] =
    if (s.matches("-?[0-9]+")) {
      Either.right(s.toInt)
    } else {
      Either.left(new NumberFormatException(s"${s} is not a valid integer."))
    }

  def reciprocal(i: Int): Either[Exception, Double] =
    if (i == 0) {
      Either.left(new IllegalArgumentException("Cannot take reciprocal of 0."))
    } else {
      Either.right(1.0 / i)
    }

  def stringify(d: Double): String = d.toString
}

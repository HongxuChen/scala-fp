package cats

object Num {

  def sum[A](xs: List[A])(implicit tc: Monoid[A]): A =
    (tc.empty /: xs)((x: A, y: A) => tc.combine(x, y))

  implicit object IntM extends Monoid[Int] {
    override def combine(x: Int, y: Int): Int = x + y

    override def empty: Int = 0
  }

  implicit object DoubleM extends Monoid[Double] {
    override def empty: Double = 0.0

    override def combine(x: Double, y: Double): Double = x + y
  }

  implicit object StringM extends Monoid[String] {
    override def empty: String = ""

    override def combine(x: String, y: String): String = x + y
  }

}

object TypeClass extends App {

  import Num._

  println(sum(List(2, 3, 4)))
  println(sum(List(2.0, 3.0, 4.3)))
  println(sum(List("good", "morning", "sir")))

}

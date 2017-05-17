package shapeless

object GEncoder extends App {

  val shapes: List[Shape] =
    List(
      Rectangle(1, 2),
      Circle(3),
      Rectangle(4, 5),
      Circle(6)
    )
  val optShapes: List[Option[Shape]] =
    List(
      Some(Rectangle(1, 2)),
      Some(Circle(3)),
      None,
      Some(Rectangle(4, 5)),
      Some(Circle(6)),
      None
    )

  def writeCsv[A](vs: List[A])(implicit enc: CsvEncoder[A]): String =
    vs.map(v => enc.encode(v).mkString(",")).mkString("\n")

  import CsvEncoder._

  sealed trait Shape

  trait CsvEncoder[A] {
    def w: Int

    def encode(value: A): List[String]
  }

  final case class Rectangle(width: Double, height: Double) extends Shape

  final case class Circle(radius: Double) extends Shape

  object CsvEncoder {
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def pure[A](width: Int)(func: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        override def encode(value: A): List[String] = func(value)

        override def w: Int = width
      }

    implicit val intEncoder: CsvEncoder[Int] = pure(1)(i => List(i.toString))
    implicit val DoubleEncoder: CsvEncoder[Double] =
      pure(1)(d => List(d.toString))
    implicit val booleanEncoder: CsvEncoder[Boolean] =
      pure(1)(b => if (b) List("yes") else List("no"))

    implicit def optionEncoder[A](
        implicit enc: CsvEncoder[A]): CsvEncoder[Option[A]] =
      pure(1)(opt => opt.map(enc.encode).getOrElse(List.fill(enc.w)("")))

    implicit val hnilEncoder: CsvEncoder[HNil] = pure(0)(hnil => Nil)

    implicit def hlistEncoder[H, T <: HList](
        implicit hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] =
      pure(hEncoder.value.w + tEncoder.w) {
        case h :: t =>
          hEncoder.value.encode(h) ++ tEncoder.encode(t)
      }

    implicit val cnilEncoder: CsvEncoder[CNil] = pure(0)(cnil => ???)

    implicit def coproductEncoder[H, T <: Coproduct](
        implicit hEncoder: Lazy[CsvEncoder[H]],
        tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T] =
      pure(hEncoder.value.w + tEncoder.w) {
        case Inl(h) => hEncoder.value.encode(h) ++ List.fill(tEncoder.w)("")
        case Inr(t) => List.fill(hEncoder.value.w)("") ++ tEncoder.encode(t)
      }

    implicit def genericEncoder[A, R](
        implicit gen: Generic.Aux[A, R],
        enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
      pure(enc.value.w)(a => enc.value.encode(gen.to(a)))

  }

  println("Shapes " + shapes)
  println("Shapes as CSV:\n" + writeCsv(shapes))
  println("Optional shapes " + optShapes)
  println("Optional shapes as CSV:\n" + writeCsv(optShapes))

}

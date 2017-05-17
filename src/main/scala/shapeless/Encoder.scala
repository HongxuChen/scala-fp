package shapeless

object Encoder extends App {

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {
    def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
      values.map(v => enc.encode(v).mkString(",")).mkString("\n")
  }

  object EncodeEmployee {

    case class Employee(name: String, number: Int, manager: Boolean)

    implicit val employeeEncoder: CsvEncoder[Employee] =
      new CsvEncoder[Employee] {
        override def encode(e: Employee) =
          List(e.name, e.number.toString, if (e.manager) "yes" else "no")
      }

  }

  object EncodeCream {

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    implicit val creamEncoder: CsvEncoder[IceCream] =
      new CsvEncoder[IceCream] {
        override def encode(e: IceCream) =
          List(e.name, e.numCherries.toString, if (e.inCone) " yes" else "no")
      }

  }

  object EncodePair {
    implicit def pairEncode[A, B](
        implicit aEncoder: CsvEncoder[A],
        bEncoder: CsvEncoder[B]): CsvEncoder[(A, B)] = new CsvEncoder[(A, B)] {
      override def encode(pair: (A, B)): List[String] = {
        val (a, b) = pair
        aEncoder.encode(a) ++ bEncoder.encode(b)
      }
    }
  }

  import CsvEncoder._
  import EncodeCream._
  import EncodeEmployee._
  import EncodePair._

  val employees: List[Employee] = List(
    Employee("Bill", 1, true),
    Employee("Peter", 2, false),
    Employee("Milton", 3, false)
  )

  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  println(writeCsv(employees))
  println(writeCsv(iceCreams))
  println(writeCsv(employees zip iceCreams))

}

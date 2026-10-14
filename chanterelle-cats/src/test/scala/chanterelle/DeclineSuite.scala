package chanterelle

import munit.*
import com.monovore.decline.Opts
import scala.compiletime.*
import scala.Tuple.Zip


class DeclineSuite extends FunSuite {

  val out = (
    port = Opts.option[Int](_, "THE port"),
    url = Opts.option[String](_, "THE url"),
    password = Opts.option[String](_, "THE password"),
    username = Opts.option[String](_, "THE username")
  )

  inline def namesOf[A <: NamedTuple.AnyNamedTuple](tup: A) = {
    constValueTuple[NamedTuple.Names[A]]
  }

  type Names = NamedTuple.Names[out.type]

  type ApplyName[A] = A match { 
    case (String, String => a) => a 
}


  inline def applyName[A](value: A): ApplyName[A] =
    value match {
      case a: (String, String => arg) @unchecked => a._2(a._1)
    }

  applyName(1) // WHY DO YOU COMPILE???????????????????

  inline def applyNames[N <: Tuple, V <: Tuple](named: NamedTuple.NamedTuple[N, V]): NamedTuple.NamedTuple[N, Tuple.Map[Zip[N, V], ApplyName]] =
    namesOf(named).zip(named).map[ApplyName]([a] => applyName(_))

  // val names = applyNames(out)

  val cos = applyNames(
    port = Opts.option[Int](_, "THE port"),
    url = Opts.option[String](_, "THE url"),
    password = Opts.option[String](_, "THE password"),
    username = Opts.option[String](_, "THE username")
  )



  test("costam") {
    Mode.applicative[Opts] {
      val out = (
        port = Opts.option[Int]("port", "THE port"),
        url = Opts.option[String]("url", "THE url"),
        password = Opts.option[String]("password", "THE password"),
        username = Opts.option[String]("username", "THE username")
      ).transform(_.hoist.local)
    }
    println("TEST")
  }

}

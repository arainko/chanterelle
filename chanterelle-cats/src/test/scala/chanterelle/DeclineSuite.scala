package chanterelle

import munit.*
import com.monovore.decline.Opts
import scala.compiletime.*
import scala.Tuple.Zip
import chanterelle.interop.*
import cats.data.NonEmptyList
import cats.data.Chain

class DeclineSuite extends FunSuite {

  test("Nel") {
    val c = 
    Collection.Builder.from(List)
      .transform(NonEmptyList.fromList)
      .fallible
      .transformK([a] => _.toRight("costam"))
    
    val chain = Collection.Builder.fromAppendable(Chain.empty, _.append(_))
  }

  test("costam") {
    Mode.applicative[Opts] {
      chanterelle.internal.Logger.locally:
        val out: Opts[(port: Int, url: String, password: String, username: String)] = (
          port = Opts.option[Int]("port", "THE port"),
          url = Opts.option[String]("url", "THE url"),
          password = Opts.option[String]("password", "THE password"),
          username = Opts.option[String]("username", "THE username")
        ).transform(_.hoist)
    }
  }

}

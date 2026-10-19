package chanterelle

import munit.*
import com.monovore.decline.Opts
import scala.compiletime.*
import scala.Tuple.Zip

class DeclineSuite extends FunSuite {
  test("costam") {
    Mode.applicative[Opts] {
      chanterelle.internal.Logger.locally:
        val out: Opts[(port : Int, url : String, password : String, username : String)] = (
          port = Opts.option[Int]("port", "THE port"),
          url = Opts.option[String]("url", "THE url"),
          password = Opts.option[String]("password", "THE password"),
          username = Opts.option[String]("username", "THE username")
        ).transform(_.hoist)
    }
  }

}

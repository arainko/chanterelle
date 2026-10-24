package chanterelle

import munit.*
import com.monovore.decline.Opts
import scala.compiletime.*
import scala.Tuple.Zip
import chanterelle.interop.*
import cats.data.NonEmptyList
import cats.data.Chain
import scala.collection.immutable.SortedMap
import cats.data.NonEmptyMap
import chanterelle.interop.Collection.Builder.Fallible
import cats.data.NonEmptyMapImpl

class DeclineSuite extends FunSuite {

  test("Nel") {
    val nel = 
    Collection.Builder.from(List)
      .transform(NonEmptyList.fromList)
      .fallible
      .transformK([a] => _.toRight("costam"))
    val nelIntoIter =
      Collection.IntoIterator.from[Int, NonEmptyList[Int]](_.iterator)
   
    given nem[K: Ordering, V]: Collection.Builder.Fallible[Option, (K, V), NonEmptyMap[K, V]] = 
      Collection.Builder.from[(K, V), SortedMap[K, V]](SortedMap).transform(NonEmptyMap.fromMap).fallible

    val nemIntoIter = Collection.IntoIterator.from[(Int, Int), NonEmptyMap[Int, Int]](_.toSortedMap.iterator)

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

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

  given nelBuilder[A]: Fallible[[a] =>> Either[String, a], A, NonEmptyList[A]] =
    Collection.Builder
      .from[List[A]]
      .transform(NonEmptyList.fromList)
      .fallible
      .transformK([a] => _.toRight("costam"))

  given nelIntoIter[A]: Collection.IntoIterator[A, NonEmptyList[A]] =
    Collection.IntoIterator.from(_.iterator)

  given nem[K: Ordering, V]: Collection.Builder.Fallible[Option, (K, V), NonEmptyMap[K, V]] =
    Collection.Builder.from[SortedMap[K, V]].transform(NonEmptyMap.fromMap).fallible

  given nemIntoIter[K, V]: Collection.IntoIterator[(K, V), NonEmptyMap[K, V]] =
    Collection.IntoIterator.from(_.toSortedMap.iterator)

  given chainBuilder[A]: Collection.Builder[A, Chain[A]] = Collection.Builder.fromAppendable(Chain.empty, _.append(_))
  given chainIntoIter[A]: Collection.IntoIterator[A, Chain[A]] = Collection.IntoIterator.from(_.iterator)

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

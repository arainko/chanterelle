package chanterelle

import chanterelle.interop.Collection.Builder
import chanterelle.interop.Collection.Builder.Fallible
import chanterelle.interop.{ Collection, Mappable }

/**
 * `Mode` is a capability that provides the ability to 'bubble up' nested wrapped values (i.e. ones wrapped in an `F`) up to the top, for example:
 * {{{
 * import chanterelle.*
 *
 * Mode.FailFast.option {
 *   // evaluates to Some((field1 = 1, field2 = 2, field3 = 3))
 *   (field1 = Some(1), field2 = Some(2), field3 = Some(3)).transform(_.hoist)
 * }
 * }}}
 *
 * `Modes` come in 3 flavors:
 * * `Accumulating` - the least powerful, able to model error accumulation but the the whole structure needs to be known from the get go (in other words, it can't hoist nested fallible fields like `F[F[Int]]`),
 * * `FailFast` - the intermediate in terms of power - can't model error accumulation, but can hoist nested fallible fields,
 * * `Parallel` - the most powerful, a mix of both `Accumulating` and `FailFast`. It can model both error accumulation AND hoisting of nested fallible fields.
 *
 * Note that a `Mode` is not really a typeclass since there isn't really a single instance for each F (for example, instances for `Either` could come in all sorts of shapes - sometimes it's best to use `FailFast`, other times `Accumulating`).
 * `Modes` are usually provided at the callsite with their `.apply` method that puts them in the implicit scope of a block of code (just like in the example above).
 */
sealed trait Mode[F[_]] extends Mappable[F] { self =>
  type Builder[Elem, Collection] <: Collection.Builder[Elem, Collection] | Collection.Builder.Fallible[F, Elem, Collection]

  def pure[A](value: A): F[A]

  def traverseCollection[AElem, BElem, AColl, BColl](
    collection: AColl,
    transformation: AElem => F[BElem]
  )(using Collection.IntoIterator[AElem, AColl], Builder[BElem, BColl]): F[BColl]
}

object Mode {

  /**
   * The least powerful of all modes - it's able to model error accumulation and it's only able to hoist the 'toplevel' level of wrappers.
   * This also means that its `traverseCollection` method is not able to function with fallible builders (since that'd entail that it's able to flatten a shape of `F[F[A]]` into `F[A]`).
   *
   * Example:
   * {{{
   * import chanterelle.*
   *
   * Mode.Accumulating.either[List[String]] {
   *    // evaluates to Left(List("err1", "err2", "err3"))
   *   (field1 = Right(1), field2 = Left(List("err1", "err2")), field3 = Left(List("err3"))).transform(_.hoist)
   *
   * }
   * }}}
   *
   * At the same time, trying to use it as such will fail to compile:
   * {{{
   * import chanterelle.*
   *
   * Mode.Accumulating.either[List[String]] {
   *   (field1 = Right(Right(1))).transform(_.hoist(_.field1.element)) // error: It's not possible to hoist nested fallible fields with just a Mode.Accumulating
   * }
   * }}}
   */
  trait Accumulating[F[_]] extends Mode[F] {
    final type Builder[Elem, Collection] = Collection.Builder[Elem, Collection]

    def zip[A, B](left: F[A], right: F[B]): F[(A, B)]
  }

  /**
   * This one can model fail fast transformations and hoisting of nested fallible fields, but it's not really possible to accumulate errors with it.
   *
   * Example:
   * {{{
   * import chanterelle.*
   *
   * Mode.FailFast.option {
   *   // evaluates to Some((field1 = 1, field2 = 2, field3 = 3))
   *   (field1 = Some(1), field2 = Some(2), field3 = Some(3)).transform(_.hoist)
   *
   *  // it's also possible to hoist nested fields
   *  (field1 = Some(Some(1))).transform(_.hoist(_.field1.element)) // evaluates to Some((field1 = 1))
   * }
   * }}}
   */
  trait FailFast[F[_]] extends Mode[F] {
    final type Builder[Elem, Collection] = Collection.Builder.Fallible[F, Elem, Collection]

    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  }

  /**
   * A mix of both `Mode.Accumulating` and `Mode.FailFast` - it's able to both accumulate errors and do nested fallible transformations.
   *
   * Example:
   *  {{{
   * import chanterelle.*
   *
   * Mode.Parallel.either[List[String]] {
   *    // evaluates to Left(List("err1", "err2", "err3"))
   *   (field1 = Right(1), field2 = Left(List("err1", "err2")), field3 = Left(List("err3"))).transform(_.hoist)
   *
   *   // hoisting nested fields also works
   *   (field1 = Right(Right(1))).transform(_.hoist(_.field1.element)) // evaluates to Right((field1 = 1))
   * }
   * }}}
   */
  trait Parallel[F[_]] extends Mode[F] {
    final type Builder[Elem, Collection] = Collection.Builder.Fallible[F, Elem, Collection]

    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
    def zip[A, B](left: F[A], right: F[B]): F[(A, B)]
  }

  extension [F[_], M <: Mode[F]](self: M) {
    inline def apply[A](inline f: M ?=> A): A = f(using self)
  }

  object Accumulating {

    /**
     * An instance of `Mode.Accumulating` for `Either` that collects errors (where the type param of `Errors` is the left side of the `Either`), example:
     * {{{
     * import chanterelle.*
     *
     * Mode.Accumulating.either[List[String]] {
     *   // when everything's fine and dandy: evaluates to Right((field1 = 1, field2 = 2, field3 = 3))
     *   (field1 = Right(1), field2 = Right(2), field3 = Right(3)).transform(_.hoist)
     *
     *   // in case of errors: evaluates to Left(List("err1", "err2", "err3"))
     *   (field1 = Right(1), field2 = Left(List("err1", "err2")), field3 = Left(List("err3"))).transform(_.hoist)
     * }
     * }}}
     *
     * Providing the `Errors` type param is usually enough to infer `E` by itself.
     */
    def either[Errors](using
      DummyImplicit
    )[E](using
      IntoIter: Collection.IntoIterator[E, Errors],
      Errors: Collection.Builder[E, Errors]
    ): Mode.Accumulating[scala.Either[Errors, _]] = Either[E, Errors]

    private final class Either[E, Errors](using
      IntoIter: Collection.IntoIterator[E, Errors],
      Errors: Collection.Builder[E, Errors]
    ) extends Mode.Accumulating[scala.Either[Errors, _]] {

      override def pure[A](value: A): scala.Either[Errors, A] = Right(value)

      override def map[A, B](fa: scala.Either[Errors, A], f: A => B): scala.Either[Errors, B] = fa.map(f)

      override def zip[A, B](fa: scala.Either[Errors, A], fb: scala.Either[Errors, B]): scala.Either[Errors, (A, B)] =
        (fa, fb) match {
          case (Right(a), Right(b))           => Right(a -> b)
          case (Right(_), err @ Left(_))      => err.asInstanceOf[scala.Either[Errors, (A, B)]]
          case (err @ Left(_), Right(_))      => err.asInstanceOf[scala.Either[Errors, (A, B)]]
          case (Left(errorsA), Left(errorsB)) =>
            val builder = Errors.newBuilder
            val accumulated = builder ++= IntoIter.iterator(errorsA) ++= IntoIter.iterator(errorsB)
            Left(accumulated.result())
        }

      // Inspired by chimney's implementation: https://github.com/scalalandio/chimney/blob/53125c0a55479763157909ef920e11f5b487b182/chimney/src/main/scala/io/scalaland/chimney/TransformerFSupport.scala#L153
      override def traverseCollection[A, B, AColl, BColl](
        collection: AColl,
        transformation: A => scala.Either[Errors, B]
      )(using
        AColl: Collection.IntoIterator[A, AColl],
        BColl: Collection.Builder[B, BColl]
      ): scala.Either[Errors, BColl] = {
        val accumulatedErrors = Errors.newBuilder
        val accumulatedSuccesses = BColl.newBuilder
        var isErroredOut = false
        val iter = AColl.iterator(collection)

        while iter.hasNext do {
          val elem = iter.next()
          transformation(elem) match {
            case Left(errors) =>
              accumulatedErrors.addAll(IntoIter.iterator(errors))
              if isErroredOut == false then {
                isErroredOut = true
                accumulatedSuccesses.clear()
              }
            case Right(value) =>
              if isErroredOut == false then {
                accumulatedSuccesses.addOne(value)
              }
          }
        }

        if isErroredOut then Left(accumulatedErrors.result()) else Right(accumulatedSuccesses.result())
      }
    }
  }

  object Parallel {

    /**
     * An instance of `Mode.Parallel` for `Either` that collects errors (where the type param of `Errors` is the left side of the `Either`), example:
     * {{{
     * import chanterelle.*
     *
     * Mode.Parallel.either[List[String]] {
     *   // when everything's fine and dandy: evaluates to Right((field1 = 1, field2 = 2, field3 = 3))
     *   (field1 = Right(1), field2 = Right(2), field3 = Right(3)).transform(_.hoist)
     *
     *   // when there are errors: evaluates to Left(List("err1", "err2", "err3"))
     *   (field1 = Right(1), field2 = Left(List("err1", "err2")), field3 = Left(List("err3"))).transform(_.hoist)
     * }
     * }}}
     *
     * Providing the `Errors` type param is usually enough to infer `E` by itself.
     */
    def either[Errors](using
      DummyImplicit
    )[E](using
      Collection.IntoIterator[E, Errors],
      Collection.Builder[E, Errors]
    ): Mode.Parallel[scala.Either[Errors, _]] =
      Either[E, Errors]

    private final class Either[E, Errors](using
      IntoIter: Collection.IntoIterator[E, Errors],
      Errors: Collection.Builder[E, Errors]
    ) extends Mode.Parallel[scala.Either[Errors, _]] {

      override def flatMap[A, B](fa: scala.Either[Errors, A], f: A => scala.Either[Errors, B]): scala.Either[Errors, B] =
        fa.flatMap(f)

      override def pure[A](value: A): scala.Either[Errors, A] = Right(value)

      override def map[A, B](fa: scala.Either[Errors, A], f: A => B): scala.Either[Errors, B] = fa.map(f)

      override def zip[A, B](fa: scala.Either[Errors, A], fb: scala.Either[Errors, B]): scala.Either[Errors, (A, B)] =
        (fa, fb) match {
          case (Right(a), Right(b))           => Right(a -> b)
          case (Right(_), err @ Left(_))      => err.asInstanceOf[scala.Either[Errors, (A, B)]]
          case (err @ Left(_), Right(_))      => err.asInstanceOf[scala.Either[Errors, (A, B)]]
          case (Left(errorsA), Left(errorsB)) =>
            val builder = Errors.newBuilder
            val accumulated = builder ++= IntoIter.iterator(errorsA) ++= IntoIter.iterator(errorsB)
            Left(accumulated.result())
        }

      // Inspired by chimney's implementation: https://github.com/scalalandio/chimney/blob/53125c0a55479763157909ef920e11f5b487b182/chimney/src/main/scala/io/scalaland/chimney/TransformerFSupport.scala#L153
      override def traverseCollection[A, B, AColl, BColl](
        collection: AColl,
        transformation: A => scala.Either[Errors, B]
      )(using
        AColl: Collection.IntoIterator[A, AColl],
        BColl: Collection.Builder.Fallible[scala.Either[Errors, _], B, BColl]
      ): scala.Either[Errors, BColl] = {
        val accumulatedErrors = Errors.newBuilder
        val accumulatedSuccesses = BColl.newBuilder
        var isErroredOut = false
        val iter = AColl.iterator(collection)

        while iter.hasNext do {
          val elem = iter.next()
          transformation(elem) match {
            case Left(errors) =>
              accumulatedErrors.addAll(IntoIter.iterator(errors))
              if isErroredOut == false then {
                isErroredOut = true
                accumulatedSuccesses.clear()
              }
            case Right(value) =>
              if isErroredOut == false then {
                accumulatedSuccesses.addOne(value)
              }
          }
        }

        if isErroredOut then Left(accumulatedErrors.result()) else accumulatedSuccesses.result()
      }
    }
  }

  object FailFast {

    /**
     * An instance of `Mode.FailFast` for `Either` that fails fast at the first error it encounters (where the type param `E` is the left side of the `Either`), example:
     * {{{
     * import chanterelle.*
     *
     * Mode.FailFast.either[String] {
     *   // when everything's fine and dandy: evaluates to Right((field1 = 1, field2 = 2, field3 = 3))
     *   (field1 = Right(1), field2 = Right(2), field3 = Right(3)).transform(_.hoist)
     *
     *   // when there are errors: evaluates to Left("err1")
     *   (field1 = Right(1), field2 = Left("err1"), field3 = Left("err2")).transform(_.hoist)
     * }
     * }}}
     */
    def either[E]: Mode.FailFast[scala.Either[E, _]] =
      eitherInstance.asInstanceOf[Mode.FailFast[scala.Either[E, _]]]

    private val eitherInstance = new Either[Any]

    private final class Either[E] extends Mode.FailFast[scala.Either[E, _]] {
      final def pure[A](value: A): scala.Either[E, A] = Right(value)

      final def map[A, B](fa: scala.Either[E, A], f: A => B): scala.Either[E, B] = fa.map(f)

      final def flatMap[A, B](fa: scala.Either[E, A], f: A => scala.Either[E, B]): scala.Either[E, B] = fa.flatMap(f)

      final def traverseCollection[A, B, AColl, BColl](
        collection: AColl,
        transformation: A => scala.Either[E, B]
      )(using
        AColl: Collection.IntoIterator[A, AColl],
        BColl: Collection.Builder.Fallible[scala.Either[E, _], B, BColl]
      ): scala.Either[E, BColl] = {
        var error: Left[E, Nothing] = null
        def isErroredOut = !(error eq null)

        val resultBuilder = BColl.newBuilder
        val iterator = AColl.iterator(collection)
        while iterator.hasNext && !isErroredOut do {
          transformation(iterator.next()) match {
            case err @ Left(_) =>
              error = err.asInstanceOf[Left[E, Nothing]]
              resultBuilder.clear()
            case Right(value) =>
              resultBuilder += value
          }
        }

        if isErroredOut then error else resultBuilder.result()
      }

    }

     /**
     * An instance of `Mode.FailFast` for `Option` that fails fast with a `None` once it encounters one.
     * {{{
     * import chanterelle.*
     *
     * Mode.FailFast.option {
     *   // when everything's fine and dandy: evaluates to Some((field1 = 1, field2 = 2, field3 = 3))
     *   (field1 = Some(1), field2 = Some(2), field3 = Some(3)).transform(_.hoist)
     *
     *   // when there are errors: evaluates to None
     *   (field1 = Right(1), field2 = None, field3 = Some(3)).transform(_.hoist)
     * }
     * }}}
     */

    val option: Mode.FailFast[scala.Option] = Option

    private object Option extends Mode.FailFast[scala.Option] {

      final def pure[A](value: A): scala.Option[A] = Some(value)

      final def map[A, B](fa: scala.Option[A], f: A => B): scala.Option[B] = fa.map(f)

      final def flatMap[A, B](fa: scala.Option[A], f: A => scala.Option[B]): scala.Option[B] = fa.flatMap(f)

      final def traverseCollection[A, B, AColl, BColl](
        collection: AColl,
        transformation: A => scala.Option[B]
      )(using
        AColl: Collection.IntoIterator[A, AColl],
        BColl: Collection.Builder.Fallible[Option, B, BColl]
      ): scala.Option[BColl] = {
        var isErroredOut = false
        val resultBuilder = BColl.newBuilder
        val iterator = AColl.iterator(collection)
        while iterator.hasNext && !isErroredOut do {
          transformation(iterator.next()) match {
            case None =>
              isErroredOut = true
              resultBuilder.clear()
            case Some(value) =>
              resultBuilder += value
          }
        }

        if isErroredOut then None else resultBuilder.result()
      }

    }
  }
}

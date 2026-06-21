package chanterelle.internal

import scala.quoted.*

extension (tpe: Type[? <: AnyKind]) {
  def assertBoundedBy[A: Type as A](using Quotes): Type[? <: A] =
    tpe match {
      case tpe @ '[A] => tpe
      case _ => quotes.reflect.report.errorAndAbort(s"${Type.show(using tpe)} is not bounded by ${Type.show[A]}. This is a bug in chanterelle.")
    }

  private[chanterelle] def fullName(using Quotes): String = {
    import quotes.reflect.*

    TypeRepr.of(using tpe).show(using Printer.TypeReprCode)
  }

  private[chanterelle] def repr(using Quotes): quotes.reflect.TypeRepr =
    quotes.reflect.TypeRepr.of(using tpe)
}

extension [A, B](self: Either[A, B]) {
  private[chanterelle] inline def zipRight[AA >: A, C](inline that: Either[AA, C]): Either[AA, C] =
    self.flatMap(_ => that)

  private[chanterelle] def leftMap[C](f: A => C): Either[C, B] = self.left.map(f)
}

extension [A](self: A | None.type) {
  private[chanterelle] inline def getOrElse[AA >: A](inline fallback: AA): AA =
    self.fold(fallback, a => a)

  private[chanterelle] inline def fold[B](inline caseNone: B, inline caseA: A => B): B =
    self match
      case None => caseNone
      case a: A => caseA(a)
}

extension [A](self: Option[A]) {
  private[chanterelle] def asUnion: A | None.type =
    self match
      case None        => None
      case Some(value) => value

}

extension [A](self: List[A]) {
  private[chanterelle] def parTraverse[E, B](f: A => Either[E, B]): Either[::[E], List[B]] = {
    self.partitionMap(f) match {
      case (errs @ ::(_, _), _) => Left(errs)
      case (_, values)          => Right(values)
    }

  }
}

private[chanterelle] type None = None.type

private[chanterelle] inline def when[A](using DummyImplicit)[B](inline f: A => B) = f

extension (self: Type.type) {
  private[chanterelle] def normalized[A](using q: Quotes, tpe: Type[A]): Type[?] =
    tpe.repr.simplified.dealias.asType
}

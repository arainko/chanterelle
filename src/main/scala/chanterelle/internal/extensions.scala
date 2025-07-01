package chanterelle.internal

import scala.quoted.*
import scala.annotation.publicInBinary

extension (tpe: Type[? <: AnyKind]) {
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

private[chanterelle] type None = None.type

private[chanterelle] inline def partialMatch[A](value: A)[Res](
  inline fns: PartialMatch.type => PartialMatch[A, Res]*
)(inline fallback: List[String] => Res): Res = ${ partialMatchImpl[A, Res]('value, 'fns, 'fallback) }


@publicInBinary
private[internal] def partialMatchImpl[A: Type, Res: Type](
  value: Expr[A],
  fns: Expr[Seq[PartialMatch.type => PartialMatch[A, Res]]],
  fallback: Expr[List[String] => Res]
)(using Quotes) = {
  import quotes.reflect.*

  val (tpes, cases) =
    Varargs
      .unapply(fns)
      .getOrElse(report.errorAndAbort("Couldn't decompose fns, are they a known vararg?"))
      .map {
        case '{
              type b
              type a >: b
              (a: PartialMatch.type) => a.when[b](using $_)[a, res]($fn)
            } =>
          val bindSym = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, TypeRepr.of[b])
          val ref = Ref(bindSym).asExprOf[b]
          val caseDef =
            CaseDef(Bind(bindSym, Typed(Wildcard(), TypeTree.of[b])), None, Expr.betaReduce('{ $fn.apply($ref) }).asTerm)
          (Type.show[b], caseDef)
        case other => report.errorAndAbort(s"Ehh: ${other.show}")
      }
      .toList
      .unzip

  

  Match(value.asTerm, cases ::: CaseDef(Wildcard(), None, '{ $fallback(${ Expr(tpes) }) }.asTerm) :: Nil ).asExprOf[Res]
}

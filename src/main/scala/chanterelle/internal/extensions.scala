package chanterelle.internal

import scala.quoted.*

extension (tpe: Type[? <: AnyKind]) {
  private[chanterelle] def fullName(using Quotes): String = {
    import quotes.reflect.*

    TypeRepr.of(using tpe).show(using Printer.TypeReprCode)
  }

  private[chanterelle] def repr(using Quotes): quotes.reflect.TypeRepr =
    quotes.reflect.TypeRepr.of(using tpe)
}

extension (expr: Expr[Any]) {

  private[chanterelle] def namedTupleToTuple(structure: Structure.Named)(using Quotes) = {
    import quotes.reflect.*
    val TuplesCompanion = '{ Tuples }.asTerm
    Select.unique(TuplesCompanion, "valuesOf")
      .appliedToTypes(structure.calculateNamesTpe.repr :: structure.calculateValuesTpe.repr :: Nil)
      .appliedTo(expr.asTerm)
      .asExpr
  }

  private[chanterelle] def accessNamedTupleFieldByName(name: String, structure: Structure.Named)(using Quotes) = {
    val asTuple = expr.namedTupleToTuple(structure)
    val idxOfName = structure.fields.keys.indexOf(name) //TODO: check for -1
    assert(idxOfName != -1, s"no field $name found in named tuple") //TODO: get rid of later
    asTuple.accesFieldByIndex(idxOfName, structure.asTuple)
  }

  private[chanterelle] def accessFieldByName(name: String)(using Quotes): quotes.reflect.Select = {
    import quotes.reflect.*
    Select.unique(expr.asTerm, name)
  }

  private[chanterelle] def accesFieldByIndex(index: Int, parentStructure: Structure.Tuple)(using Quotes): Expr[Any] = {
    import quotes.reflect.*
    if parentStructure.isPlain then accessFieldByName(s"_${index + 1}").asExpr // tuple accessors are 1 based
    else
      val tpeAtIndex = parentStructure.elements(index).calculateTpe
      (expr, tpeAtIndex): @unchecked match {
        case '{ $prod: scala.Product } -> '[tpe] => '{ $prod.productElement(${ Expr(index) }).asInstanceOf[tpe] }
      }
  }
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

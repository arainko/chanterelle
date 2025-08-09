package chanterelle.internal

import scala.annotation.tailrec
import scala.quoted.*

private[chanterelle] object PathSelector {

  def unapply(using outer: Quotes)(expr: quotes.reflect.Term): Some[Path] = {
    @tailrec
    def recurse(using
      Quotes
    )(acc: List[Path.Segment], term: quotes.reflect.Term): Path = {
      import quotes.reflect.*

      term match {
        case Inlined(_, _, tree) =>
          Logger.debug("Matched 'Inlined', recursing...")
          recurse(acc, tree)

        case Lambda(_, tree) =>
          Logger.debug("Matched 'Lambda', recursing...")
          recurse(acc, tree)

        case Block(_, tree) =>
          Logger.debug("Matched 'Block', recursing...")
          recurse(acc, tree)

        case Select(tree, name @ TupleField(index)) if tree.tpe <:< TypeRepr.of[Tuple] =>
          Logger.debug(
            s"Matched 'Select' (matching a tuple field) with name = $name"
          )
          recurse(
            acc.prepended(Path.Segment.TupleElement(tree.tpe.asType, index)),
            tree
          )

        case Apply(
              TypeApply(Select(Ident(_), "element"), elemTpe :: Nil),
              tree :: Nil
            ) =>
          Logger.debug(
            s"Matched 'Apply(TypeApply(...)) (matching .element)'",
            elemTpe.tpe.asType
          )
          recurse(acc.prepended(Path.Segment.Element(elemTpe.tpe.asType)), tree)

        case Apply(
              Apply(
                TypeApply(Select(Ident("NamedTuple"), "apply"), List(namesTpe, _)),
                tree :: Nil
              ),
              Literal(IntConstant(idx)) :: Nil
            ) =>
          Logger.debug(s"Matching NamedTuple#apply at index ($idx)")
          val names = TupleTypes.unrollStrings(namesTpe.tpe)
          // widen here because we're dealing with a singleton type of the lambda param, eg. '_$4'
          recurse(acc.prepended(Path.Segment.Field(tree.tpe.widen.asType, names(idx))), tree)

        case Apply(
              TypeApply(Select(tree, "apply"), List(_)),
              List(Literal(IntConstant(index)))
            ) if tree.tpe <:< TypeRepr.of[Tuple] =>
          Logger.debug(s"Matching Tuple#apply($index)")
          recurse(
            acc.prepended(Path.Segment.TupleElement(tree.tpe.asType, index)),
            tree
          )

        case ident @ Ident(_) =>
          Logger.debug(s"Matched 'Ident', returning...")
          Path(ident.tpe.asType, acc.toVector)

        case other =>
          Logger.debug(s"Matched an unexpected term: ${Printer.TreeStructure.show(other)}")
          val pos = expr.pos
          val code = pos.sourceCode.mkString
          outer.reflect.report.errorAndAbort(s"Couldn't parse '$code' as a valid path selector", pos)
      }
    }

    Some(Logger.loggedInfo("Parsed path")(recurse(Nil, expr)))
  }

  private object TupleField {
    def unapply(name: String): Option[Int] =
      name.stripPrefix("_").toIntOption.map(_ - 1) // ._1 means .apply(0)
  }
}

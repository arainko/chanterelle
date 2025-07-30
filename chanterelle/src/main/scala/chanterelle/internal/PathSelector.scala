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
        case Inlined(
              Some(
                Apply(
                  TypeApply(Select(tree, "apply"), List(Inferred())),
                  List(Literal(IntConstant(index)))
                )
              ),
              _,
              Typed(_, tpe @ Applied(TypeIdent("Elem"), _))
            ) =>
          recurse(
            acc.prepended(Path.Segment.TupleElement(tpe.tpe.asType, index)),
            tree
          )

        case Inlined(_, _, tree) =>
          Logger.debug("Matched 'Inlined', recursing...")
          recurse(acc, tree)

        case Lambda(_, tree) =>
          Logger.debug("Matched 'Lambda', recursing...")
          recurse(acc, tree)

        case Block(_, tree) =>
          Logger.debug("Matched 'Block', recursing...")
          recurse(acc, tree)

        case select @ Select(tree, name @ TupleField(index)) =>
          Logger.debug(
            s"Matched 'Select' (matching a tuple field) with name = $name"
          )
          if tree.tpe <:< TypeRepr.of[Tuple] then
            recurse(
              acc.prepended(Path.Segment.TupleElement(tree.tpe.asType, index)),
              tree
            )
          else
            recurse(
              acc.prepended(Path.Segment.Field(select.tpe.asType, name)),
              tree
            )

        case select @ Select(tree, name) =>
          Logger.debug(
            s"Matched 'Select' (matching field access) with name = $name"
          )
          recurse(
            acc.prepended(Path.Segment.Field(select.tpe.asType, name)),
            tree
          )

        // case TypeApply(Apply(TypeApply(Select(Ident(_), "at"), _), tree :: Nil), tpe :: Nil) =>
        //   Logger.debug(s"Matched 'TypeApply' (matching '.at')", tpe.tpe.asType)
        //   recurse(acc.prepended(Path.Segment.Case(tpe.tpe.asType)), tree)

        case Apply(
              TypeApply(Select(Ident(_), "element"), elemTpe :: Nil),
              tree :: Nil
            ) =>
          Logger.debug(
            s"Matched 'Apply(TypeApply(...)) (matching .element)'",
            elemTpe.tpe.asType
          )
          recurse(acc.prepended(Path.Segment.Element(elemTpe.tpe.asType)), tree)

        // Apply(Apply(TypeApply(Select(Ident("NamedTuple"), "apply"), List(Inferred(), Inferred())), List(Ident("_$2"))), List(Literal(IntConstant(2)))

        case Apply(
              Apply(
                TypeApply(Ident("apply") | Select(Ident("NamedTuple"), "apply"), List(_, _)),
                tree :: Nil
              ),
              Literal(IntConstant(idx)) :: Nil
            ) if tree.tpe <:< TypeRepr.of[NamedTuple.AnyNamedTuple] =>
          // widen here because we're dealing with a singleton type of the lambda param, eg. '_$4'
          tree.tpe.widen.asType match {
            case '[type tpe <: NamedTuple.AnyNamedTuple; tpe] =>
              val names = constStringTuple(TypeRepr.of[NamedTuple.Names[tpe]])
              // TODO: 'names' is a List and indexed access on a list is... Not the best to say the least
              recurse(acc.prepended(Path.Segment.Field(Type.of[tpe], names(idx))), tree)
          }

        // case Apply(
        //       Apply(
        //         TypeApply(Select(Ident(_), "element"), elemTpe :: _ :: Nil),
        //         Ident(_) :: Nil
        //       ),
        //       tree :: Nil
        //     ) =>
        //   Logger.debug(s"Matched 'element' of F[Elem] selection", elemTpe.tpe.asType)
        //   recurse(acc.prepended(Path.Segment.Element(elemTpe.tpe.asType)), tree)

        // Function arg selection can only happen as the first selection (aka the last one to be parsed) so this not being recursive is fine (?)
        // case TypeApply(
        //       Select(Apply(Select(ident @ Ident(_), "selectDynamic"), Literal(StringConstant(argName)) :: Nil), "$asInstanceOf$"),
        //       argTpe :: Nil
        //     ) =>
        //   Logger.debug(s"Matched 'selectDynamic' (matching a function arg selector) with name = $argName")
        //   Path(ident.tpe.asType, acc.prepended(Path.Segment.Field(argTpe.tpe.asType, argName)).toVector)

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

  private def constStringTuple(using
    Quotes
  )(tp: quotes.reflect.TypeRepr): List[String] = {
    import quotes.reflect.*
    tupleTypeElements(tp.asType).map {
      case ConstantType(StringConstant(l)) =>
        l
    }
  }

  private def tupleTypeElements(
    tpe: Type[?]
  )(using Quotes): List[quotes.reflect.TypeRepr] = {
    @tailrec def loop(using
      Quotes
    )(
      curr: Type[?],
      acc: List[quotes.reflect.TypeRepr]
    ): List[quotes.reflect.TypeRepr] = {
      import quotes.reflect.*

      curr match {
        case '[head *: tail] =>
          loop(Type.of[tail], TypeRepr.of[head] :: acc)
        case '[EmptyTuple] =>
          acc
        case other =>
          report.errorAndAbort(
            s"Unexpected type (${other.repr.show}) encountered when extracting tuple type elems. This is a bug in chanterelle."
          )
      }
    }

    loop(tpe, Nil).reverse
  }
}

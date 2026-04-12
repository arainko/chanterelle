package chanterelle.internal

import chanterelle.FieldName
import chanterelle.hidden.TupleModifier

import scala.quoted.*

import NamedTuple.AnyNamedTuple

private[chanterelle] enum Modifier derives Debug {
  def path: Path
  def span: Span

  case Put(path: Path, valueStructure: Structure.Named.Singular, value: Sources.Ref, span: Span)
  case Compute(path: Path, valueStructure: Structure.Named.Singular, value: Sources.Ref, span: Span)
  case Update(path: Path, tpe: Type[?], function: Sources.Ref, span: Span)
  case Remove(path: Path, fieldToRemove: String | Int, span: Span)
  case Rename(path: Path, fieldName: String => String, kind: Modifier.Kind, span: Span)
  case Merge(path: Path, valueStructure: Structure.Named, ref: Sources.Ref, span: Span)
}

private[chanterelle] object Modifier {
  def parse[A](
    mods: List[Expr[TupleModifier.Builder[A] => TupleModifier[A]]]
  )(using sources: Sources.Builder, quotes: Quotes): Either[List[ErrorMessage], List[Modifier]] = {
    import quotes.reflect.*

    def parseMerged[Mergee: Type](path: Path, mergee: Expr[Mergee], cfgSpan: Span)(using
      sources: Sources.Builder,
      quotes: Quotes
    ) =
      Structure
        .toplevel[Mergee]
        .narrow[Structure.Named]
        .toRight(ErrorMessage.CanOnlyMergeNamedTuples(cfgSpan))
        .map { struct =>
          val sourceRef = sources.add(mergee)
          Modifier.Merge(path, struct, sourceRef, cfgSpan)
        }

    mods.parTraverse {
      // TODO: report an issue to dotty: not able to match with quotes if $value is of type NamedTuple[?, ?]
      case cfg @ '{
            type selected <: AnyNamedTuple
            type v <: AnyNamedTuple
            (builder: TupleModifier.Builder[tup]) => builder.put[selected](${ AsTerm(PathSelector(path)) })[v]($value)
          } =>
        Structure
          .toplevel[v]
          .narrow[Structure.Named.Singular]
          .toRight(ErrorMessage.ExpectedSingletonNamedTuple(Type.of[v], Span.fromExpr(cfg)))
          .map(valueStructure => Modifier.Put(path, valueStructure, sources.add(value), Span.fromExpr(cfg)))

      case cfg @ '{
            type selected <: AnyNamedTuple
            type v <: AnyNamedTuple
            (builder: TupleModifier.Builder[tup]) => builder.compute[selected](${ AsTerm(PathSelector(path)) })[v]($value)
          } =>
        Structure
          .toplevel[v]
          .narrow[Structure.Named.Singular]
          .toRight(ErrorMessage.ExpectedSingletonNamedTuple(Type.of[v], Span.fromExpr(cfg)))
          .map(valueStructure => Modifier.Compute(path, valueStructure, sources.add(value), Span.fromExpr(cfg)))

      case cfg @ '{ (builder: TupleModifier.Builder[tup]) =>
            builder.update[selected](${ AsTerm(PathSelector(path)) })[newField]($fn)
          } =>
        Right(Modifier.Update(path, Type.of[newField], sources.add(fn), Span.fromExpr(cfg)))

      case cfg @ '{ (builder: TupleModifier.Builder[tup]) => builder.remove[selected](${ AsTerm(PathSelector(path)) }) } =>
        path.stripLast.collect {
          case (path, Path.Segment.Field(tpe, name))         => Right(Modifier.Remove(path, name, Span.fromExpr(cfg)))
          case (path, Path.Segment.TupleElement(tpe, index)) => Right(Modifier.Remove(path, index, Span.fromExpr(cfg)))
        }.getOrElse(Left(ErrorMessage.SelectorNeedsToPointToAField(path, Span.fromExpr(cfg))))

      // workaround for the case below...
      case cfg @ AsTerm(Lambda(_, Apply(Select(Ident(_), "rename"), List(fieldName)))) =>
        val parsedRenames = ParseFieldName.parse(fieldName.asExprOf[FieldName => FieldName])
        Right(
          Modifier.Rename(Path.empty(Type.of[Any]), parsedRenames, Kind.Regional, Span.fromExpr(cfg))
        )

      // this ish ain't matchign stuff if the type of `.rename` is  'Local & Regional' even tho the .local and .regional extractors work. Fun.
      // case cfg @ '{ (builder: TupleModifier.Builder[tup]) => builder.rename($fieldName) } =>
      //   val parsedRenames = ParseFieldName.parse(fieldName)
      //   Right(Modifier.Rename(Path.empty(Type.of[tup]), parsedRenames, Kind.Regional, Span.fromExpr(cfg)))

      case cfg @ '{ (builder: TupleModifier.Builder[tup]) =>
            builder.rename($fieldName).local(${ AsTerm(PathSelector(path)) })
          } =>
        val parsedRenames = ParseFieldName.parse(fieldName)
        Right(
          Modifier.Rename(path, parsedRenames, Kind.Local, Span.fromExpr(cfg))
        )

      case cfg @ '{ (builder: TupleModifier.Builder[tup]) =>
            builder.rename($fieldName).regional(${ AsTerm(PathSelector(path)) })
          } =>
        val parsedRenames = ParseFieldName.parse(fieldName)
        Right(
          Modifier.Rename(path, parsedRenames, Kind.Regional, Span.fromExpr(cfg))
        )

      case cfg @ AsTerm(Lambda(_, Apply(TypeApply(Select(Ident(_), "merge"), tpe :: Nil), List(mergee)))) =>
        tpe.tpe.asType match {
          case '[a] => parseMerged(Path.empty(Type.of[Any]), mergee.asExprOf[a], Span.fromExpr(cfg))
        }

      case cfg @ '{
            type a <: NamedTuple.AnyNamedTuple
            (builder: TupleModifier.Builder[tup]) => builder.merge[a]($mergee).regional(${ AsTerm(PathSelector(path)) })
          } =>
        parseMerged(path, mergee.asExprOf[a], Span.fromExpr(cfg))

      case other =>
        Logger.debug(s"Error parsing modifier: ${other.asTerm.show(using Printer.TreeStructure)}")
        report.errorAndAbort(s"Couldn't parse '${CodePrinter.codeAtSpan(Span.fromExpr(other))}' as a valid modifier", other)
    }
  }

  enum Kind derives Debug {
    case Regional, Local

    def isLocal: Boolean = this.isInstanceOf[Local.type]
  }

  private object AsTerm {
    def unapply(expr: Expr[Any])(using Quotes): Some[quotes.reflect.Term] = {
      import quotes.reflect.*
      Some(expr.asTerm)
    }
  }
}

package chanterelle.internal

import scala.quoted.*
import chanterelle.Mode
import scala.quoted.Type
import chanterelle.internal.Debug.AST
import chanterelle.internal.Transformation.IsHoisted
import chanterelle.internal.Transformation.Field
import scala.reflect.TypeTest

private[chanterelle] object FallibleInterpreter {
  def run[F[_]](transformation: Transformation[Fallible], source: Expr[Any])(using
    Quotes,
    Sources,
    Context.PossiblyFallible[F]
  ): Expr[Any] = {
    given Type[F] = Context.current.wrapperType.wrapper
    val mode = TransformationMode.create[F](Context.current.mode)
    recurse(transformation, source, mode).wrapped(mode)
  }

  private def recurse[F[_]: Type](transformation: Transformation[Fallible], source: Expr[Any], F: TransformationMode[F])(using
    Quotes,
    Sources,
    Context.PossiblyFallible[F]
  ): Value[F] = {
    FallibilityRefiner.run(transformation) match {
      case nonfallible: Transformation[Nothing] =>
        Value.Unwrapped(Context.current.asTotal.locally(Interpreter.runTransformation(source, nonfallible)))
      case None =>
        transformation match {
          case t @ Transformation.Named(_, fields, namesTpe, valuesTpe, outTpe) =>
            namedTransformation(t, source, F)
          case Transformation.Tuple(source, fields, outputTpe) =>
            ???
          case Transformation.Optional(source, paramTransformation, outputTpe) => ???
          case Transformation.EitherLike(source, left, right, outputTpe)       => ???
          case Transformation.MapLike(source, key, value, factory, outputTpe)  => ???
          case Transformation.IterLike(source, elem, factory, outputTpe)       => ???
          case Transformation.Leaf(output)                                     =>
            Value.Unwrapped(source)
          case Transformation.ConfedUp(config)                                     => ???
          case Transformation.Merged(mergees, fields, namesTpe, valuesTpe, outTpe) => ???
          case t: Transformation.Wrapped[fallible, f]                              =>
            t.isHoisted match {
              case IsHoisted.Yes =>
                // outputTpe is the unwrapped type, but the source is very much an F[a]
                (t.source.tpe) match {
                  case '[F[src]] =>
                    val src = source.asExprOf[F[src]]

                    FallibilityRefiner.run(t.wrapped) match {
                      case nonfallible: Transformation[Nothing] =>
                        Value.Wrapped {
                          '{
                            ${ F.value }
                              .map(
                                $src,
                                src => ${ Context.current.asTotal.locally(Interpreter.runTransformation('src, nonfallible)) }
                              )
                          }
                        }

                      case None =>
                    }
                    // to be able to advance with 'wrapped' that's fallible the 'mode' needs to be Mode.FailFast otherwise we can only .map the transformation

                    Value.Wrapped { src }
                }
              case IsHoisted.No =>
                t.source.tpe match {
                  case '[F[src]] =>
                    val src = source.asExprOf[F[src]]
                    Value.Unwrapped {
                      '{
                        ${ F.value }
                          .map($src, src => ${ Context.current.asTotal.locally(Interpreter.runTransformation('src, t.wrapped)) })
                      }
                    }
                }
            }
        }
    }
  }

  private def namedTransformation[F[_]: Type](t: Transformation.Named[Fallible], src: Expr[Any], F: TransformationMode[F])(using
    Quotes,
    Sources,
    Context.PossiblyFallible[F]
  ) = {
    val (unwrappeds, wrappeds) = t.fields.zipWithIndex.partitionMap {
      case (fieldName, field) -> idx =>
        field match
          case Field.FromSource(name, transformation) =>
            val fieldValue = StructuredValue.of(t.source, src).fieldValue(name)
            recurse(transformation, fieldValue, F).asFieldValue(idx, transformation.outputTpe)
          case Field.FromModifier(modifier) => ???
    }
    t.outputTpe match {
      case '[dest] =>

        F match {
          case TransformationMode.Accumulating(value) =>
            NonEmptyList
              .fromList(wrappeds.toList)
              .map(wrappeds =>
                Value.Wrapped(
                  ProductZipper.zipAndConstruct[F, dest](value, wrappeds, unwrappeds.toList)(
                    ProductConstructor.Primary(t.outputTpe)
                  )
                )
              )
              .getOrElse(Value.Unwrapped(ProductConstructor.Primary(t.outputTpe)(unwrappeds.map(_.value).toSeq)))
          case TransformationMode.FailFast(value) =>
            Value.Wrapped[F] {
              ProductBinder.nestFlatMapsAndConstruct[F, dest](
                value,
                unwrappeds.toList,
                wrappeds.toList,
                ProductConstructor.Primary(t.outputTpe)
              )
            }
        }
    }
  }

  enum TransformationMode[F[x]] {
    def value: Expr[Mode[F]]

    case Accumulating(value: Expr[Mode.Accumulating[F]])
    case FailFast(value: Expr[Mode.FailFast[F]])
  }

  object TransformationMode {
    def create[F[x]: Type](expr: Expr[Mode[F]])(using Quotes): TransformationMode[F] =
      expr match
        case '{ $acc: Mode.Accumulating[F] } =>
          Accumulating(acc)
        case '{ $ff: Mode.FailFast[F] } =>
          FailFast(ff)
        case _ =>
          quotes.reflect.report.errorAndAbort(
            "Couldn't determine the transformation mode, make sure an instance of either Mode.FailFast[F] or Mode.Accumulating[F] is in implicit scope"
          )

    given Debug[TransformationMode[?]] with {
      def astify(self: TransformationMode[?])(using Quotes): AST =
        self match
          case Accumulating(value) => AST.Text("Accumulating")
          case FailFast(value)     => AST.Text("FailFast")

    }
  }
  private enum Value[F[x]] {
    final def wrapped(F: TransformationMode[F])(using Quotes, Type[F]) = {

      this.runtimeChecked match
        case Unwrapped('{ $value: a }) =>
          '{ ${ F.value }.pure[a](${ value }) }
        case Wrapped('{ $value: F[a] }) =>
          value
    }

    final def asFieldValue(
      index: Int,
      unwrappedTpe: Type[?]
    ): scala.Either[FieldValue.Unwrapped, FieldValue.Wrapped[F]] =
      this match {
        case unw: Unwrapped[F]   => Left(new FieldValue.Unwrapped(index, unwrappedTpe, unw.value))
        case wrapped: Wrapped[F] => Right(new FieldValue.Wrapped(index, unwrappedTpe, wrapped.value))
      }

    case Unwrapped(value: Expr[Any]) extends Value[F]
    case Wrapped[F[_]](value: Expr[Any]) extends Value[F]
  }
}

package chanterelle.internal

import chanterelle.Mode
import chanterelle.internal.Debug.AST
import chanterelle.internal.Transformation.{ ElemTransformation, Field }

import scala.quoted.*
import scala.reflect.TypeTest
import scala.collection.Factory

private[chanterelle] object FallibleInterpreter {
  def run[F[_]](transformation: Transformation[Fallible], source: Expr[Any])(using
    Quotes,
    Sources,
    Context.PossiblyFallible[F]
  ): Expr[Any] = {
    given Type[F] = Context.current.wrapperType.wrapper
    val mode = Context.current.mode
    recurse(transformation, source, mode).wrapped(mode)
  }

  private def recurse[F[_]: Type](transformation: Transformation[Fallible], source: Expr[Any], F: TransformationMode[F])(using
    Quotes,
    Sources,
    Context.PossiblyFallible[F]
  ): Value[F] = {
    def nonfallibleTransformation(src: Expr[Any], nonfallible: Transformation[Nothing]) =
      Context.current.asTotal.locally(Interpreter.runTransformation(src, nonfallible))

    FallibilityRefiner.run(transformation) match {
      case nonfallible: Transformation[Nothing] =>
        Value.Unwrapped(nonfallibleTransformation(source, nonfallible))
      case None =>
        transformation match {
          case t @ Transformation.Named(_, fields, namesTpe, valuesTpe, outTpe) =>
            val (unwrappeds, wrappeds) = t.fields.zipWithIndex.partitionMap {
              case (_, Field.FromSource(name, transformation)) -> idx =>
                val fieldValue = StructuredValue.of(t.source, source).fieldValue(name)
                recurse(transformation, fieldValue, F).asFieldValue(idx, transformation.outputTpe)
              case (fieldName, Field.FromModifier(modifier)) -> idx => ???
            }
            handleTransformation(F, t, unwrappeds, wrappeds, ProductConstructor.Primary(t.outputTpe))

          case t @ Transformation.Tuple(src, fields, outputTpe) =>
            val (unwrappeds, wrappeds) = fields.partitionMap { (idx, transformation) =>
              val fieldValue = StructuredValue.of(t.source, source).elementValue(idx)
              recurse(transformation, fieldValue, F).asFieldValue(idx, transformation.outputTpe)
            }
            handleTransformation(F, t, unwrappeds, wrappeds, ProductConstructor.Tuple(src))

          case Transformation.Optional(sourceStruct, paramTransformation, outputTpe) =>
            (sourceStruct.tpe, paramTransformation.outputTpe).runtimeChecked match {
              case '[Option[src]] -> '[out] =>
                val src = source.asExprOf[Option[src]]
                Value.Wrapped {
                  '{
                    $src match {
                      case Some(value) =>
                        ${ F.value }.map(${ recurse(paramTransformation, 'value, F).wrapped(F).asExprOf[F[out]] }, Some.apply)
                      case None => ${ F.value }.pure(None)
                    }
                  }
                }
            }
          case Transformation.EitherLike(sourceStruct, left, right, outputTpe) =>
            (sourceStruct.tpe, left.outputTpe, right.outputTpe).runtimeChecked match {
              case ('[Either[left, right]], '[leftOut], '[rightOut]) =>
                val src = source.asExprOf[Either[left, right]]
                Value.Wrapped {
                  '{
                    $src match {
                      case Right(rightValue) =>
                        ${ F.value }.map(${ recurse(right, 'rightValue, F).wrapped(F).asExprOf[F[rightOut]] }, Right.apply)
                      case Left(leftValue) =>
                        ${ F.value }.pure(Left(${ nonfallibleTransformation('leftValue, left).asExprOf[leftOut] }))
                    }
                  }
                }
            }
          case Transformation.MapLike(sourceStruct, key, value, factory, outputTpe) =>
            (sourceStruct.tycon, outputTpe, source).runtimeChecked match {
              case (
                    '[type outMap[k, v]; outMap],
                    '[collection.Map[outKey, outValue]],
                    '{ $srcValue: collection.Map[srcKey, srcValue] }
                  ) =>
                val fac = factory.asExprOf[Factory[(outKey, outValue), outMap[outKey, outValue]]]
                def handlePair[A: Type, B: Type](left: Expr[F[A]], right: Expr[F[B]])(using Quotes): Expr[F[(A, B)]] =
                  F match {
                    case TransformationMode.Accumulating(value) =>
                      '{ $value.zip[A, B]($left, $right) }
                    case TransformationMode.FailFast(value) =>
                      '{ $value.flatMap($left, left => $value.map($right, right => (left, right))) }
                  }
                Value.Wrapped {
                  '{
                    ${ F.value }.traverseCollection[(srcKey, srcValue), (outKey, outValue), Iterable[(srcKey, srcValue)], outMap[
                      outKey,
                      outValue
                    ]](
                      $srcValue,
                      (srcKey, srcValue) =>
                        ${
                          handlePair(
                            recurse(key, 'srcKey, F).wrapped(F).asExprOf[F[outKey]],
                            recurse(value, 'srcValue, F).wrapped(F).asExprOf[F[outValue]]
                          )
                        }
                    )(using $fac)
                  }
                }

            }
          case Transformation.IterLike(sourceStruct, elem, factory, outputTpe) =>
            (sourceStruct.tycon, outputTpe, source).runtimeChecked match {
              case (
                    '[type coll[a]; coll],
                    '[Iterable[elem]],
                    '{ $srcValue: Iterable[srcElem] }
                  ) =>
                val f = factory.asExprOf[Factory[elem, coll[elem]]]
                Value.Wrapped {
                  '{
                    ${ F.value }.traverseCollection[srcElem, elem, Iterable[srcElem], coll[elem]](
                      $srcValue,
                      srcElem => ${ recurse(elem, 'srcElem, F).wrapped(F).asExprOf[F[elem]] }
                    )(using $f)
                  }
                }
            }

          case Transformation.Leaf(output) =>
            Value.Unwrapped(source)
          case Transformation.ConfedUp(config)                            => ???
          case merged: (Transformation.Merged | Transformation.Mapped[f]) =>
            Value.Unwrapped(nonfallibleTransformation(source, merged))
          case t: Transformation.Hoisted[f] =>
            (t.source.tpe, t.outputTpe).runtimeChecked match {
              case '[F[src]] -> '[out] =>
                val src = source.asExprOf[F[src]]
                (t.wrapped) match {
                  case ElemTransformation.HoistedFallible(wrapped, mode) =>
                    val failFast = mode.asExprOf[Mode.FailFast[F]]
                    Value.Wrapped {
                      '{
                        $failFast
                          .flatMap(
                            $src,
                            src => ${ recurse[F](wrapped, 'src, F).wrapped(Context.current.mode).asExprOf[F[out]] }
                          )
                      }
                    }
                  case ElemTransformation.HoistedNonFallible(wrapped) =>
                    Value.Wrapped {
                      '{
                        ${ F.value }
                          .map(
                            $src,
                            src =>
                              ${ Context.current.asTotal.locally(Interpreter.runTransformation('src, wrapped)).asExprOf[out] }
                          )
                      }
                    }
                }
            }
        }
    }
  }

  private def handleTransformation[F[_]](
    F: TransformationMode[F],
    transformation: Transformation[Fallible],
    unwrappeds: Iterable[FieldValue.Unwrapped],
    wrappeds: Iterable[FieldValue.Wrapped[F]],
    construct: ProductConstructor
  )(using Type[F], Quotes): Value[F] =
    transformation.outputTpe match {
      case '[dest] =>
        F match {
          case TransformationMode.Accumulating(value) =>
            NonEmptyList
              .fromList(wrappeds.toList)
              .map(wrappeds =>
                Value.Wrapped(
                  ProductZipper.zipAndConstruct[F, dest](value, wrappeds, unwrappeds.toList)(
                    construct
                  )
                )
              )
              .getOrElse(Value.Unwrapped(construct(unwrappeds.map(_.value).toSeq)))
          case TransformationMode.FailFast(value) =>
            Value.Wrapped[F] {
              ProductBinder.nestFlatMapsAndConstruct[F, dest](
                value,
                unwrappeds.toList,
                wrappeds.toList,
                construct
              )
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

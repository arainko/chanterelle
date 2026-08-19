package chanterelle.internal

import scala.quoted.*
import chanterelle.Mode
import scala.quoted.Type
import chanterelle.internal.Debug.AST

private[chanterelle] object FallibleInterpreter {
  def run[F[_]](transformation: Transformation[Fallible], source: Expr[Any])(using
    Quotes,
    Sources,
    Context.PossiblyFallible[F]
  ): Expr[Any] = {
    transformation match {
      case Transformation.Named(source, fields, namesTpe, valuesTpe)       => ???
      case Transformation.Tuple(source, fields, outputTpe)                 => ???
      case Transformation.Optional(source, paramTransformation, outputTpe) => ???
      case Transformation.EitherLike(source, left, right, outputTpe)       => ???
      case Transformation.MapLike(source, key, value, factory, outputTpe)  => ???
      case Transformation.IterLike(source, elem, factory, outputTpe)       => ???
      case Transformation.Leaf(output)                                     => ???
      case Transformation.ConfedUp(config)                                 => ???
      case Transformation.Merged(mergees, fields, namesTpe, valuesTpe)     => ???
      case Transformation.Wrapped(source, wrapped, outputTpe, isHoisted)   => ???
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
    final def wrapped[A](F: TransformationMode[F], tpe: Type[A])(using Quotes, Type[F]): Expr[F[A]] = {
      given Type[A] = tpe

      this match
        case Unwrapped(value) =>
          '{ ${ F.value }.pure[A](${ value.asExprOf[A] }) }
        case Wrapped(value) =>
          value.asExprOf[F[A]]
    }

    final def asFieldValue(index: Int, tpe: Type[?]): scala.Either[FieldValue.Unwrapped, FieldValue.Wrapped[F]] =
      this match {
        case unw: Unwrapped[F]   => Left(new FieldValue.Unwrapped(index, tpe, unw.value))
        case wrapped: Wrapped[F] => Right(new FieldValue.Wrapped(index, tpe, wrapped.value))
      }

    case Unwrapped(value: Expr[Any])
    case Wrapped(value: Expr[F[Any]])
  }
}

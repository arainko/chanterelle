package chanterelle.internal

import chanterelle.internal.FallibleInterpreter.TransformationMode

import scala.annotation.nowarn
import scala.quoted.*

private[chanterelle] object ProductZipper {
  type ZippableMode[F[_]] = TransformationMode.Accumulating[F] | TransformationMode.Parallel[F]

  /**
   * 'Zips' a product using Mode.Accumulating[F]#zip into a nested Tuple2 then
   * unpacks that said tuple and allows the caller to use the unpacked fields to construct a tree of type Dest.
   *
   * @param wrappedFields fields wrapped in F
   * @param unwrappedFields fields that are not wrapped in F (will not be a part of the zipped tuple)
   * @param construct function that allows the caller to create trees with the unpacked values
   */
  def zipAndConstruct[F[x]: Type, Dest: Type](
    F: ZippableMode[F],
    wrappedFields: NonEmptyList[FieldValue.Wrapped[F]],
    unwrappedFields: List[FieldValue.Unwrapped]
  )(construct: ProductConstructor)(using Quotes): Expr[F[Dest]] = {
    val reorderedFields = wrappedFields.reverse

    val (zippedValue, zippedTpe) = zipFields[F](F, reorderedFields)

    // using a method for 'transform' fixes a bunch of owner issues reported by -Xcheck-macros
    zippedTpe match {
      case '[a] =>
        '{
          def transform(value: a) = ${ unzipAndConstruct[Dest](reorderedFields, unwrappedFields, 'value, construct) }
          ${ F.value }.map(${ zippedValue.asExprOf[F[a]] }, transform)
        }
    }: @nowarn("msg=unused local definition")
  }

  // so this could be implemented in a way way way WAY easier way without tracking the zipped type but it falls apart in presence of opaque types :):):)
  private def zipFields[F[x]: Type](
    F: ZippableMode[F],
    wrappedFields: NonEmptyList[FieldValue.Wrapped[F]]
  )(using Quotes) = {
    val first = wrappedFields.head
    val (zippedValue, zippedTpe) = wrappedFields.tail.foldLeft((first.value, first.tpe)) {
      case (accValue, '[accTpe]) -> FieldValue.Wrapped(tpe = '[fieldTpe], value = fieldValue) =>
        val nextTpe = Type.of[(fieldTpe, accTpe)]
        F.zip[fieldTpe, accTpe](fieldValue.asExprOf[F[fieldTpe]], accValue.asExprOf[F[accTpe]]) -> nextTpe
    }: @nowarn("msg=match may not be exhaustive")
    alignOwner(zippedValue) -> zippedTpe
  }

  private def unzipAndConstruct[Dest: Type](
    wrappedFields: NonEmptyList[FieldValue.Wrapped[?]],
    unwrappedFields: List[FieldValue.Unwrapped],
    nestedPairs: Expr[Any],
    construct: ProductConstructor
  )(using Quotes) = {
    val unzippedFields = ProductZipper.unzip(nestedPairs, wrappedFields)
    val fields = (unzippedFields ::: unwrappedFields).sortBy(_.index).map(field => alignOwner(field.value))
    construct(fields).asExprOf[Dest]
  }

  private def alignOwner(expr: Expr[Any])(using Quotes) = {
    import quotes.reflect.*
    expr.asTerm.changeOwner(Symbol.spliceOwner).asExpr
  }

  private def unzip(
    nestedPairs: Expr[Any],
    _fields: NonEmptyList[FieldValue.Wrapped[?]]
  )(using Quotes) = {
    val fields = _fields.toVector
    val size = fields.size

    if size == 1 then {
      fields.head.unwrapped(nestedPairs) :: Nil
    } else {
      fields.indices.map { idx =>
        if idx == 0 then fields(idx).unwrapped(unpackRight(nestedPairs, size - 1).asExpr)
        else fields(idx).unwrapped(unpackLeft(unpackRight(nestedPairs, size - 1 - idx).asExpr).asExpr)
      }.toList
    }
  }

  private def unpackRight(expr: Expr[Any], times: Int)(using Quotes): quotes.reflect.Term = {
    import quotes.reflect.*

    if times == 0 then expr.asTerm
    else Select.unique(unpackRight(expr, times - 1), "_2")
  }

  private def unpackLeft(expr: Expr[Any])(using Quotes): quotes.reflect.Term = {
    import quotes.reflect.*
    Select.unique(expr.asTerm, "_1")
  }
}

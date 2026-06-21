package chanterelle.internal

import chanterelle.hidden.Tuples

import scala.quoted.*

private[chanterelle] opaque type StructuredValue[S <: Structure, V <: Expr[Any] & Singleton] = S

private[chanterelle] object StructuredValue {
  def of[S <: Structure](structure: S, value: Expr[Any]): StructuredValue[S, value.type] = structure

  extension [V <: Expr[Any] & Singleton](self: StructuredValue[Structure.Named, V]) {
    def fieldValue(name: String)(using v: ValueOf[V], q: Quotes) = v.value.accessNamedTupleFieldByName(name, self)
  }

  extension [V <: Expr[Any] & Singleton](self: StructuredValue[Structure.Tuple, V]) {
    def elementValue(idx: Int)(using v: ValueOf[V], q: Quotes) = v.value.accesFieldByIndex(idx, self)
  }

  extension (expr: Expr[Any]) {

    private def namedTupleToTuple(structure: Structure.Named)(using Quotes) = {
      import quotes.reflect.*
      // NamedTuple.toTuple[("i", "b"), (Int, Int)]((i = 1, b = 2))
      val TuplesCompanion = '{ Tuples }.asTerm
      // (structure.namesTpe, structure.valuesTpe).runtimeChecked match {
      //   case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
      //     '{
      //       // $expr.asInstanceOf[values]
      //       NamedTuple.toTuple(${ expr.asExprOf[NamedTuple.NamedTuple[names, values]] }): values
      //       // .toTuple
      //     }
      // }
      Typed(
        Select
          .unique(TuplesCompanion, "valuesOf")
          .appliedToTypes(structure.namesTpe.repr :: structure.valuesTpe.repr :: Nil)
          .appliedTo(expr.asTerm),
        TypeTree.of(using structure.valuesTpe)
      ).asExpr
    }

    private def accessNamedTupleFieldByName(name: String, structure: Structure.Named)(using Quotes) = {
      val asTuple = expr.namedTupleToTuple(structure)
      val idxOfName = structure.fields.keys.indexOf(name) // TODO: check for -1
      assert(idxOfName != -1, s"no field $name found in named tuple") // TODO: get rid of later
      asTuple.accesFieldByIndex(idxOfName, structure.asTuple)
    }

    private def accessFieldByName(name: String, tpe: Type[?])(using Quotes): quotes.reflect.Term = {
      import quotes.reflect.*
      Typed(Select.unique(expr.asTerm, name), TypeTree.of(using tpe)) // wrap in a Typed node to widen the type
    }

    private def accesFieldByIndex(index: Int, parentStructure: Structure.Tuple)(using Quotes): Expr[Any] = {
      // parentStructure.tpe.repr.typeSymbol.declaredField()
      // if parentStructure.isPlain then accessFieldByName(s"_${index + 1}", parentStructure.elements(index).tpe).asExpr
      // else
        // Tuple
        // (1, 2, 3).apply()
        val tpeAtIndex = parentStructure.elements(index).tpe
        (expr, tpeAtIndex).runtimeChecked match {
          case '{ $prod: scala.Product } -> '[tpe] => '{ $prod.productElement(${ Expr(index) }).asInstanceOf[tpe] }
        }
    }
  }

}

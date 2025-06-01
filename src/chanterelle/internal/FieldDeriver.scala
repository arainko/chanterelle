package chanterelle.internal

import chanterelle.*
import scala.quoted.*

private[chanterelle] object FieldDeriver {
  def fromStructure(structure: Structure)(using Quotes): Type[Field[?]] = {
    def recurse(using Quotes)(current: Structure): quotes.reflect.TypeRepr = {
      import quotes.reflect.*
      current match
        case Structure.Named(tpe = tpe, fields = fields) => 
          tpe match {
            case '[tpe] =>
              fields.foldLeft(TypeRepr.of[Field[tpe]]) { case (acc, (name, curr)) =>
                Refinement(acc, name, recurse(curr))
              }
          }
        case Structure.Tuple(tpe = tpe, elements = elems) => 
          tpe match {
            case '[tpe] =>
              elems.zipWithIndex.foldLeft(TypeRepr.of[Field[tpe]]) { case (acc, (curr, idx)) =>
                Refinement(acc, s"_$idx", recurse(curr))
              }
          }
        case Structure.Optional(tpe = tpe, paramStruct = param) => 
          tpe match {
            case '[tpe] =>
              Refinement(TypeRepr.of[Field[tpe]], "element", recurse(param))
          }
        case Structure.Collection(tpe = tpe, paramStruct = param) => 
          tpe match {
            case '[tpe] =>
              Refinement(TypeRepr.of[Field[tpe]], "element", recurse(param))
          }
        case Structure.Leaf(tpe = tpe) => 
          tpe match {
            case '[tpe] =>
              TypeRepr.of[Field[tpe]]
          }
    }
      
    recurse(structure).asType match {
      case tpe @ '[Field[?]] => tpe 
    }
  }
}

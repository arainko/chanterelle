package chanterelle.internal

import chanterelle.*
import scala.quoted.*
import Selector as Sel

private[chanterelle] object FieldDeriver {
  inline def testParsing[A](using DummyImplicit)[Tpe](inline selector: Selector ?=> A => Tpe) = ${ testParsinMacro[A, Tpe]('selector) }

  def testParsinMacro[A: Type, Tpe: Type](selector: Expr[Selector ?=> A => Tpe])(using Quotes) = {
    import quotes.reflect.*

    // report.info(selector.asTerm.show)
    val translated: Expr[(Sel, A) => Tpe] = selector match {
      case '{ (sel: Sel) ?=> (a: A) => ($body(sel, a): Tpe)  } => body
    }

    def recurse[A, Tpe](expr: Expr[(Sel, A) => Tpe], descriptions: List[String])(using Quotes): List[String] = {
      expr match {
        case '{ (_: Sel, a: t) => a } => "identity" :: descriptions
        case '{ (sel: Sel, a: Option[a] | Iterable[a]) => sel.element(($next(sel, a))) } =>
          recurse(next, "elem" :: descriptions)
          // import quotes.reflect.*

          // recurse(nextTerm.asExprOf[(Sel, Any) => Any], "field" :: descriptions)
        // case '{ (sel: Sel) ?=> (a: A) => ($body(sel, a): Option[a] | Iterable[a]).element } => 
          // report.info(s"matched elem, rest: ${body.asTerm.show}")
      }
    }

    val msg = recurse(translated, Nil).reverse.mkString(", ")
    report.info(msg)

    '{}
  }
}

/*
case '{ (arg: Renamer) => arg } => accumulatedFunctions

case '{ (arg: Renamer) => ($body(arg): Renamer).toUpperCase } =>
  recurse(body, ((str: String) => str.toUpperCase) :: accumulatedFunctions)
*/

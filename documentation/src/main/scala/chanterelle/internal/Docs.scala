package chanterelle.internal

import scala.quoted.*

object Docs {
  def prettyPrint[A: Show](value: A) =
    println {
      s"""
```scala

${Show.show(value)}
```
"""
    }

  inline def prettyPrintVals(inline values: Any*) = {
    val reprs = valsRepr(values*)
    val output = reprs.map(repr => s"val ${repr.valName} = ${repr.repr}").mkString("\n```scala\n\n", "\n", "\n```")
    println(output)
  }

  inline def valsRepr(inline values: Any*): List[(valName: String, repr: String)] = ${ prettyPrintValsMacro('values) }

  def prettyPrintValsMacro(values: Expr[Seq[Any]])(using Quotes) = {
    import quotes.reflect.*

    Expr.ofList {
      Varargs
        .unapply(values)
        .getOrElse(report.errorAndAbort("Couldn't unpack varargs", values))
        .map {
          case '{ $expr: tpe } =>
            val Show = Expr.summon[Show[tpe]].getOrElse(report.errorAndAbort(s"Couldn't get Show instance for ${Type.show[tpe]}"))
            val valName = valNameMacro(expr)
            '{ (valName = $valName, repr = $Show.show($expr)) }
        }
    }
  }

  inline def valName[A](inline value: A): String = ${ valNameMacro('value) }

  def valNameMacro[A](value: Expr[A])(using Quotes): Expr[String] = {
    import quotes.reflect.*
    val name = value.asTerm match {
      case Ident(name)                => name
      case Inlined(_, _, Ident(name)) => name
      case other => report.errorAndAbort(s"Couldn't find name of val - ${other.show(using Printer.TreeStructure)}", value)
    }
    Expr(name)
  }

}

package chanterelle.internal

import chanterelle.FieldName

import java.util.regex.Pattern
import scala.quoted.*

private[chanterelle] object ParseFieldName {
  def parse(expr: Expr[FieldName => FieldName])(using Quotes): String => String = {
    import quotes.reflect.*

    def recurse(
      current: Expr[FieldName => FieldName],
      accumulatedFunctions: List[String => String]
    )(using Quotes): List[String => String] = {
      current match {
        case '{ (arg: FieldName) => arg } => accumulatedFunctions

        case '{ (arg: FieldName) => ($body(arg): FieldName).toUpperCase } =>
          recurse(body, ((str: String) => str.toUpperCase) :: accumulatedFunctions)

        case '{ (arg: FieldName) => ($body(arg): FieldName).toLowerCase } =>
          recurse(body, ((str: String) => str.toLowerCase) :: accumulatedFunctions)

        case '{ (arg: FieldName) => ($body(arg): FieldName).rename(${ Expr(from) }, ${ Expr(to) }) } =>
          recurse(body, ((str: String) => if str == from then to else str) :: accumulatedFunctions)

        case '{ (arg: FieldName) => ($body(arg): FieldName).replace(${ Expr(from) }, ${ Expr(to) }) } =>
          recurse(body, ((str: String) => str.replace(from, to)) :: accumulatedFunctions)

        case '{ (arg: FieldName) =>
              ($body(arg): FieldName).regexReplace(${ Expr(pattern) }: String, ${ Expr(replacement) }: String)
            } =>
          recurse(
            body, {
              val regex = Pattern.compile(pattern)
              (str: String) => regex.matcher(str).replaceAll(replacement)
            } :: accumulatedFunctions
          )

        case '{ (arg: FieldName) =>
              ($body(arg): FieldName).regexReplace(${ Expr(pattern) }: String, $fieldName: FieldName => FieldName)
            } =>
          val nestedFieldName = Function.chain(recurse(fieldName, Nil))
          recurse(
            body, {
              val regex = Pattern.compile(pattern)
              (str: String) => regex.matcher(str).replaceAll(matchGroup => nestedFieldName(matchGroup.group()))
            } :: accumulatedFunctions
          )

        case '{ (arg: FieldName) => ($body(arg): FieldName).stripPrefix(${ Expr(prefix) }) } =>
          recurse(body, ((str: String) => str.stripPrefix(prefix)) :: accumulatedFunctions)

        case '{ (arg: FieldName) => ($body(arg): FieldName).stripSuffix(${ Expr(suffix) }) } =>
          recurse(body, ((str: String) => str.stripSuffix(suffix)) :: accumulatedFunctions)

        case '{ (arg: FieldName) => ($body(arg): FieldName).capitalize } =>
          recurse(body, ((str: String) => str.capitalize) :: accumulatedFunctions)

        case '{ 
          type trans[x <: String] <: String
          (arg: FieldName) => ($body(arg): FieldName).matchTyped[trans] 
        } => 
          def evaluate[F[x <: String] <: String: Type](name: String)(using Quotes): String = {
            import quotes.reflect.*
            val fieldNameTpe = ConstantType(StringConstant(name)).asType
            fieldNameTpe match { case '[type name <: String; name] => 
              Type.of[F[name]].repr.simplified.asType match {
                case '[type res <: String; res] => Type.valueOfConstant[res].get
              }
            }
          }

          recurse(body, evaluate[trans] :: accumulatedFunctions)

        case '{ ($first: FieldName => FieldName).andThen[FieldName]($second) } =>
          recurse(first, recurse(second, accumulatedFunctions))

        case '{ ($first: FieldName => FieldName).compose[FieldName]($second) } =>
          recurse(second, recurse(first, accumulatedFunctions))

        case expr =>
          report.errorAndAbort(
            s"Invalid fieldName expression - make sure all of the fieldName expressions can be read at compiletime - ${expr.show}",
            expr
          )
      }
    }

    val renameFunctions = recurse(expr, Nil)
    scala.Function.chain(renameFunctions)
  }
}

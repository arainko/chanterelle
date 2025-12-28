package chanterelle

/**
 * `FieldName` is a DSL designed to describe name transformations on fields and cases.
 *
 * Its compiletime nature means that all the operations and the arguments to those operations need to be known ahead of time, for example:
 *
 * {{{
 * (fieldName: FieldName) => fieldName.toUpperCase.rename("fromThis", "toThat")
 * }}}
 *
 * ...is a valid fieldName expression that can be lifted at compiletime, while something like:
 * {{{
 * val dynamicString = Random.nextString(10)
 * (fieldName: FieldName) => fieldName.toUpperCase.rename("fromThis", dynamicString)
 * }}}
 *
 * ...is not since the value of `dynamicString` is not known at compiletime.
 *
 * Users can provide their own 'bundles' of operations by creating and using `transparent inline defs`, like so:
 * {{{
 * transparent inline def renamedAndUppercased(inline fieldName: FieldName) =
 *   fieldName.rename("someName", "someOtherName").toUpperCase
 *
 * (someName = 1).transform(_.rename(renamedAndUppercased)) // yields (SOMEOTHERNAME = 1)
 * }}}
 */
sealed trait FieldName {

  /**
   * Equivalent to `String#toUpperCase`
   */
  def toUpperCase: FieldName

  /**
   * Equivalent to `String#toLowerCase`
   */
  def toLowerCase: FieldName

  /**
   * Equivalent to the function `(str: String) => if str == from then to else str`
   */
  def rename(from: String, to: String): FieldName

  /**
   * Equivalent to `String#replace(target, replacement)`
   */
  def replace(target: String, replacement: String): FieldName

  /**
   * Equivalent to the function `(str: String) => Pattern.compile(pattern).matcher(str).replaceAll(replacement: String)`
   */
  def regexReplace(pattern: String, replacement: String): FieldName

  /**
   * Equivalent to the function `(str: String) => Pattern.compile(pattern).matcher(str).replaceAll(replacement: MatchGroup => String)`
   */
  def regexReplace(pattern: String, replacement: FieldName => FieldName): FieldName

  /**
   * Equivalent to `String#stripPrefix(prefix)`
   */
  def stripPrefix(prefix: String): FieldName

  /**
   * Equivalent to `String#stripSuffix(suffix)`
   */
  def stripSuffix(suffix: String): FieldName

  /**
   * Equivalent to `String#capitalize`
   */
  def capitalize: FieldName
}

object FieldName {
  // The snake-case-to-camel-case and kebab-case-to-camel-case transformation regexes were copied from circe-generic-extras:
  // https://github.com/circe/circe-generic-extras/blob/2e103585b26ec30619a7de33ff15122344f041f3/generic-extras/src/main/scala/io/circe/generic/extras/Configuration.scala#L86
  object camelCase {

    /**
     * Transforms a field name from camelCase to kebab-case
     */
    transparent inline def toKebabCase(inline fieldName: FieldName): FieldName =
      fieldName
        .regexReplace("([A-Z]+)([A-Z][a-z])", "$1-$2")
        .regexReplace("([a-z\\d])([A-Z])", "$1-$2")
        .toLowerCase

    /**
     * Transforms a field name from camelCase to snake_case
     */
    transparent inline def toSnakeCase(inline fieldName: FieldName): FieldName =
      fieldName
        .regexReplace("([A-Z]+)([A-Z][a-z])", "$1_$2")
        .regexReplace("([a-z\\d])([A-Z])", "$1_$2")
        .toLowerCase
  }

  object snakeCase {

    /**
     * Transforms a field from snake_case to camelCase
     */
    transparent inline def toCamelCase(inline fieldName: FieldName): FieldName =
      fieldName
        .regexReplace("^_[a-zA-Z\\d]", _.replace("_", "").toLowerCase)
        .regexReplace("_[a-zA-Z\\d]", _.replace("_", "").toUpperCase)
  }

  object kebabCase {

    /**
     * Transforms a field from kebab-case to camelCase
     */
    transparent inline def toCamelCase(inline fieldName: FieldName): FieldName =
      fieldName
        .regexReplace("^-[a-zA-Z\\d]", _.replace("-", "").toLowerCase)
        .regexReplace("-[a-zA-Z\\d]", _.replace("-", "").toUpperCase)
  }
}

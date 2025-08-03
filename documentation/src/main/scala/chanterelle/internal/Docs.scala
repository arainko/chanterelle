package chanterelle.internal

object Docs {
  def prettyPrint[A: Show](value: A) =
    println {
      s"""
```scala

${Show.show(value)}
```
"""
    }
}

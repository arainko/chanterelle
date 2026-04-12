package chanterelle.internal

import scala.annotation.implicitNotFound
import scala.collection.mutable.ArrayBuffer
import scala.quoted.*

private[chanterelle] opaque type Sources = Map[Int, Expr[Any]]

private[chanterelle] object Sources {

  @implicitNotFound("Call Sources.current.withPrimary to be able to use Sources")
  opaque type Scope = Unit

  opaque type Ref = Int

  object Ref {
    val Primary: Ref = -1

    given Debug[Ref] = summon
  }

  opaque type Builder = collection.mutable.ArrayBuffer[Expr[Any]]

  extension (self: Builder) {
    def add(value: Expr[Any]): Ref = {
      val ref = self.size
      self.addOne(value)
      ref
    }
    def build: Sources = self.zipWithIndex.map(_.swap).toMap
  }

  def newBuilder: Builder = ArrayBuffer.empty

  inline def current(using src: Sources): src.type = src

  extension (self: Sources) {
    inline def withPrimary[A](expr: Expr[Any])(inline f: (Sources, Scope) ?=> A): A = {
      f(using self.updated(Ref.Primary, expr), ())
    }

    def updated(ref: Ref, value: Expr[Any])(using Scope): Sources =
      self.updated(ref, value)

    def get(ref: Ref)(using Scope): Expr[Any] = self(ref)
  }
}

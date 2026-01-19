package chanterelle.internal

import scala.quoted.*
import scala.collection.mutable.ArrayBuffer

private[chanterelle] opaque type Sources = Map[Int, Expr[Any]]

private[chanterelle] object Sources {

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

  extension (self: Sources) {
    def updated(ref: Ref, value: Expr[Any]): Sources = 
      self.updated(ref, value)
    // def advanceSources()
    def get(ref: Ref): Expr[Any] = self(ref)
  }
}

package chanterelle.internal

import scala.collection.immutable.VectorMap
import scala.compiletime.*
import scala.deriving.Mirror
import scala.quoted.*
import scala.reflect.ClassTag
import chanterelle.internal.Debug.AST

trait Show[-A] {
  def astify(self: A): Debug.AST

  extension (self: A) final def show: String = astify(self).dropEmpty.show(using Debug.UseColors.No)
}

object Show extends LowPriorityShow {
  import Debug.AST.*
  import Debug.AST

  def show[A: Show](value: A) = value.show

  inline given namedTuple[A <: NamedTuple.AnyNamedTuple](using Mirror.ProductOf[A]): Show[A] =
    Show.derived[A]

  given nothing: Show[Nothing] with {
    def astify(self: Nothing): AST = throw RuntimeException("That's just silly, trying to show a Nothing?")
  }

  given string: Show[String] with {
    override def astify(self: String): AST = Text(s""""${self}"""")
  }

  given int: Show[Int] with {
    override def astify(self: Int): AST = Text(self.toString)
  }

  given bool: Show[Boolean] with {
    override def astify(self: Boolean): AST = Text(self.toString)
  }

  given collection[A: Show as A, Coll[a] <: Iterable[a]](using tag: ClassTag[Coll[A]]): Show[Coll[A]] with {
    def astify(self: Coll[A]): AST = {
      val name = tag.runtimeClass.getSimpleName()
      Collection(name, self.map(A.astify).toVector)
    }
  }

  given map[K: Show as K, V: Show as V]: Show[Map[K, V]] with {

    def astify(self: Map[K, V]): AST = {
      Collection(
        "Map",
        self
          .map((key, value) => Product("Entry", VectorMap("key" -> K.astify(key), "value" -> V.astify(value))))
          .toVector
      )
    }
  }

  given option[A: Show as A]: Show[Option[A]] with {
    def astify(self: Option[A]): AST =
      self match
        case None        => Text("None")
        case Some(value) => Collection("Some", Vector(A.astify(value)))
  }

  given either[E: Show as E, A: Show as A]: Show[Either[E, A]] with {
    def astify(self: Either[E, A]): AST =
      self match
        case Left(value)  => Collection("Left", Vector(E.astify(value)))
        case Right(value) => Collection("Right", Vector(A.astify(value)))
  }

  inline def derived[A: Mirror.Of as A]: Show[A] =
    inline A match {
      case given Mirror.ProductOf[A] => product
      case given Mirror.SumOf[A]     => coproduct
    }

  private[chanterelle] class ForProduct[A](tpeName: String, fieldNames: List[String], _instances: => IArray[Show[Any]])
      extends Show[A] {
    private lazy val instances = _instances
    def astify(self: A): AST = {
      val prod = self.asInstanceOf[scala.Product]
      val fields = fieldNames
        .zip(instances)
        .zip(prod.productIterator)
        .map {
          case label -> show -> value =>
            label -> show.astify(value)
        }
        .to(VectorMap)

      Product(tpeName, fields)
    }
  }

  private inline def product[A: Mirror.ProductOf as A]: Show[A] = {
    val tpeName =
      inline erasedValue[A] match {
        case _: NamedTuple.AnyNamedTuple => ""
        case _: Tuple                    => ""
        case _                           => constValue[A.MirroredLabel].toString
      }
    val fieldNames = inline constValueTuple[A.MirroredElemLabels].toList match { case fields: List[String] => fields }
    def instances = summonAll[Tuple.Map[A.MirroredElemTypes, Show]].toIArray.map(_.asInstanceOf[Show[Any]])
    ForProduct(tpeName, fieldNames, instances)
  }

  private[chanterelle] class ForCoproduct[A: Mirror.SumOf as A](instances: Vector[Show[Any]]) extends Show[A] {
    def astify(self: A): AST = {
      val ordinal = A.ordinal(self)
      instances(ordinal).astify(self)
    }
  }

  private inline def coproduct[A: Mirror.SumOf as A]: Show[A] = ForCoproduct(deriveForAll[A.MirroredElemTypes].toVector)

  private inline def deriveForAll[Tup <: Tuple]: List[Show[Any]] =
    inline erasedValue[Tup] match {
      case _: (head *: tail) =>
        // TODO: Doesn't take into account existing instances, getting stack overflows when trying to do that for some reason
        derived[head](using summonInline[Mirror.Of[head]]).asInstanceOf[Show[Any]] :: deriveForAll[tail]
      case _: EmptyTuple => Nil
    }
}

private[chanterelle] transparent trait LowPriorityShow {
  inline given tuple[A <: Tuple](using Mirror.ProductOf[A]): Show[A] = Show.derived[A]
}

package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.VectorMap
import chanterelle.internal.Transformation.NamedSpecificConfigured

sealed trait Transformation derives Debug {

  def calculateTpe(using Quotes): Type[?]

  final def applyModifier(modifier: Modifier)(using Quotes): Transformation = {
    def recurse(segments: List[Path.Segment], curr: Transformation)(using Quotes): Transformation = {
      import quotes.reflect.*
      (segments, curr) match {
        case (Path.Segment.Field(name = name) :: next, t: Transformation.Named) =>
          t.update(name, recurse(next, _))
        case (Path.Segment.TupleElement(index = index) :: next, t: Transformation.Tuple) =>
          t.update(index, recurse(next, _))
        case (Path.Segment.Element(tpe) :: next, t: Transformation.Optional) =>
          t.update(recurse(next, _))
        case (Path.Segment.Element(tpe) :: next, t: Transformation.Collection) =>
          t.update(recurse(next, _))
        case (Nil, t) => apply(modifier, t)
        case (p, t)   => report.errorAndAbort(s"Illegal path segment and transformation combo: ${Debug.show(t)}")
      }
    }

    def apply(modifier: Modifier, transformation: Transformation)(using Quotes): Transformation = {
      import quotes.reflect.*
      (modifier, transformation) match {
        case (mod: Modifier.Add, t: Transformation.Named) =>
          t.withModifiedField(mod.fieldName, Transformation.OfField.FromModifier(NamedSpecificConfigured.Add(mod.fieldName, mod.value.asTerm.tpe.asType, mod.value)))
        // case (m: Modifier.Compute, t: Transformation.Named) => ???
        case (m: Modifier.Remove, t: Transformation.Named) =>
          t.withoutField(m.fieldToRemove)

        case (m: Modifier.Update, _) =>
          Transformation.ConfedUp(Transformation.Configured.Update(m.tpe, m.function))
        // case (
        //       Modifier.Update(tpe = tpe, segmentToUpdate = Path.Segment.Field(_, name), function = fn),
        //       t: Transformation.Named
        //     ) =>
        //   t.withModifiedField(name, Transformation.OfField.FromModifier(NamedSpecificConfigured.Update(tpe, fn)))

        // case (
        //       Modifier.Update(tpe = tpe, segmentToUpdate = Path.Segment.TupleElement(_, idx), function = fn),
        //       t: Transformation.Tuple
        //     ) =>
        //   t.withModifiedElement(idx, Transformation.OfField.FromModifier(NamedSpecificConfigured.Update(tpe, fn)))

        // case (Modifier.Update(tpe = tpe, segmentToUpdate = Path.Segment.Element(_), function = fn), t: Transformation.Optional) =>
        //   ??? // TODO

        // case (
        //       Modifier.Update(tpe = tpe, segmentToUpdate = Path.Segment.Element(_), function = fn),
        //       t: Transformation.Collection
        //     ) =>
        //   ??? // TODO

      }
    }
    recurse(modifier.path.segments.toList, this)
  }
}

object Transformation {

  def fromStructure(structure: Structure): Transformation = {
    structure match {
      case named: Structure.Named =>
        Named(named, named.fields.map { (name, field) => name -> Transformation.OfField.FromSource(name, fromStructure(field)) })

      case tuple: Structure.Tuple =>
        Tuple(
          tuple,
          tuple.elements.zipWithIndex.map { (field, idx) => Transformation.OfField.FromSource(idx, fromStructure(field)) }
        )

      case optional: Structure.Optional =>
        Optional(optional, fromStructure(optional.paramStruct))

      case coll: Structure.Collection =>
        Collection(coll, fromStructure(coll.paramStruct))

      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named(
    source: Structure.Named,
    fields: VectorMap[String, Transformation.OfField[String]]
  ) extends Transformation {

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    def calculateValuesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(
        fields.map {
          case _ -> OfField.FromSource(idx, transformation) => transformation.calculateTpe.repr
          case _ -> OfField.FromModifier(conf)              => conf.tpe.repr
        }.toVector
      )

    def calculateTpe(using Quotes): Type[? <: NamedTuple.AnyNamedTuple] = {
      val values = calculateValuesTpe
      val names = calculateNamesTpe
      ((names, values): @unchecked) match {
        case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
          Type.of[NamedTuple.NamedTuple[names, values]]
      }
    }

    def update(name: String, f: Transformation => Transformation)(using Quotes): Named = {
      import quotes.reflect.*
      val fieldTransformation @ Transformation.OfField.FromSource(idx, transformation) =
        this.fields.getOrElse(name, report.errorAndAbort(s"No field ${name}")): @unchecked // TODO: temporary
      this.copy(fields = this.fields.updated(name, fieldTransformation.copy(transformation = f(transformation))))
    }

    def withModifiedFields(fields: VectorMap[String, Transformation.OfField[Nothing]]): Named =
      this.copy(fields = this.fields ++ fields)

    def withModifiedField(name: String, transformation: Transformation.OfField[Nothing]): Named =
      this.copy(fields = this.fields.updated(name, transformation)) // this will uhhh... create a new record if it doesn't exist

    def withoutField(name: String): Named =
      this.copy(fields = this.fields - name)
  }

  case class Tuple(
    source: Structure.Tuple,
    fields: Vector[Transformation.OfField[Int]]
  ) extends Transformation {

    def calculateTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(
        fields.map {
          case OfField.FromSource(idx, transformation) => transformation.calculateTpe.repr
          case OfField.FromModifier(conf)              => conf.tpe.repr
        }
      )

    def update(index: Int, f: Transformation => Transformation): Tuple = {
      val t @ Transformation.OfField.FromSource(idx, transformation) = fields(index): @unchecked // TODO: temporary
      this.copy(fields = fields.updated(index, t.copy(transformation = f(transformation))))
    }

    def withModifiedElement(idx: Int, transformation: Transformation.OfField[Nothing]): Tuple =
      this.copy(fields = fields.updated(idx, transformation))
  }

  case class Optional(
    source: Structure.Optional,
    paramTransformation: Transformation
  ) extends Transformation {
    def calculateTpe(using Quotes): Type[? <: Option[?]] =
      paramTransformation.calculateTpe match {
        case '[tpe] => Type.of[Option[tpe]]
      }

    def update(f: Transformation => Transformation): Optional =
      this.copy(paramTransformation = f(paramTransformation))

  }

  case class Collection(
    source: Structure.Collection,
    paramTransformation: Transformation
  ) extends Transformation {
    def calculateTpe(using Quotes): Type[? <: Iterable[?]] =
      paramTransformation.calculateTpe match {
        case '[tpe] =>
          import quotes.reflect.*
          AppliedType(source.collectionTpe.repr, TypeRepr.of[tpe] :: Nil).asType.asInstanceOf[Type[? <: Iterable[?]]]
      }

    def update(f: Transformation => Transformation): Collection =
      this.copy(paramTransformation = f(paramTransformation))
  }

  case class Leaf(output: Structure.Leaf) extends Transformation {
    def calculateTpe(using Quotes): Type[?] = output.tpe
  }

  case class ConfedUp(config: Configured) extends Transformation {
    def calculateTpe(using Quotes): Type[?] = config.tpe
  }

  enum OfField[+Idx <: Int | String] derives Debug {
    case FromSource(idx: Idx, transformation: Transformation)
    case FromModifier(modifier: NamedSpecificConfigured) extends OfField[Nothing]
  }

  enum NamedSpecificConfigured derives Debug {
    def tpe: Type[?]

    case Add(
      fieldName: String,
      tpe: Type[?],
      value: Expr[?]
    )
  }

  enum Configured derives Debug {
    def tpe: Type[?]

    case Update(
      tpe: Type[?],
      fn: Expr[? => ?]
    )
  }

  private def rollupTuple(using Quotes)(elements: Vector[quotes.reflect.TypeRepr]) = {
    import quotes.reflect.*

    elements.size match {
      case 0 => Type.of[EmptyTuple]
      case 1 =>
        elements.head.asType.match { case '[tpe] => Type.of[Tuple1[tpe]] }
      case size if size <= 22 =>
        defn
          .TupleClass(size)
          .typeRef
          .appliedTo(elements.toList)
          .asType
          .match { case '[type tpe <: scala.Tuple; tpe] => Type.of[tpe] }
      case _ =>
        val TupleCons = TypeRepr.of[*:]
        val tpe = elements.foldRight(TypeRepr.of[EmptyTuple])((curr, acc) => TupleCons.appliedTo(curr :: acc :: Nil))
        tpe.asType.match { case '[type tpe <: scala.Tuple; tpe] => Type.of[tpe] }
    }
  }
}

package chanterelle.internal

import scala.quoted.*
import scala.collection.immutable.VectorMap
import scala.collection.mutable.ArrayBuffer

sealed trait ModifiableTransformation derives Debug {

  def calculateTpe(using Quotes): Type[?]

  final def applyModifier(modifier: Modifier)(using Quotes): ModifiableTransformation = {
    def recurse(segments: List[Path.Segment], curr: ModifiableTransformation)(using Quotes): ModifiableTransformation = {
      import quotes.reflect.*
      (segments, curr) match {
        case (Path.Segment.Field(name = name) :: next, t: ModifiableTransformation.Named) =>
          t.update(name, recurse(next, _))
        case (Path.Segment.TupleElement(index = index) :: next, t: ModifiableTransformation.Tuple) =>
          t.update(index, recurse(next, _))
        case (Path.Segment.Element(tpe) :: next, t: ModifiableTransformation.Optional) =>
          t.update(recurse(next, _))
        case (Path.Segment.Element(tpe) :: next, t: ModifiableTransformation.Iter[coll]) =>
          t.update(recurse(next, _))
        case (Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 0) :: next, t: ModifiableTransformation.Map[map]) =>
          t.updateKey(recurse(next, _))
        case (Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 1) :: next, t: ModifiableTransformation.Map[map]) =>
          t.updateValue(recurse(next, _))
        case (Nil, t) => apply(modifier, t)
        case (p, t)   => report.errorAndAbort(s"Illegal path segment and transformation combo: ${Debug.show(t)}")
      }
    }

    def apply(modifier: Modifier, transformation: ModifiableTransformation)(using Quotes): ModifiableTransformation = {
      import quotes.reflect.*
      (modifier, transformation) match {
        case (mod: Modifier.Add, t: ModifiableTransformation.Named) =>
          t.withModifiedField(
            mod.valueStructure.fieldName,
            ModifiableTransformation.OfField.FromModifier(Configured.NamedSpecific.Add(mod.valueStructure, mod.value))
          )
        case (mod: Modifier.Compute, t: ModifiableTransformation.Named) =>
          t.withModifiedField(
            mod.valueStructure.fieldName,
            ModifiableTransformation.OfField.FromModifier(Configured.NamedSpecific.Compute(mod.valueStructure, mod.value))
          )
        case (Modifier.Remove(fieldToRemove = name: String), t: ModifiableTransformation.Named) =>
          // IMO this should merely mark a field as deleted so we don't need to mess around with indices later on, same for named deletes
          t.withoutField(name)
        case (Modifier.Remove(fieldToRemove = idx: Int), t: ModifiableTransformation.Tuple) =>
          t.withoutField(idx)
        case (m: Modifier.Update, _) =>
          ModifiableTransformation.ConfedUp(Configured.Update(m.tpe, m.function))
      }
    }
    recurse(modifier.path.segments.toList, this)
  }
}

object ModifiableTransformation {

  def fromStructure(structure: Structure): ModifiableTransformation = {
    structure match {
      case named: Structure.Named =>
        Named(named, named.fields.map { (name, field) => name -> ModifiableTransformation.OfField.FromSource(name, fromStructure(field), false) })

      case tuple: Structure.Tuple =>
        Tuple(tuple, tuple.elements.map(fromStructure))

      case optional: Structure.Optional =>
        Optional(optional, fromStructure(optional.paramStruct))

      case coll: Structure.Collection =>
        coll.repr match
          case source @ Structure.Collection.Repr.Map(tycon, key, value) =>
            ModifiableTransformation.Map(source, fromStructure(key), fromStructure(value))
          case source @ Structure.Collection.Repr.Iter(tycon, element) =>
            ModifiableTransformation.Iter(source, fromStructure(element))
      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named(
    source: Structure.Named,
    fields: VectorMap[String, ModifiableTransformation.OfField]
  ) extends ModifiableTransformation {

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    def calculateValuesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(
        fields.map {
          case _ -> OfField.FromSource(_, transformation, _) => transformation.calculateTpe.repr
          case _ -> OfField.FromModifier(conf)            => conf.tpe.repr
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

    def update(name: String, f: ModifiableTransformation => ModifiableTransformation)(using Quotes): Named = {
      import quotes.reflect.*
      val fieldTransformation @ ModifiableTransformation.OfField.FromSource(idx, transformation, _) =
        this.fields.getOrElse(name, report.errorAndAbort(s"No field ${name}")): @unchecked // TODO: temporary
      this.copy(fields = this.fields.updated(name, fieldTransformation.copy(transformation = f(transformation), removed = false)))
    }

    def withModifiedFields(fields: VectorMap[String, ModifiableTransformation.OfField]): Named =
      this.copy(fields = this.fields ++ fields)

    def withModifiedField(name: String, transformation: ModifiableTransformation.OfField): Named =
      this.copy(fields = this.fields.updated(name, transformation)) // this will uhhh... create a new record if it doesn't exist

    def withoutField(name: String): Named =
      this.copy(fields = this.fields - name)
  }

  case class Tuple(
    source: Structure.Tuple,
    fields: Vector[ModifiableTransformation]
  ) extends ModifiableTransformation {

    def calculateTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.map(_.calculateTpe.repr))

    def update(index: Int, f: ModifiableTransformation => ModifiableTransformation): Tuple = {
      this.copy(fields = fields.updated(index, f(fields(index))))
    }

    def withModifiedElement(idx: Int, transformation: ModifiableTransformation): Tuple =
      this.copy(fields = fields.updated(idx, transformation))

    def withoutField(index: Int): Tuple = {
      val (prefix, suffix) = fields.splitAt(index)
      this.copy(fields = prefix ++ suffix.drop(1))
    }

  }

  case class Optional(
    source: Structure.Optional,
    paramTransformation: ModifiableTransformation
  ) extends ModifiableTransformation {
    def calculateTpe(using Quotes): Type[? <: Option[?]] =
      paramTransformation.calculateTpe match {
        case '[tpe] => Type.of[Option[tpe]]
      }

    def update(f: ModifiableTransformation => ModifiableTransformation): Optional =
      this.copy(paramTransformation = f(paramTransformation))

  }

  case class Map[F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.Map[F],
    key: ModifiableTransformation,
    value: ModifiableTransformation
  ) extends ModifiableTransformation {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, key.calculateTpe, value.calculateTpe): @unchecked) match {
        case ('[type map[k, v]; map], '[key], '[value]) => Type.of[map[key, value]]
      }
    }

    def updateKey(f: ModifiableTransformation => ModifiableTransformation): Map[F] =
      this.copy(key = f(key))

    def updateValue(f: ModifiableTransformation => ModifiableTransformation): Map[F] =
      this.copy(value = f(value))
  }

  case class Iter[F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.Iter[F],
    elem: ModifiableTransformation
  ) extends ModifiableTransformation {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, elem.calculateTpe): @unchecked) match {
        case ('[type coll[a]; coll], '[elem]) =>
          Type.of[coll[elem]]
      }
    }

    def update(f: ModifiableTransformation => ModifiableTransformation): Iter[F] =
      this.copy(elem = f(elem))
  }

  case class Leaf(output: Structure.Leaf) extends ModifiableTransformation {
    def calculateTpe(using Quotes): Type[?] = output.tpe
  }

  case class ConfedUp(config: Configured) extends ModifiableTransformation {
    def calculateTpe(using Quotes): Type[?] = config.tpe
  }

  enum OfField derives Debug {
    case FromSource(name: String, transformation: ModifiableTransformation, removed: Boolean)
    case FromModifier(modifier: Configured.NamedSpecific)
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

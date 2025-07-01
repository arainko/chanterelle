package chanterelle.internal

import scala.collection.immutable.SortedMap
import scala.collection.immutable.VectorMap
import scala.quoted.*

sealed trait ModifiableTransformation derives Debug {

  final inline def narrow[A <: ModifiableTransformation](using Quotes)[Res](inline fn: A => Res) =
    narrowAll[Res](_.when[A](fn))

  final inline def narrowAll[Res](
    inline fns: PartialMatch.type => PartialMatch[ModifiableTransformation, Res]*
  )(using Quotes) = partialMatch(this)(fns*)(errs => quotes.reflect.report.errorAndAbort(s"dupal :( $errs"))

  def calculateTpe(using Quotes): Type[?]

  final def applyModifier(modifier: Modifier)(using Quotes): ModifiableTransformation = {
    def recurse(segments: List[Path.Segment])(curr: ModifiableTransformation)(using Quotes): ModifiableTransformation = {
      segments match {
        case Path.Segment.Field(name = name) :: next =>
          curr.narrow[ModifiableTransformation.Named](_.update(name, recurse(next)))

        case Path.Segment.TupleElement(index = index) :: next =>
          curr.narrow[ModifiableTransformation.Tuple](_.update(index, recurse(next)))

        case Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 0) :: next =>
          curr.narrow[ModifiableTransformation.Map[?]](_.updateKey(recurse(next)))

        case Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 1) :: next =>
          curr.narrow[ModifiableTransformation.Map[?]](_.updateValue(recurse(next)))

        case Path.Segment.Element(tpe) :: next =>
          curr.narrowAll(
            _.when[ModifiableTransformation.Optional](_.update(recurse(next))),
            _.when[ModifiableTransformation.Iter[?]](_.update(recurse(next)))
          )

        case Nil => apply(modifier, curr)
      }
    }

    def apply(modifier: Modifier, transformation: ModifiableTransformation)(using Quotes): ModifiableTransformation = {
      modifier match {
        case m: Modifier.Add =>
          transformation.narrow[ModifiableTransformation.Named](
            _.withModifiedField(
              m.valueStructure.fieldName,
              ModifiableTransformation.OfField.FromModifier(Configured.NamedSpecific.Add(m.valueStructure, m.value), false)
            )
          )

        case m: Modifier.Compute =>
          transformation.narrow[ModifiableTransformation.Named](
            _.withModifiedField(
              m.valueStructure.fieldName,
              ModifiableTransformation.OfField.FromModifier(Configured.NamedSpecific.Compute(m.valueStructure, m.value), false)
            )
          )

        case Modifier.Remove(fieldToRemove = name: String) =>
          transformation.narrow[ModifiableTransformation.Named](_.withoutField(name))

        case Modifier.Remove(fieldToRemove = idx: Int) =>
          transformation.narrow[ModifiableTransformation.Tuple](_.withoutField(idx))
          
        case m: Modifier.Update =>
          ModifiableTransformation.ConfedUp(Configured.Update(m.tpe, m.function))
      }
    }
    recurse(modifier.path.segments.toList)(this)
  }
}

object ModifiableTransformation {

  def create(structure: Structure): ModifiableTransformation = {
    structure match {
      case named: Structure.Named =>
        Named(
          named,
          named.fields.map { (name, field) =>
            name -> ModifiableTransformation.OfField.FromSource(name, create(field), false)
          }
        )

      case tuple: Structure.Tuple =>
        Tuple(
          tuple,
          tuple.elements.zipWithIndex.map((t, idx) => idx -> (transformation = create(t), removed = false)).to(SortedMap)
        )

      case optional: Structure.Optional =>
        Optional(optional, create(optional.paramStruct))

      case coll: Structure.Collection =>
        coll.repr match
          case source @ Structure.Collection.Repr.Map(tycon, key, value) =>
            ModifiableTransformation.Map(source, create(key), create(value))
          case source @ Structure.Collection.Repr.Iter(tycon, element) =>
            ModifiableTransformation.Iter(source, create(element))
      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named(
    source: Structure.Named,
    private val allFields: VectorMap[String, OfField]
  ) extends ModifiableTransformation {
    final def _2 = fields
    val fields: VectorMap[String, OfField] = allFields.filter((_, t) => !t.removed)

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    def calculateValuesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(
        fields.map {
          case _ -> OfField.FromSource(transformation = t) => t.calculateTpe.repr
          case _ -> OfField.FromModifier(modifier = conf)  => conf.tpe.repr
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
        this.allFields.getOrElse(name, report.errorAndAbort(s"No field ${name}")): @unchecked // TODO: temporary
      this.copy(allFields =
        this.allFields.updated(name, fieldTransformation.copy(transformation = f(transformation), removed = false))
      )
    }

    def withModifiedFields(fields: VectorMap[String, ModifiableTransformation.OfField]): Named =
      this.copy(allFields = this.allFields ++ fields)

    def withModifiedField(name: String, transformation: ModifiableTransformation.OfField): Named =
      this.copy(allFields =
        this.allFields.updated(name, transformation)
      ) // this will uhhh... create a new record if it doesn't exist

    def withoutField(name: String)(using Quotes): Named =
      this.copy(allFields = this.allFields.updatedWith(name) {
        case Some(src: ModifiableTransformation.OfField.FromSource)   => Some(src.copy(removed = true))
        case Some(mod: ModifiableTransformation.OfField.FromModifier) => Some(mod.copy(removed = true))
        case None => quotes.reflect.report.errorAndAbort(s"no field named ${name}")
      })
  }

  case class Tuple(
    source: Structure.Tuple,
    private val allFields: SortedMap[Int, (transformation: ModifiableTransformation, removed: Boolean)]
  ) extends ModifiableTransformation {
    final def _2 = fields
    val fields = allFields.collect { case (idx, (transformation = t, removed = false)) => idx -> t }

    def calculateTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.map { case (_, value) => value.calculateTpe.repr }.toVector)

    def update(index: Int, f: ModifiableTransformation => ModifiableTransformation)(using Quotes): Tuple = {
      val (transformation, _) =
        allFields.applyOrElse(index, idx => quotes.reflect.report.errorAndAbort(s"No field defined at index ${idx}"))
      this.copy(allFields = allFields + (index -> (transformation = f(transformation), removed = false)))
    }

    // TODO: check for availability under that index
    def withModifiedElement(idx: Int, transformation: ModifiableTransformation)(using Quotes): Tuple = {
      update(idx, _ => transformation)
    }

    def withoutField(index: Int)(using Quotes): Tuple = {
      val (transformation, _) =
        allFields.applyOrElse(index, idx => quotes.reflect.report.errorAndAbort(s"No field defined at index ${idx}"))
      this.copy(allFields = allFields + (index -> (transformation = transformation, removed = true)))
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
    def removed: Boolean

    case FromSource(name: String, transformation: ModifiableTransformation, removed: Boolean)
    case FromModifier(modifier: Configured.NamedSpecific, removed: Boolean)
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

package chanterelle.internal

import scala.collection.immutable.SortedMap
import scala.collection.immutable.VectorMap
import scala.quoted.*
import scala.annotation.nowarn
import chanterelle.internal.ModifiableTransformation.Named
import chanterelle.internal.ModifiableTransformation.Optional
import chanterelle.internal.ModifiableTransformation.Iter
import chanterelle.internal.ModifiableTransformation.Leaf
import chanterelle.internal.ModifiableTransformation.ConfedUp
import chanterelle.internal.ModifiableTransformation.OfField

case object Err
type Err = Err.type

sealed trait ModifiableTransformation[+E <: Err] {

  @nowarn("msg=Unreachable case except for null")
  final inline def narrow[A <: ModifiableTransformation[Err]](using Quotes)[Res](inline fn: A => Res) =
    this match {
      case a: A => fn(a)
      case _    => quotes.reflect.report.errorAndAbort("ehh :(")
    }

  @nowarn("msg=Unreachable case except for null")
  final inline def narrowedAll[A <: ModifiableTransformation[Err], B <: ModifiableTransformation[Err], Res](
    inline fnA: A => Res,
    inline fnB: B => Res
  )(using Quotes): Res =
    this match {
      case a: A => fnA(a)
      case b: B => fnB(b)
      case _    => quotes.reflect.report.errorAndAbort("ehg")
    }

  def calculateTpe(using Quotes): Type[?]

  final def applyModifier(modifier: Modifier)(using Quotes): ModifiableTransformation[Err] = {
    def recurse(
      segments: List[Path.Segment]
    )(curr: ModifiableTransformation[Err])(using Quotes): ModifiableTransformation[Err] = {
      segments match {
        case Path.Segment.Field(name = name) :: next =>
          curr.narrow[ModifiableTransformation.Named[Err]](_.update(name, recurse(next)))

        case Path.Segment.TupleElement(index = index) :: next =>
          curr.narrow[ModifiableTransformation.Tuple[Err]](_.update(index, recurse(next)))

        case Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 0) :: next =>
          curr.narrow[ModifiableTransformation.Map[Err, ?]](_.updateKey(recurse(next)))

        case Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 1) :: next =>
          curr.narrow[ModifiableTransformation.Map[Err, ?]](_.updateValue(recurse(next)))

        case Path.Segment.Element(tpe) :: next =>
          curr.narrowedAll(
            when[ModifiableTransformation.Optional[Err]](_.update(recurse(next))),
            when[ModifiableTransformation.Iter[Err, ?]](_.update(recurse(next)))
          )

        case Nil => apply(modifier, curr)
      }
    }

    def apply(modifier: Modifier, transformation: ModifiableTransformation[Err])(using Quotes): ModifiableTransformation[Err] = {
      modifier match {
        case m: Modifier.Add =>
          transformation.narrow[ModifiableTransformation.Named[Err]](
            _.withModifiedField(
              m.valueStructure.fieldName,
              ModifiableTransformation.OfField.FromModifier(Configured.NamedSpecific.Add(m.valueStructure, m.value), false)
            )
          )

        case m: Modifier.Compute =>
          transformation.narrow[ModifiableTransformation.Named[Err]](
            _.withModifiedField(
              m.valueStructure.fieldName,
              ModifiableTransformation.OfField.FromModifier(Configured.NamedSpecific.Compute(m.valueStructure, m.value), false)
            )
          )

        case Modifier.Remove(fieldToRemove = name: String) =>
          transformation.narrow[ModifiableTransformation.Named[Err]](_.withoutField(name))

        case Modifier.Remove(fieldToRemove = idx: Int) =>
          transformation.narrow[ModifiableTransformation.Tuple[Err]](_.withoutField(idx))

        case m: Modifier.Update =>
          ModifiableTransformation.ConfedUp(Configured.Update(m.tpe, m.function))
      }
    }
    recurse(modifier.path.segments.toList)(this)
  }

  def refine: Either[List[ModifiableTransformation.Error], ModifiableTransformation[Nothing]] = {
    def recurse(
      stack: List[ModifiableTransformation[E]],
      acc: List[ModifiableTransformation.Error]
    ): Either[List[ModifiableTransformation.Error], ModifiableTransformation[Nothing]] =
      stack match {
        case head :: tail =>
          head match
            case ModifiableTransformation.Named(source, allFields) =>
              val transformations =
                allFields.values.collect { case OfField.FromSource(name, transformation, removed) => transformation }.toList
              recurse(transformations ::: tail, acc)
            case ModifiableTransformation.Tuple(source, allFields) =>
              recurse(allFields.values.toList ::: tail, acc)
            case ModifiableTransformation.Optional(source, paramTransformation) =>
              recurse(paramTransformation :: tail, acc)
            case ModifiableTransformation.Map(source, key, value) =>
              recurse(value :: tail, acc)
            case ModifiableTransformation.Iter(source, elem) =>
              recurse(elem :: tail, acc)
            case ModifiableTransformation.Leaf(output) =>
              recurse(tail, acc)
            case ModifiableTransformation.ConfedUp(config) =>
              recurse(tail, acc)
            case err @ ModifiableTransformation.Error(message) =>
              recurse(tail, err :: acc)

        case Nil => if acc.isEmpty then Right(this.asInstanceOf[ModifiableTransformation[Nothing]]) else Left(acc)
      }

    recurse(this :: Nil, Nil)
  }
}

object ModifiableTransformation {

  given Debug[ModifiableTransformation[Err]] = Debug.derived

  def create(structure: Structure): ModifiableTransformation[Err] = {
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

  case class Named[+E <: Err](
    source: Structure.Named,
    private val allFields: VectorMap[String, OfField[E]]
  ) extends ModifiableTransformation[E] {
    final def _2 = fields
    val fields: VectorMap[String, OfField[E]] = allFields.filter((_, t) => !t.removed)

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

    def update(name: String, f: ModifiableTransformation[E] => ModifiableTransformation[Err])(using Quotes): Named[Err] = {
      import quotes.reflect.*
      val fieldTransformation @ ModifiableTransformation.OfField.FromSource(idx, transformation, _) =
        this.allFields.getOrElse(name, report.errorAndAbort(s"No field ${name}")): @unchecked // TODO: temporary
      this.copy(allFields =
        this.allFields.updated(name, fieldTransformation.copy(transformation = f(transformation), removed = false))
      )
    }

    def withModifiedFields(fields: VectorMap[String, ModifiableTransformation.OfField[Err]]): Named[Err] =
      this.copy(allFields = this.allFields ++ fields)

    def withModifiedField(name: String, transformation: ModifiableTransformation.OfField[Err]): Named[Err] =
      this.copy(allFields =
        this.allFields.updated(name, transformation)
      ) // this will uhhh... create a new record if it doesn't exist

    def withoutField(name: String)(using Quotes): Named[E] =
      this.copy(allFields = this.allFields.updatedWith(name) {
        case Some(src: ModifiableTransformation.OfField.FromSource[E]) => Some(src.copy(removed = true))
        case Some(mod: ModifiableTransformation.OfField.FromModifier)  => Some(mod.copy(removed = true))
        case None => quotes.reflect.report.errorAndAbort(s"no field named ${name}")
      })
  }

  case class Tuple[+E <: Err](
    source: Structure.Tuple,
    private val allFields: SortedMap[Int, (transformation: ModifiableTransformation[E], removed: Boolean)]
  ) extends ModifiableTransformation[E] {
    final def _2 = fields
    val fields = allFields.collect { case (idx, (transformation = t, removed = false)) => idx -> t }

    def calculateTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.map { case (_, value) => value.calculateTpe.repr }.toVector)

    def update(index: Int, f: ModifiableTransformation[E] => ModifiableTransformation[Err])(using Quotes): Tuple[Err] = {
      val (transformation, _) =
        allFields.applyOrElse(index, idx => quotes.reflect.report.errorAndAbort(s"No field defined at index ${idx}"))
      this.copy(allFields = allFields + (index -> (transformation = f(transformation), removed = false)))
    }

    // TODO: check for availability under that index
    def withModifiedElement(idx: Int, transformation: ModifiableTransformation[Err])(using Quotes): Tuple[Err] = {
      update(idx, _ => transformation)
    }

    def withoutField(index: Int)(using Quotes): Tuple[E] = {
      val (transformation, _) =
        allFields.applyOrElse(index, idx => quotes.reflect.report.errorAndAbort(s"No field defined at index ${idx}"))
      this.copy(allFields = allFields + (index -> (transformation = transformation, removed = true)))
    }
  }

  case class Optional[+E <: Err](
    source: Structure.Optional,
    paramTransformation: ModifiableTransformation[E]
  ) extends ModifiableTransformation[E] {
    def calculateTpe(using Quotes): Type[? <: Option[?]] =
      paramTransformation.calculateTpe match {
        case '[tpe] => Type.of[Option[tpe]]
      }

    def update(f: ModifiableTransformation[E] => ModifiableTransformation[Err]): Optional[Err] =
      this.copy(paramTransformation = f(paramTransformation))

  }

  case class Map[+E <: Err, F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.Map[F],
    key: ModifiableTransformation[E],
    value: ModifiableTransformation[E]
  ) extends ModifiableTransformation[E] {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, key.calculateTpe, value.calculateTpe): @unchecked) match {
        case ('[type map[k, v]; map], '[key], '[value]) => Type.of[map[key, value]]
      }
    }

    def updateKey(f: ModifiableTransformation[E] => ModifiableTransformation[Err]): Map[Err, F] =
      this.copy(key = f(key))

    def updateValue(f: ModifiableTransformation[E] => ModifiableTransformation[Err]): Map[Err, F] =
      this.copy(value = f(value))
  }

  case class Iter[+E <: Err, F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.Iter[F],
    elem: ModifiableTransformation[E]
  ) extends ModifiableTransformation[E] {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, elem.calculateTpe): @unchecked) match {
        case ('[type coll[a]; coll], '[elem]) =>
          Type.of[coll[elem]]
      }
    }

    def update(f: ModifiableTransformation[E] => ModifiableTransformation[Err]): Iter[Err, F] =
      this.copy(elem = f(elem))
  }

  case class Leaf(output: Structure.Leaf) extends ModifiableTransformation[Nothing] {
    def calculateTpe(using Quotes): Type[?] = output.tpe
  }

  case class ConfedUp(config: Configured) extends ModifiableTransformation[Nothing] {
    def calculateTpe(using Quotes): Type[?] = config.tpe
  }

  case class Error(message: ErrorMessage) extends ModifiableTransformation[Err] {
    // TODO: make calculateTpe an extension on ModifiableTransformation[Nothing]
    def calculateTpe(using Quotes): Type[? <: AnyKind] = Type.of[Nothing]
  }

  enum OfField[+E <: Err] derives Debug {
    def removed: Boolean

    case FromSource(name: String, transformation: ModifiableTransformation[E], removed: Boolean) extends OfField[E]
    case FromModifier(modifier: Configured.NamedSpecific, removed: Boolean) extends OfField[Nothing]
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

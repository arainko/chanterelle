package chanterelle.internal

import chanterelle.internal.Transformation.IsModified

import scala.annotation.nowarn
import scala.collection.immutable.{ SortedMap, VectorMap }
import scala.quoted.*
import scala.deriving.Mirror
import Transformation.Error

private[chanterelle] case object Err
private[chanterelle] type Err = Err.type

private[chanterelle] sealed abstract class Transformation[+E <: Err](val readableName: String) {

  @nowarn("msg=Unreachable")
  final inline def narrow[A <: Transformation[Err]](
    inline fn: A => Transformation[Err]
  )(inline errorMessage: Transformation[Err] => ErrorMessage): Transformation[Err] =
    this match {
      case a: A  => fn(a)
      case other => Error(errorMessage(other))
    }

  @nowarn("msg=Unreachable")
  final inline def narrow[A <: Transformation[Err], B <: Transformation[Err]](
    inline fnA: A => Transformation[Err],
    inline fnB: B => Transformation[Err]
  )(inline errorMessage: Transformation[Err] => ErrorMessage): Transformation[Err] =
    this match {
      case a: A  => fnA(a)
      case b: B  => fnB(b)
      case other => Error(errorMessage(other))
    }

  def calculateTpe(using Quotes): Type[?]

  def isModified: IsModified

  final def applyModifier(modifier: Modifier)(using Quotes): Transformation[Err] = {
    def recurse(
      segments: List[Path.Segment]
    )(curr: Transformation[Err])(using Quotes): Transformation[Err] = {
      segments match {
        case Path.Segment.Field(name = name) :: next =>
          curr.narrow[Transformation.Named[Err]](_.update(name, recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("named tuple", other, modifier.span)
          )

        case Path.Segment.TupleElement(index = index) :: next =>
          curr.narrow[Transformation.Tuple[Err]](_.update(index, recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("tuple", other, modifier.span)
          )

        case Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 0) :: next =>
          curr.narrow[Transformation.MapLike[Err, ?]](_.updateKey(recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("map", other, modifier.span)
          )

        case Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 1) :: next =>
          curr.narrow[Transformation.MapLike[Err, ?]](_.updateValue(recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("map", other, modifier.span)
          )

        case Path.Segment.Element(tpe) :: next =>
          curr.narrow(
            when[Transformation.Optional[Err]](_.update(recurse(next))),
            when[Transformation.IterLike[Err, ?]](_.update(recurse(next)))
          )(other => ErrorMessage.UnexpectedTransformation("option or collection", other, modifier.span))

        case Path.Segment.LeftElement(tpe) :: next =>
          curr.narrow[Transformation.Either[Err]](_.updateLeft(recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("either", other, modifier.span)
          )

        case Path.Segment.RightElement(tpe) :: next =>
          curr.narrow[Transformation.Either[Err]](_.updateRight(recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("either", other, modifier.span)
          )

        case Nil => apply(modifier, curr)
      }
    }

    def apply(modifier: Modifier, transformation: Transformation[Err]): Transformation[Err] = {
      modifier match {
        case m: Modifier.Put =>
          transformation.narrow[Transformation.Named[Err]](
            _.withModifiedField(
              m.valueStructure.fieldName,
              Transformation.Field.FromModifier(Configured.NamedSpecific.Add(m.valueStructure, m.value))
            )
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, modifier.span))

        case m: Modifier.Compute =>
          transformation.narrow[Transformation.Named[Err]](
            _.withModifiedField(
              m.valueStructure.fieldName,
              Transformation.Field.FromModifier(Configured.NamedSpecific.Compute(m.valueStructure, m.value))
            )
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, modifier.span))

        case Modifier.Remove(fieldToRemove = name: String) =>
          transformation.narrow[Transformation.Named[Err]](_.withoutField(name))(other =>
            ErrorMessage.UnexpectedTransformation("named tuple", other, modifier.span)
          )

        case Modifier.Remove(fieldToRemove = idx: Int) =>
          transformation.narrow[Transformation.Tuple[Err]](_.withoutField(idx))(other =>
            ErrorMessage.UnexpectedTransformation("tuple", other, modifier.span)
          )

        case m: Modifier.Update =>
          Transformation.ConfedUp(Configured.Update(m.tpe, m.function), m.span)

        case m: Modifier.Rename =>
          Transformation.renameNamedNodes(transformation, m.fieldName, m.kind)

        case m: Modifier.Merge =>
          transformation.narrow[Transformation.Named[Err]](transformation =>
            Transformation.Merged.create(transformation, m.valueStructure, m.ref) // TODO: add case for Transformation.Merged
          )(other => ErrorMessage.UnexpectedTransformation("named tuple or merged", other, modifier.span))
      }
    }
    recurse(modifier.path.segments.toList)(this)
  }

  def refine: Either[List[ErrorMessage], Transformation[Nothing]] = {
    def recurse(
      stack: List[Transformation[E]],
      acc: List[ErrorMessage]
    ): Either[List[ErrorMessage], Transformation[Nothing]] =
      stack match {
        case head :: tail =>
          head match
            case Transformation.Named(source, allFields, _) =>
              val transformations =
                allFields.values.collect {
                  case Transformation.Field.FromSource(name, transformation) => transformation
                }.toList
              recurse(transformations ::: tail, acc)
            case Transformation.Tuple(source, allFields, _) =>
              recurse(allFields.values.toList ::: tail, acc)
            case Transformation.Optional(source, paramTransformation, _) =>
              recurse(paramTransformation :: tail, acc)
            case Transformation.Either(source, left, right, _) =>
              recurse(left :: right :: tail, acc)
            case Transformation.MapLike(source, key, value, _) =>
              recurse(value :: tail, acc)
            case Transformation.IterLike(source, elem, _) =>
              recurse(elem :: tail, acc)
            case Transformation.Leaf(output) =>
              recurse(tail, acc)
            case Transformation.ConfedUp(config, span) =>
              recurse(tail, acc)
            case err @ Transformation.Error(message) =>
              recurse(tail, err.message :: acc)

        case Nil => if acc.isEmpty then Right(this.asInstanceOf[Transformation[Nothing]]) else Left(acc)
      }

    recurse(this :: Nil, Nil)
  }
}

private[chanterelle] object Transformation {
  type Exact[Struct <: Structure] <: Transformation[Nothing] =
    Struct match {
      case Structure.Named      => Transformation.Named[Nothing]
      case Structure.Tuple      => Transformation.Tuple[Nothing]
      case Structure.Optional   => Transformation.Optional[Nothing]
      case Structure.Either     => Transformation.Either[Nothing]
      case Structure.Collection =>
        Transformation.MapLike[Nothing, scala.collection.Map] | Transformation.IterLike[Nothing, Iterable]
      case Structure.Leaf => Leaf
    }

  given Debug[Transformation[Err]] = Debug.derived

  def create(structure: Structure): Transformation[Nothing] = createExact(structure)

  def createExact(structure: Structure): Transformation.Exact[structure.type] = {
    structure match {
      case named: Structure.Named =>
        Named(
          named,
          named.fields.map { (name, field) =>
            name -> (field = Transformation.Field.FromSource(name, createExact(field)), removed = false)
          },
          IsModified.No
        )

      case tuple: Structure.Tuple =>
        Tuple(
          tuple,
          tuple.elements.zipWithIndex.map((t, idx) => idx -> (transformation = createExact(t), removed = false)).to(SortedMap),
          IsModified.No
        )

      case optional: Structure.Optional =>
        Optional(optional, createExact(optional.paramStruct), IsModified.No)

      case either: Structure.Either =>
        Either(either, createExact(either.left), createExact(either.right), IsModified.No)

      case coll: Structure.Collection =>
        coll.repr match
          case source @ Structure.Collection.Repr.MapLike(tycon, key, value) =>
            Transformation.MapLike(source, createExact(key), createExact(value), IsModified.No)
          case source @ Structure.Collection.Repr.IterLike(tycon, element) =>
            Transformation.IterLike(source, createExact(element), IsModified.No)
      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named[+E <: Err](
    source: Structure.Named,
    private[Transformation] val allFields: VectorMap[String, (field: Field[E], removed: Boolean)],
    isModified: IsModified
  ) extends Transformation[E]("named tuple") {
    final def _2 = fields
    val fields: VectorMap[String, Field[E]] = allFields.collect { case (key, (field = value, removed = false)) => key -> value }

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    def calculateValuesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.map((_, field) => field.calculateTpe.repr).toVector)

    def calculateTpe(using Quotes): Type[? <: NamedTuple.AnyNamedTuple] = {
      val values = calculateValuesTpe
      val names = calculateNamesTpe
      ((names, values): @unchecked) match {
        case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
          Type.of[NamedTuple.NamedTuple[names, values]]
      }
    }

    def updateAll(fn: (String, Field[E]) => (String, Field[Err])): Named[Err] = {
      val updatedFields =
        this.allFields.map {
          case name -> (field, removed) =>
            val (updatedName, updatedField) = fn(name, field)
            updatedName -> (updatedField, removed)
        }

      this.copy(allFields = updatedFields, isModified = IsModified.Yes)
    }

    def update(name: String, f: Transformation[E] => Transformation[Err]): Named[Err] = {
      val fieldTransformation =
        this.allFields.andThen {
          case (field = field @ Field.FromSource(name, transformation)) =>
            (field = field.copy(transformation = f(transformation)), removed = false)
          case (field = Field.FromModifier(_)) =>
            (field = Field.error(name, ErrorMessage.AlreadyConfigured(name)), removed = false)
        }
          .applyOrElse(name, name => (field = Field.error(name, ErrorMessage.NoFieldFound(name)), removed = false))
      this.copy(
        allFields = this.allFields.updated(name, fieldTransformation),
        isModified = IsModified.Yes
      )
    }

    def withModifiedFields(fields: VectorMap[String, Transformation.Field[Err]]): Named[Err] =
      this.copy(
        allFields = this.allFields ++ fields.transform((_, v) => (field = v, removed = false)),
        isModified = IsModified.Yes
      )

    def withModifiedField(name: String, transformation: Transformation.Field[Err]): Named[Err] =
      // this will uhhh... create a new record if it doesn't exist
      this.copy(
        allFields = this.allFields.updated(name, (field = transformation, removed = false)),
        isModified = IsModified.Yes
      )

    def withoutField(name: String): Named[Err] =
      this.copy(
        allFields = this.allFields.updatedWith(name) {
          case Some((field = src)) => Some((field = src, removed = true))
          case None                => Some((field = Field.error(name, ErrorMessage.NoFieldFound(name)), removed = false))
        },
        isModified = IsModified.Yes
      )
  }

  case class Merged[+E <: Err](
    source: Structure.Named,
    mergees: VectorMap[Sources.Ref, Structure.Named],
    fields: VectorMap[String, Merged.Field[E]]
  ) extends Transformation[E]("merged") {

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    def calculateValuesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.map((_, field) => field.calculateTpe.repr).toVector)

    def calculateTpe(using Quotes): Type[? <: NamedTuple.AnyNamedTuple] = {
      val values = calculateValuesTpe
      val names = calculateNamesTpe
      ((names, values): @unchecked) match {
        case ('[type names <: scala.Tuple; names], '[type values <: scala.Tuple; values]) =>
          Type.of[NamedTuple.NamedTuple[names, values]]
      }
    }

    override val isModified: IsModified = IsModified.Yes

  }

  object Merged {
    def create(source: Transformation.Named[Err], mergee: Structure.Named, ref: Sources.Ref): Transformation.Merged[Err] = {
      val mutualKeys = source.allFields.keySet.intersect(mergee.fields.keySet)

      val overriddenTransformations = mutualKeys.view.map { name =>
        val mergeeStruct = mergee.fields(name)
        val (field, _) = source.allFields(name)
        val value = (field, mergeeStruct) match {
          case (Transformation.Field.FromSource(`name`, left: Transformation.Named[Err]), right: Structure.Named) =>
            Merged.Field.FromSecondary(name, ref, Set(ref, Sources.Ref.Primary), Merged.create(left, right, ref))
          case (Transformation.Field.FromSource(_, left: Transformation.Named[Err]), right: Structure.Named) =>
            Merged.Field.FromSecondary(name, ref, Set(ref), Merged.create(left, right, ref))
          case (Transformation.Field.FromSource(name, left: Transformation.Merged[Err]), right: Structure.Named) =>
            ???
          case (_, right) =>
            Merged.Field.FromSecondary(name, ref, Set(ref), Transformation.Leaf(right.asLeaf))
        }
        name -> value
      }.toMap

      val additionalTransformations =
        mergee.fields
          .removedAll(mutualKeys)
          .transform((name, struct) => Merged.Field.FromSecondary(name, ref, Set(ref), Transformation.Leaf(struct.asLeaf)))

      val allFields =
        source.allFields.transform((_, value) => Merged.Field.FromPrimary(value.field, value.removed)) ++
          overriddenTransformations ++
          additionalTransformations

      Merged(source.source, VectorMap(ref -> mergee), allFields)
    }

    enum Field[+E <: Err] {
      case FromPrimary(underlying: Transformation.Field[E], removed: Boolean)
      case FromSecondary(
        name: String,
        ref: Sources.Ref,
        accessibleFrom: Set[Sources.Ref],
        transformation: Transformation.Leaf | Transformation.Merged[E]
      )
    }

    object Field {
      extension [E <: Err](self: Field[E]) {
        def calculateTpe(using Quotes): Type[?] =
          self match
            case FromPrimary(underlying, removed)                         => underlying.calculateTpe
            case FromSecondary(name, ref, accessibleFrom, transformation) => transformation.calculateTpe

      }
    }
  }

  case class Tuple[+E <: Err](
    source: Structure.Tuple,
    private val allFields: SortedMap[Int, (transformation: Transformation[E], removed: Boolean)],
    isModified: IsModified
  ) extends Transformation[E]("tuple") {
    final def _2 = fields
    val fields = allFields.collect { case (idx, (transformation = t, removed = false)) => idx -> t }

    def calculateTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.map { case (_, value) => value.calculateTpe.repr }.toVector)

    def updateAll(f: Transformation[E] => Transformation[Err]): Tuple[Err] =
      this.copy(
        allFields = allFields.transform { case (_, (t, removed)) => (f(t), removed) },
        isModified = IsModified.Yes
      )

    def update(index: Int, f: Transformation[E] => Transformation[Err]): Tuple[Err] = {
      val t =
        allFields.andThen { case (transformation, _) => (transformation = f(transformation), removed = false) }
          .applyOrElse(index, idx => (Transformation.Error(ErrorMessage.NoFieldAtIndexFound(idx)), false))
      this.copy(allFields = allFields + (index -> t), isModified = IsModified.Yes)
    }

    def withModifiedElement(idx: Int, transformation: Transformation[Err]): Tuple[Err] =
      update(idx, _ => transformation)

    def withoutField(index: Int): Tuple[Err] = {
      val t: (transformation: Transformation[Err], removed: Boolean) =
        allFields.applyOrElse(
          index,
          idx => (transformation = Transformation.Error(ErrorMessage.NoFieldAtIndexFound(idx)), removed = false)
        )
      this.copy(
        allFields = allFields + (index -> (transformation = t.transformation, removed = true)),
        isModified = IsModified.Yes
      )
    }
  }

  case class Optional[+E <: Err](
    source: Structure.Optional,
    paramTransformation: Transformation[E],
    isModified: IsModified
  ) extends Transformation[E]("option") {
    def calculateTpe(using Quotes): Type[? <: Option[?]] =
      paramTransformation.calculateTpe match {
        case '[tpe] => Type.of[Option[tpe]]
      }

    def update(f: Transformation[E] => Transformation[Err]): Optional[Err] =
      this.copy(paramTransformation = f(paramTransformation), isModified = IsModified.Yes)

  }

  case class Either[+E <: Err](
    source: Structure.Either,
    left: Transformation[E],
    right: Transformation[E],
    isModified: IsModified
  ) extends Transformation[E]("either") {
    def calculateTpe(using Quotes): Type[? <: scala.Either[?, ?]] =
      (left.calculateTpe, right.calculateTpe): @unchecked match {
        case '[left] -> '[right] => Type.of[scala.Either[left, right]]
      }

    def updateLeft(f: Transformation[E] => Transformation[Err]): Either[Err] =
      this.copy(left = f(left), isModified = IsModified.Yes)

    def updateRight(f: Transformation[E] => Transformation[Err]): Either[Err] =
      this.copy(right = f(right), isModified = IsModified.Yes)
  }

  case class MapLike[+E <: Err, F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.MapLike[F],
    key: Transformation[E],
    value: Transformation[E],
    isModified: IsModified
  ) extends Transformation[E]("map") {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, key.calculateTpe, value.calculateTpe): @unchecked) match {
        case ('[type map[k, v]; map], '[key], '[value]) => Type.of[map[key, value]]
      }
    }

    def updateKey(f: Transformation[E] => Transformation[Err]): MapLike[Err, F] =
      this.copy(key = f(key), isModified = IsModified.Yes)

    def updateValue(f: Transformation[E] => Transformation[Err]): MapLike[Err, F] =
      this.copy(value = f(value), isModified = IsModified.Yes)
  }

  case class IterLike[+E <: Err, F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.IterLike[F],
    elem: Transformation[E],
    isModified: IsModified
  ) extends Transformation[E]("iterable") {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, elem.calculateTpe): @unchecked) match {
        case ('[type coll[a]; coll], '[elem]) =>
          Type.of[coll[elem]]
      }
    }

    def update(f: Transformation[E] => Transformation[Err]): IterLike[Err, F] =
      this.copy(elem = f(elem), isModified = IsModified.Yes)
  }

  case class Leaf(output: Structure.Leaf) extends Transformation[Nothing]("ordinary value") {
    def calculateTpe(using Quotes): Type[?] = output.tpe
    val isModified = IsModified.No
  }

  // TODO: change this goofyahh name
  case class ConfedUp(config: Configured, span: Span) extends Transformation[Nothing]("configured value") {
    def calculateTpe(using Quotes): Type[?] = config.tpe
    val isModified = IsModified.Yes
  }

  case class Error(message: ErrorMessage) extends Transformation[Err]("erroneous transformation") {
    // TODO: make calculateTpe an extension on ModifiableTransformation[Nothing]
    def calculateTpe(using Quotes): Type[? <: AnyKind] = Type.of[Nothing]

    val isModified = IsModified.Yes
  }

  @nowarn("msg=unused implicit parameter")
  enum Field[+E <: Err] derives Debug {
    case FromSource(name: String, transformation: Transformation[E]) extends Field[E]
    case FromModifier(modifier: Configured.NamedSpecific) extends Field[Nothing]
  }

  object Field {

    extension [E <: Err](self: Field[E]) {
      def update(f: Transformation[E] => Transformation[Err]): Field[Err] =
        self match {
          case src @ FromSource(transformation = t) => src.copy(transformation = f(t))
          case mod: FromModifier                    => mod
        }

      def calculateTpe(using Quotes): Type[?] = self match
        case FromSource(name, transformation) => transformation.calculateTpe
        case FromModifier(modifier)           => modifier.tpe

    }

    def error(name: String, message: ErrorMessage): Field[Err] =
      Field.FromSource(name, Transformation.Error(message))
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

  enum IsModified derives Debug {
    case Yes, No
  }

  private def renameNamedNodes(
    transformation: Transformation[Err],
    rename: String => String,
    kind: Modifier.Kind
  ): Transformation[Err] = {
    def recurse(curr: Transformation[Err]): Transformation[Err] = curr match {
      case named: Transformation.Named[Err] =>
        named.updateAll((name, field) => rename(name) -> field.update(recurse))
      case tup: Transformation.Tuple[Err] =>
        tup.updateAll(recurse)
      case opt: Transformation.Optional[Err] =>
        opt.update(recurse)
      case either: Transformation.Either[Err] =>
        either.updateLeft(recurse).updateRight(recurse)
      case map: Transformation.MapLike[Err, scala.collection.Map] =>
        map.updateKey(recurse).updateValue(recurse)
      case iter: Transformation.IterLike[Err, Iterable] =>
        iter.update(recurse)
      case leaf: Transformation.Leaf =>
        leaf
      case confed: Transformation.ConfedUp =>
        confed
      case err: Transformation.Error =>
        err
    }

    def locally(curr: Transformation[Err]): Transformation[Err] = curr match {
      case named: Transformation.Named[Err] =>
        named.updateAll((name, field) => rename(name) -> field)
      case other => other
    }

    if kind.isLocal then locally(transformation) else recurse(transformation)
  }
}

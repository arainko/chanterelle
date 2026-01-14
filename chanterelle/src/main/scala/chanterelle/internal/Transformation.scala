package chanterelle.internal

import chanterelle.internal.Transformation.IsModified

import scala.annotation.nowarn
import scala.collection.immutable.{ SortedMap, VectorMap }
import scala.quoted.*

private[chanterelle] case object Err
private[chanterelle] type Err = Err.type

private[chanterelle] sealed abstract class Transformation[+E <: Err](val readableName: String) {

  @nowarn("msg=Unreachable")
  final inline def narrow[A <: Transformation[Err]](
    inline fn: A => Transformation[Err]
  )(inline errorMessage: Transformation[Err] => ErrorMessage): Transformation[Err] =
    this match {
      case a: A  => fn(a)
      case other => Transformation.Error(errorMessage(other))
    }

  @nowarn("msg=Unreachable")
  final inline def narrow[A <: Transformation[Err], B <: Transformation[Err]](
    inline fnA: A => Transformation[Err],
    inline fnB: B => Transformation[Err]
  )(inline errorMessage: Transformation[Err] => ErrorMessage): Transformation[Err] =
    this match {
      case a: A  => fnA(a)
      case b: B  => fnB(b)
      case other => Transformation.Error(errorMessage(other))
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
          curr.narrow[Transformation.Map[Err, ?]](_.updateKey(recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("map", other, modifier.span)
          )

        case Path.Segment.Element(tpe) :: Path.Segment.TupleElement(_, 1) :: next =>
          curr.narrow[Transformation.Map[Err, ?]](_.updateValue(recurse(next)))(other =>
            ErrorMessage.UnexpectedTransformation("map", other, modifier.span)
          )

        case Path.Segment.Element(tpe) :: next =>
          curr.narrow(
            when[Transformation.Optional[Err]](_.update(recurse(next))),
            when[Transformation.Iter[Err, ?]](_.update(recurse(next)))
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
              Transformation.OfField.FromModifier(Configured.NamedSpecific.Add(m.valueStructure, m.value))
            )
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, modifier.span))

        case m: Modifier.Compute =>
          transformation.narrow[Transformation.Named[Err]](
            _.withModifiedField(
              m.valueStructure.fieldName,
              Transformation.OfField.FromModifier(Configured.NamedSpecific.Compute(m.valueStructure, m.value))
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
                  case Transformation.OfField.FromSource(name, transformation) => transformation
                }.toList
              recurse(transformations ::: tail, acc)
            case Transformation.Tuple(source, allFields, _) =>
              recurse(allFields.values.toList ::: tail, acc)
            case Transformation.Optional(source, paramTransformation, _) =>
              recurse(paramTransformation :: tail, acc)
            case Transformation.Either(source, left, right, _) =>
              recurse(left :: right :: tail, acc)
            case Transformation.Map(source, key, value, _) =>
              recurse(value :: tail, acc)
            case Transformation.Iter(source, elem, _) =>
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

object Transformation {

  given Debug[Transformation[Err]] = Debug.derived

  def create(structure: Structure): Transformation[Err] = {
    structure match {
      case named: Structure.Named =>
        Named(
          named,
          named.fields.map { (name, field) =>
            name -> (field = Transformation.OfField.FromSource(name, create(field)), removed = false)
          },
          IsModified.No
        )

      case tuple: Structure.Tuple =>
        Tuple(
          tuple,
          tuple.elements.zipWithIndex.map((t, idx) => idx -> (transformation = create(t), removed = false)).to(SortedMap),
          IsModified.No
        )

      case optional: Structure.Optional =>
        Optional(optional, create(optional.paramStruct), IsModified.No)

      case either: Structure.Either =>
        Either(either, create(either.left), create(either.right), IsModified.No)

      case coll: Structure.Collection =>
        coll.repr match
          case source @ Structure.Collection.Repr.Map(tycon, key, value) =>
            Transformation.Map(source, create(key), create(value), IsModified.No)
          case source @ Structure.Collection.Repr.Iter(tycon, element) =>
            Transformation.Iter(source, create(element), IsModified.No)
      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named[+E <: Err](
    source: Structure.Named,
    private val allFields: VectorMap[String, (field: OfField[E], removed: Boolean)],
    isModified: IsModified
  ) extends Transformation[E]("named tuple") {
    final def _2 = fields
    val fields: VectorMap[String, OfField[E]] = allFields.collect { case (key, (field = value, removed = false)) => key -> value }

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

    def updateAll(fn: (String, OfField[E]) => (String, OfField[Err])): Named[Err] = {
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
          case (field = field @ OfField.FromSource(name, transformation)) =>
            (field = field.copy(transformation = f(transformation)), removed = false)
          case (field = OfField.FromModifier(_)) =>
            (field = OfField.error(name, ErrorMessage.AlreadyConfigured(name)), removed = false)
        }
          .applyOrElse(name, name => (field = OfField.error(name, ErrorMessage.NoFieldFound(name)), removed = false))
      this.copy(
        allFields = this.allFields.updated(name, fieldTransformation),
        isModified = IsModified.Yes
      )
    }

    def withModifiedFields(fields: VectorMap[String, Transformation.OfField[Err]]): Named[Err] =
      this.copy(
        allFields = this.allFields ++ fields.transform((_, v) => (field = v, removed = false)),
        isModified = IsModified.Yes
      )

    def withModifiedField(name: String, transformation: Transformation.OfField[Err]): Named[Err] =
      // this will uhhh... create a new record if it doesn't exist
      this.copy(
        allFields = this.allFields.updated(name, (field = transformation, removed = false)),
        isModified = IsModified.Yes
      )

    def withoutField(name: String): Named[Err] =
      this.copy(
        allFields = this.allFields.updatedWith(name) {
          case Some((field = src)) => Some((field = src, removed = true))
          case None                => Some((field = OfField.error(name, ErrorMessage.NoFieldFound(name)), removed = false))
        },
        isModified = IsModified.Yes
      )
  }

  opaque type SourceRef = Int

  object SourceRef {
    private var counter = 0

    val Primary: SourceRef = -1

    def fresh(): SourceRef = {
      val current = counter
      counter += 1
      current
    }
  }

  case class Merged[+E <: Err](
    source: Structure.Named,
    mergees: VectorMap[SourceRef, Structure.Named],
    private val allFields: VectorMap[String, (field: OfField[E], sourceRef: SourceRef, accessibleFrom: Vector[SourceRef], removed: Boolean)]
  ) extends Transformation[E]("merged") {

    final def _3 = fields
    val fields: VectorMap[String, (field: OfField[E], sourceRef: SourceRef, accessibleFrom: Vector[SourceRef])] = allFields.collect {
      case (key, (field = value, sourceRef = idx, accessibleFrom = af, removed = false)) => key -> (value, idx, af)
    }

    def calculateNamesTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.keys.map(name => quotes.reflect.ConstantType(quotes.reflect.StringConstant(name))))

    override def calculateTpe(using Quotes): Type[? <: AnyKind] =
      rollupTuple(
        fields.map {
          case _ -> (field = OfField.FromSource(transformation = t)) => t.calculateTpe.repr
          case _ -> (field = OfField.FromModifier(modifier = conf))  => conf.tpe.repr
        }.toVector
      )

    override val isModified: IsModified = IsModified.Yes

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

  case class Map[+E <: Err, F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.Map[F],
    key: Transformation[E],
    value: Transformation[E],
    isModified: IsModified
  ) extends Transformation[E]("map") {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, key.calculateTpe, value.calculateTpe): @unchecked) match {
        case ('[type map[k, v]; map], '[key], '[value]) => Type.of[map[key, value]]
      }
    }

    def updateKey(f: Transformation[E] => Transformation[Err]): Map[Err, F] =
      this.copy(key = f(key), isModified = IsModified.Yes)

    def updateValue(f: Transformation[E] => Transformation[Err]): Map[Err, F] =
      this.copy(value = f(value), isModified = IsModified.Yes)
  }

  case class Iter[+E <: Err, F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.Iter[F],
    elem: Transformation[E],
    isModified: IsModified
  ) extends Transformation[E]("iterable") {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, elem.calculateTpe): @unchecked) match {
        case ('[type coll[a]; coll], '[elem]) =>
          Type.of[coll[elem]]
      }
    }

    def update(f: Transformation[E] => Transformation[Err]): Iter[Err, F] =
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
  enum OfField[+E <: Err] derives Debug {
    case FromSource(name: String, transformation: Transformation[E]) extends OfField[E]
    case FromModifier(modifier: Configured.NamedSpecific) extends OfField[Nothing]
  }

  object OfField {

    extension [E <: Err](self: OfField[E]) {
      def update(f: Transformation[E] => Transformation[Err]): OfField[Err] =
        self match {
          case src @ FromSource(transformation = t) => src.copy(transformation = f(t))
          case mod: FromModifier                    => mod
        }
    }

    def error(name: String, message: ErrorMessage): OfField[Err] =
      OfField.FromSource(name, Transformation.Error(message))
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
      case map: Transformation.Map[Err, scala.collection.Map] =>
        map.updateKey(recurse).updateValue(recurse)
      case iter: Transformation.Iter[Err, Iterable] =>
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

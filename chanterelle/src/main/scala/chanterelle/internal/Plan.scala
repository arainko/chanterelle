package chanterelle.internal

import chanterelle.internal.Plan.IsModified

import scala.annotation.nowarn
import scala.collection.immutable.{ SortedMap, VectorMap }
import scala.quoted.*
import Plan.Error
import chanterelle.internal.Plan.Merged.Field
import scala.annotation.tailrec

private[chanterelle] case object Err
private[chanterelle] type Err = Err.type

private[chanterelle] sealed abstract class Plan[+E <: Err](val readableName: String) {

  @nowarn("msg=Unreachable")
  final inline def narrow[A <: Plan[Err]](
    inline fn: A => Plan[Err]
  )(inline errorMessage: Plan[Err] => ErrorMessage): Plan[Err] =
    this match {
      case a: A  => fn(a)
      case other => Error(errorMessage(other))
    }

  @nowarn("msg=Unreachable")
  final inline def narrow[A <: Plan[Err], B <: Plan[Err]](
    inline fnA: A => Plan[Err],
    inline fnB: B => Plan[Err]
  )(inline errorMessage: Plan[Err] => ErrorMessage): Plan[Err] =
    this match {
      case a: A  => fnA(a)
      case b: B  => fnB(b)
      case other => Error(errorMessage(other))
    }

  def calculateTpe(using Quotes): Type[?]

  def isModified: IsModified

  final def applyModifier(modifier: Modifier)(using Quotes): Plan[Err] = {
    def recurse(
      segments: List[Path.Segment],
      traversed: Path
    )(curr: Plan[Err])(using Quotes): Plan[Err] = {
      segments match {
        case (seg @ Path.Segment.Field(name = name)) :: next =>
          val traversedPath = traversed :+ seg
          curr.narrow(
            when[Plan.Named[Err]](_.update(name, recurse(next, traversedPath), modifier.span)),
            when[Plan.Merged[Err]](_.update(name, recurse(next, traversedPath), modifier.span))
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, traversedPath, modifier.span))

        case (seg @ Path.Segment.TupleElement(index = index)) :: next =>
          val traversedPath = traversed :+ seg

          curr.narrow[Plan.Tuple[Err]](_.update(index, recurse(next, traversedPath)))(other =>
            ErrorMessage.UnexpectedTransformation("tuple", other, traversedPath, modifier.span)
          )

        case (elem @ Path.Segment.Element(tpe)) :: (zero @ Path.Segment.TupleElement(_, 0)) :: next =>
          val traversedPath = traversed :+ elem :+ zero
          curr.narrow[Plan.MapLike[Err, ?]](_.updateKey(recurse(next, traversedPath)))(other =>
            ErrorMessage.UnexpectedTransformation("map", other, traversedPath, modifier.span)
          )

        case (elem @ Path.Segment.Element(tpe)) :: (one @ Path.Segment.TupleElement(_, 1)) :: next =>
          val traversedPath = traversed :+ elem :+ one

          curr.narrow[Plan.MapLike[Err, ?]](_.updateValue(recurse(next, traversedPath)))(other =>
            ErrorMessage.UnexpectedTransformation("map", other, traversedPath, modifier.span)
          )

        case (elem @ Path.Segment.Element(tpe)) :: next =>
          val traversedPath = traversed :+ elem

          curr.narrow(
            when[Plan.Optional[Err]](_.update(recurse(next, traversedPath))),
            when[Plan.IterLike[Err, ?]](_.update(recurse(next, traversedPath)))
          )(other => ErrorMessage.UnexpectedTransformation("option or collection", other, traversedPath, modifier.span))

        case (elem @ Path.Segment.LeftElement(tpe)) :: next =>
          val traversedPath = traversed :+ elem

          curr.narrow[Plan.Either[Err]](_.updateLeft(recurse(next, traversedPath)))(other =>
            ErrorMessage.UnexpectedTransformation("either", other, traversedPath, modifier.span)
          )

        case (elem @ Path.Segment.RightElement(tpe)) :: next =>
          val traversedPath = traversed :+ elem

          curr.narrow[Plan.Either[Err]](_.updateRight(recurse(next, traversedPath)))(other =>
            ErrorMessage.UnexpectedTransformation("either", other, traversedPath, modifier.span)
          )

        case Nil => apply(modifier, curr, traversed)
      }
    }

    def apply(modifier: Modifier, transformation: Plan[Err], traversedPath: Path): Plan[Err] = {
      modifier match {
        case m: Modifier.Put =>
          transformation.narrow[Plan.Named[Err]](
            _.withModifiedField(
              m.valueStructure.fieldName,
              Plan.Field.FromModifier(Configured.NamedSpecific.Add(m.valueStructure, m.value))
            )
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, traversedPath, modifier.span))

        case m: Modifier.Compute =>
          transformation.narrow[Plan.Named[Err]](
            _.withModifiedField(
              m.valueStructure.fieldName,
              Plan.Field.FromModifier(Configured.NamedSpecific.Compute(m.valueStructure, m.value))
            )
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, traversedPath, modifier.span))

        case Modifier.Remove(fieldToRemove = name: String) =>
          transformation.narrow(
            when[Plan.Named[Err]](_.withoutField(name))
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, traversedPath, modifier.span))

        case Modifier.Remove(fieldToRemove = idx: Int) =>
          transformation.narrow[Plan.Tuple[Err]](_.withoutField(idx))(other =>
            ErrorMessage.UnexpectedTransformation("tuple", other, traversedPath, modifier.span)
          )

        case m: Modifier.Update =>
          Plan.ConfedUp(Configured.Update(m.tpe, m.function), m.span)

        case m: Modifier.Rename =>
          Plan.renameNamedNodes(transformation, m.fieldName, m.kind)

        case m: Modifier.Merge =>
          transformation.narrow(
            when[Plan.Named[Err]](transformation => Plan.Merged.create(transformation, m.valueStructure, m.ref)),
            when[Plan.Merged[Err]](_.merge(m.valueStructure, m.ref))
          )(other => ErrorMessage.UnexpectedTransformation("named tuple", other, traversedPath, modifier.span))
      }
    }
    recurse(modifier.path.segments.toList, Path.empty(modifier.path.root))(this)
  }

  def refine: Either[List[ErrorMessage], Plan[Nothing]] = {
    @tailrec def recurse(
      stack: List[Plan[E]],
      acc: List[ErrorMessage]
    ): Either[List[ErrorMessage], Plan[Nothing]] =
      stack match {
        case head :: tail =>
          head match
            case Plan.Named(source, allFields, _) =>
              val plans = List.newBuilder[Plan[E]]

              allFields.foreach {
                case (_, Plan.Field.FromSource(name, transformation)) => plans += transformation
                case (_, Plan.Field.FromModifier(_))                  => ()
              }

              recurse(plans.result() ::: tail, acc)
            case Plan.Merged(_, fields) =>
              val errors = List.newBuilder[ErrorMessage]
              val plans = List.newBuilder[Plan[E]]

              fields.foreach {
                case (_, Plan.Merged.Field.FromPrimary(underlying = Plan.Field.FromSource(plan = plan))) => plans += plan
                case (_, Plan.Merged.Field.FromPrimary(underlying = Plan.Field.FromModifier(_)))         => ()
                case (_, Plan.Merged.Field.FromSecondary(plan = plan))                                   => plans += plan
                case (_, Plan.Merged.Field.Error(err))                                                   => errors += err.message
              }

              recurse(plans.result() ::: tail, acc ::: errors.result())
            case Plan.Tuple(source, allFields, _) =>
              recurse(allFields.values.toList ::: tail, acc)
            case Plan.Optional(source, paramTransformation, _) =>
              recurse(paramTransformation :: tail, acc)
            case Plan.Either(source, left, right, _) =>
              recurse(left :: right :: tail, acc)
            case Plan.MapLike(source, key, value, _) =>
              recurse(value :: tail, acc)
            case Plan.IterLike(source, elem, _) =>
              recurse(elem :: tail, acc)
            case Plan.Leaf(output) =>
              recurse(tail, acc)
            case Plan.ConfedUp(config, span) =>
              recurse(tail, acc)
            case err @ Plan.Error(message) =>
              recurse(tail, err.message :: acc)

        case Nil => if acc.isEmpty then Right(this.asInstanceOf[Plan[Nothing]]) else Left(acc)
      }

    recurse(this :: Nil, Nil)
  }
}

private[chanterelle] object Plan {
  type Exact[Struct <: Structure] <: Plan[Nothing] =
    Struct match {
      case Structure.Named      => Plan.Named[Nothing]
      case Structure.Tuple      => Plan.Tuple[Nothing]
      case Structure.Optional   => Plan.Optional[Nothing]
      case Structure.Either     => Plan.Either[Nothing]
      case Structure.Collection =>
        Plan.MapLike[Nothing, scala.collection.Map] | Plan.IterLike[Nothing, Iterable]
      case Structure.Leaf => Leaf
    }

  given Debug[Plan[Err]] = Debug.derived

  def create(structure: Structure): Plan[Nothing] = createExact(structure)

  def createExact(structure: Structure): Plan.Exact[structure.type] = {
    structure match {
      case named: Structure.Named =>
        Named(
          named,
          named.fields.map { (name, field) =>
            name -> (field = Plan.Field.FromSource(name, createExact(field)), removed = false)
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
            Plan.MapLike(source, createExact(key), createExact(value), IsModified.No)
          case source @ Structure.Collection.Repr.IterLike(tycon, element) =>
            Plan.IterLike(source, createExact(element), IsModified.No)
      case leaf: Structure.Leaf =>
        Leaf(leaf)
    }
  }

  case class Named[+E <: Err](
    source: Structure.Named,
    private val allFields: VectorMap[String, (field: Field[E], removed: Boolean)],
    isModified: IsModified
  ) extends Plan[E]("named tuple") {
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

    def update(name: String, f: Plan[E] => Plan[Err], modifierSpan: Span): Named[Err] = {
      val fieldTransformation =
        this.allFields.andThen {
          case (field = field @ Field.FromSource(name, transformation)) =>
            (field = field.copy(plan = f(transformation)), removed = false)
          case (field = Field.FromModifier(_)) =>
            (field = Field.error(name, ErrorMessage.AlreadyConfigured(name, modifierSpan)), removed = false)
        }
          .applyOrElse(name, name => (field = Field.error(name, ErrorMessage.NoFieldFound(name)), removed = false))
      this.copy(
        allFields = this.allFields.updated(name, fieldTransformation),
        isModified = IsModified.Yes
      )
    }

    def withModifiedFields(fields: VectorMap[String, Plan.Field[Err]]): Named[Err] =
      this.copy(
        allFields = this.allFields ++ fields.transform((_, v) => (field = v, removed = false)),
        isModified = IsModified.Yes
      )

    def withModifiedField(name: String, transformation: Plan.Field[Err]): Named[Err] =
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
    mergees: VectorMap[Sources.Ref, Structure.Named],
    fields: VectorMap[String, Merged.Field[E]]
  ) extends Plan[E]("merged value") {

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

    def update(name: String, f: Plan[E] => Plan[Err], modifierSpan: Span): Merged[Err] = {
      val fieldTransformation =
        this.fields.andThen {
          case field @ Merged.Field.FromPrimary(underlying = Field.FromSource(name, plan)) =>
            field.copy(underlying = Field.FromSource(name, f(plan)), removed = false)
          case field @ Merged.Field.FromPrimary(underlying = Field.FromModifier(_)) =>
            field.copy(underlying = Field.error(name, ErrorMessage.AlreadyConfigured(name, modifierSpan)), removed = false)
          case field: Merged.Field.FromSecondary[E] =>
            // TODO: could also check if input is Leaf then out is also Leaf, same for Merged?
            f(field.plan) match
              case merged @ Merged(_, _) => field.copy(plan = merged)
              case _                     => Merged.Field.Error(Plan.Error(ErrorMessage.CantModifySecondaryField(modifierSpan)))
        }
          .applyOrElse(name, name => Merged.Field.Error(Plan.Error(ErrorMessage.NoFieldFound(name))))

      this.copy(fields = fields.updated(name, fieldTransformation))
    }

    def updateAll(
      f: String => String,
      primaryUpdate: Plan.Field[E] => Plan.Field[Err],
      secondaryMerged: Plan.Merged[E] => Plan.Merged[Err]
    ): Merged[Err] = {
      val updatedFields = fields.map { (name, field) => f(name) -> field.update(primaryUpdate, secondaryMerged) }
      this.copy(fields = updatedFields)
    }

    def merge(mergee: Structure.Named, ref: Sources.Ref): Merged[Err] = {
      val mutualKeys = fields.keySet.intersect(mergee.fields.keySet)

      val overriddenTransformations = mutualKeys.view.map { name =>
        val mergeeStruct = mergee.fields(name)
        val field = fields(name)
        val value = (field, mergeeStruct) match {
          // merge with a field that hasn't been overridden by a merge yet which just happens to point to a named tuple
          case (
                Merged.Field.FromPrimary(source, Field.FromSource(srcName, left: Plan.Named[E]), removed),
                right: Structure.Named
              ) =>
            val refs = if srcName == name then Set(ref, Sources.Ref.Primary) else Set(ref)
            Merged.Field.FromSecondary(name, ref, refs, Merged.create(left, right, ref))

          // merge with a field that hasn't been overridden by a merge yet but points to a field that has been merged in the past
          case (
                Merged.Field.FromPrimary(source, Field.FromSource(srcName, left: Plan.Merged[E]), removed),
                right: Structure.Named
              ) =>
            val refs = if srcName == name then Set(ref, Sources.Ref.Primary) else Set(ref)
            Merged.Field.FromSecondary(name, ref, refs, left.merge(right, ref))

          // merge with a field that is neither a named tuple or a nother merged value -> it's just a run of the mill override from the mergee
          case (Merged.Field.FromPrimary(source, _, removed), right) =>
            Merged.Field.FromSecondary(name, ref, Set(ref), Plan.Leaf(right.asLeaf))

          // merge with a field that has already been overridden and which points to another merged value
          case (
                Merged.Field.FromSecondary(srcName, _, accessibleFrom, plan: Plan.Merged[E]),
                right: Structure.Named
              ) =>
            Merged.Field.FromSecondary(srcName, ref, accessibleFrom + ref, plan.merge(right, ref))

          // merge with a field that has already been overridden and which doesn't point to a merged value
          case (Merged.Field.FromSecondary(srcName, _, accessibleFrom, _), right) =>
            Merged.Field.FromSecondary(srcName, ref, Set(ref), Plan.Leaf(right.asLeaf))

          case (err: Merged.Field.Error, _) => err
        }
        name -> value
      }.toMap

      val additionalTransformations =
        mergee.fields
          .removedAll(mutualKeys)
          .transform {
            case (name, struct: Structure.Named) =>
              Merged.Field.FromSecondary(name, ref, Set(ref), Merged.fromSecondaryNamed(struct, ref))
            case (name, struct) =>
              Merged.Field.FromSecondary(name, ref, Set(ref), Plan.Leaf(struct.asLeaf))
          }

      val allFields =
        fields ++
          overriddenTransformations ++
          additionalTransformations

      Merged(mergees.updated(ref, mergee), allFields)
    }

  }

  object Merged {
    def fromSecondaryNamed(source: Structure.Named, ref: Sources.Ref): Plan.Merged[Nothing] = {
      val fields = source.fields.transform {
        case (name, struct: Structure.Named) =>
          Merged.Field.FromSecondary(name, ref, Set(ref), fromSecondaryNamed(struct, ref))
        case (name, struct) =>
          Merged.Field.FromSecondary(name, ref, Set(ref), Plan.Leaf(struct.asLeaf))
      }

      Merged(VectorMap(ref -> source), fields)
    }

    def create(source: Plan.Named[Err], mergee: Structure.Named, ref: Sources.Ref): Plan.Merged[Err] = {
      val mutualKeys = source.fields.keySet.intersect(mergee.fields.keySet)

      val overriddenTransformations = mutualKeys.view.map { name =>
        val mergeeStruct = mergee.fields(name)
        val field = source.fields(name)
        val value = (field, mergeeStruct) match {
          // merge with a field that hasn't been overridden by a merge yet which just happens to point to a named tuple
          case (Plan.Field.FromSource(srcName, left: Plan.Named[Err]), right: Structure.Named) =>
            val refs = if srcName == name then Set(ref, Sources.Ref.Primary) else Set(ref)
            Merged.Field.FromSecondary(name, ref, refs, Merged.create(left, right, ref))

          // merge with a field that hasn't been overridden by a merge yet but points to a field that has been merged in the past
          case (Plan.Field.FromSource(srcName, left: Plan.Merged[Err]), right: Structure.Named) =>
            val refs = if srcName == name then Set(ref, Sources.Ref.Primary) else Set(ref)
            Merged.Field.FromSecondary(srcName, ref, refs, left.merge(right, ref))

          // everything else becomes a simple override from the mergee
          case (_, right) =>
            Merged.Field.FromSecondary(
              name,
              ref,
              Set(ref),
              Plan.Leaf(right.asLeaf)
            )
        }
        name -> value
      }.toMap

      val additionalTransformations =
        mergee.fields
          .removedAll(mutualKeys)
          .transform {
            case (name, struct: Structure.Named) =>
              Merged.Field.FromSecondary(name, ref, Set(ref), Merged.fromSecondaryNamed(struct, ref))
            case (name, struct) =>
              Merged.Field.FromSecondary(name, ref, Set(ref), Plan.Leaf(struct.asLeaf))
          }

      val allFields =
        source.fields.transform((_, value) => Merged.Field.FromPrimary(source.source, value, false)) ++
          overriddenTransformations ++
          additionalTransformations

      Merged(VectorMap(Sources.Ref.Primary -> source.source, ref -> mergee), allFields)
    }

    enum Field[+E <: Err] {
      case FromPrimary(source: Structure.Named, underlying: Plan.Field[E], removed: Boolean)
      case FromSecondary(
        name: String,
        ref: Sources.Ref,
        accessibleFrom: Set[Sources.Ref],
        plan: Plan.Leaf | Plan.Merged[E]
      )
      case Error(error: Plan.Error) extends Field[Err]

      final def update(
        primaryUpdate: Plan.Field[E] => Plan.Field[Err],
        secondaryMerged: Plan.Merged[E] => Plan.Merged[Err]
      ): Field[Err] =
        this match {
          case primary @ FromPrimary(underlying = field) =>
            primary.copy(underlying = primaryUpdate(field))
          case secondary @ FromSecondary(plan = plan) =>
            secondary.copy(
              plan = plan match {
                case leaf: Plan.Leaf        => leaf
                case merged: Plan.Merged[E] => secondaryMerged(merged)
              }
            )
          case err: Field.Error => err
        }
    }

    object Field {
      extension [E <: Err](self: Field[E]) {
        def calculateTpe(using Quotes): Type[?] =
          self match
            case FromPrimary(_, underlying, removed)                      => underlying.calculateTpe
            case FromSecondary(name, ref, accessibleFrom, transformation) => transformation.calculateTpe
            case Error(error) => Type.of[Nothing] // TODO: make this work only on Field[Nothing]

      }
    }
  }

  case class Tuple[+E <: Err](
    source: Structure.Tuple,
    private val allFields: SortedMap[Int, (transformation: Plan[E], removed: Boolean)],
    isModified: IsModified
  ) extends Plan[E]("tuple") {
    final def _2 = fields
    val fields = allFields.collect { case (idx, (transformation = t, removed = false)) => idx -> t }

    def calculateTpe(using Quotes): Type[? <: scala.Tuple] =
      rollupTuple(fields.map { case (_, value) => value.calculateTpe.repr }.toVector)

    def updateAll(f: Plan[E] => Plan[Err]): Tuple[Err] =
      this.copy(
        allFields = allFields.transform { case (_, (t, removed)) => (f(t), removed) },
        isModified = IsModified.Yes
      )

    def update(index: Int, f: Plan[E] => Plan[Err]): Tuple[Err] = {
      val t =
        allFields.andThen { case (transformation, _) => (transformation = f(transformation), removed = false) }
          .applyOrElse(index, idx => (Plan.Error(ErrorMessage.NoFieldAtIndexFound(idx)), false))
      this.copy(allFields = allFields + (index -> t), isModified = IsModified.Yes)
    }

    def withModifiedElement(idx: Int, transformation: Plan[Err]): Tuple[Err] =
      update(idx, _ => transformation)

    def withoutField(index: Int): Tuple[Err] = {
      val t: (transformation: Plan[Err], removed: Boolean) =
        allFields.applyOrElse(
          index,
          idx => (transformation = Plan.Error(ErrorMessage.NoFieldAtIndexFound(idx)), removed = false)
        )
      this.copy(
        allFields = allFields + (index -> (transformation = t.transformation, removed = true)),
        isModified = IsModified.Yes
      )
    }
  }

  case class Optional[+E <: Err](
    source: Structure.Optional,
    paramTransformation: Plan[E],
    isModified: IsModified
  ) extends Plan[E]("option") {
    def calculateTpe(using Quotes): Type[? <: Option[?]] =
      paramTransformation.calculateTpe match {
        case '[tpe] => Type.of[Option[tpe]]
      }

    def update(f: Plan[E] => Plan[Err]): Optional[Err] =
      this.copy(paramTransformation = f(paramTransformation), isModified = IsModified.Yes)

  }

  case class Either[+E <: Err](
    source: Structure.Either,
    left: Plan[E],
    right: Plan[E],
    isModified: IsModified
  ) extends Plan[E]("either") {
    def calculateTpe(using Quotes): Type[? <: scala.Either[?, ?]] =
      (left.calculateTpe, right.calculateTpe): @unchecked match {
        case '[left] -> '[right] => Type.of[scala.Either[left, right]]
      }

    def updateLeft(f: Plan[E] => Plan[Err]): Either[Err] =
      this.copy(left = f(left), isModified = IsModified.Yes)

    def updateRight(f: Plan[E] => Plan[Err]): Either[Err] =
      this.copy(right = f(right), isModified = IsModified.Yes)
  }

  case class MapLike[+E <: Err, F[k, v] <: collection.Map[k, v]](
    source: Structure.Collection.Repr.MapLike[F],
    key: Plan[E],
    value: Plan[E],
    isModified: IsModified
  ) extends Plan[E]("map") {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, key.calculateTpe, value.calculateTpe): @unchecked) match {
        case ('[type map[k, v]; map], '[key], '[value]) => Type.of[map[key, value]]
      }
    }

    def updateKey(f: Plan[E] => Plan[Err]): MapLike[Err, F] =
      this.copy(key = f(key), isModified = IsModified.Yes)

    def updateValue(f: Plan[E] => Plan[Err]): MapLike[Err, F] =
      this.copy(value = f(value), isModified = IsModified.Yes)
  }

  case class IterLike[+E <: Err, F[elem] <: Iterable[elem]](
    source: Structure.Collection.Repr.IterLike[F],
    elem: Plan[E],
    isModified: IsModified
  ) extends Plan[E]("iterable") {
    def calculateTpe(using Quotes): Type[?] = {
      ((source.tycon, elem.calculateTpe): @unchecked) match {
        case ('[type coll[a]; coll], '[elem]) =>
          Type.of[coll[elem]]
      }
    }

    def update(f: Plan[E] => Plan[Err]): IterLike[Err, F] =
      this.copy(elem = f(elem), isModified = IsModified.Yes)
  }

  case class Leaf(output: Structure.Leaf) extends Plan[Nothing]("ordinary value") {
    def calculateTpe(using Quotes): Type[?] = output.tpe
    val isModified = IsModified.No
  }

  // TODO: change this goofyahh name
  case class ConfedUp(config: Configured, span: Span) extends Plan[Nothing]("configured value") {
    def calculateTpe(using Quotes): Type[?] = config.tpe
    val isModified = IsModified.Yes
  }

  case class Error(message: ErrorMessage) extends Plan[Err]("erroneous transformation") {
    // TODO: make calculateTpe an extension on ModifiableTransformation[Nothing]
    def calculateTpe(using Quotes): Type[? <: AnyKind] = Type.of[Nothing]

    val isModified = IsModified.Yes
  }

  @nowarn("msg=unused implicit parameter")
  enum Field[+E <: Err] derives Debug {
    case FromSource(name: String, plan: Plan[E]) extends Field[E]
    case FromModifier(modifier: Configured.NamedSpecific) extends Field[Nothing]
  }

  object Field {

    extension [E <: Err](self: Field[E]) {
      def update(f: Plan[E] => Plan[Err]): Field[Err] =
        self match {
          case src @ FromSource(plan = p) => src.copy(plan = f(p))
          case mod: FromModifier          => mod
        }

      def calculateTpe(using Quotes): Type[?] = self match
        case FromSource(name, transformation) => transformation.calculateTpe
        case FromModifier(modifier)           => modifier.tpe

    }

    def error(name: String, message: ErrorMessage): Field[Err] =
      Field.FromSource(name, Plan.Error(message))
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
    transformation: Plan[Err],
    rename: String => String,
    kind: Modifier.Kind
  ): Plan[Err] = {
    def recurse(curr: Plan[Err]): Plan[Err] = curr match {
      case named: Plan.Named[Err] =>
        named.updateAll((name, field) => rename(name) -> field.update(recurse))
      case tup: Plan.Tuple[Err] =>
        tup.updateAll(recurse)
      case opt: Plan.Optional[Err] =>
        opt.update(recurse)
      case either: Plan.Either[Err] =>
        either.updateLeft(recurse).updateRight(recurse)
      case map: Plan.MapLike[Err, scala.collection.Map] =>
        map.updateKey(recurse).updateValue(recurse)
      case iter: Plan.IterLike[Err, Iterable] =>
        iter.update(recurse)
      case merged: Plan.Merged[Err] =>
        def update(merged: Plan.Merged[Err]): Plan.Merged[Err] =
          merged.updateAll(rename, _.update(recurse), update)
        update(merged)
      case leaf: Plan.Leaf =>
        leaf
      case confed: Plan.ConfedUp =>
        confed
      case err: Plan.Error =>
        err
    }

    def locally(curr: Plan[Err]): Plan[Err] = curr match {
      case named: Plan.Named[Err] =>
        named.updateAll((name, field) => rename(name) -> field)
      case merged: Plan.Merged[Err] =>
        merged.updateAll(rename, identity, identity)
      case other => other
    }

    if kind.isLocal then locally(transformation) else recurse(transformation)
  }
}

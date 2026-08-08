package chanterelle.internal


import scala.annotation.unused
import scala.quoted.*
import chanterelle.internal.Debug.AST

private[chanterelle] sealed trait WrapperType[F[_]] {
  def wrapper(using Quotes): Type[F]

  def unapply(tpe: Type[?])(using Quotes): Option[(WrapperType[F], Type[?])]
}

private[chanterelle] object WrapperType {
  def create[F[_]: Type](using Quotes): WrapperType[F] = {
    Type.of[F[Any]] match {
      case '[Option[?]] =>
        Optional.asInstanceOf[WrapperType[F]]
      case _ =>
        Wrapped(Type.of[F])
    }
  }

  given Debug[WrapperType[?]] with {
    def astify(self: WrapperType[?])(using Quotes): AST =
      import quotes.reflect.*
      self match
        case Optional            => Debug.AST.Text(s"WrapperType[Option]")
        case Wrapped(wrapperTpe) => Debug.AST.Text(s"WrapperType[${wrapperTpe.repr.show(using Printer.TypeReprShortCode)}]")
  }
  //
  // def unapply(using Quotes)(tpe: Type[?]) =
  //   Context.current match {
  //     case ctx: Context.PossiblyFallible[?] => ctx.wrapperType.unapply(tpe)
  //     case Context.Total(_, _)              => None
  //   }

  case object Optional extends WrapperType[Option] {

    def wrapper(using Quotes): Type[Option] = Type.of[Option]

    override def unapply(tpe: Type[? <: AnyKind])(using Quotes): Option[(WrapperType[Option], Type[?])] = {
      tpe match {
        case '[Option[underlying]] => Some(this -> Type.of[underlying])
        case _                     => None
      }
    }
  }

  final case class Wrapped[F[_]] private[WrapperType] (wrapperTpe: Type[F]) extends WrapperType[F] {
    def wrapper(using Quotes): Type[F] = wrapperTpe

    override def unapply(tpe: Type[? <: AnyKind])(using Quotes): Option[(WrapperType[F], Type[?])] = {
      @unused given Type[F] = wrapperTpe
      tpe match
        case '[F[underlying]] => Some(this -> Type.of[underlying])
        case _                => None 
                              

    }
  }
}

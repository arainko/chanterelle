package chanterelle.internal

import scala.Ordering.Implicits.*
import scala.annotation.nowarn
import scala.quoted.*

@scala.annotation.publicInBinary
private[chanterelle] object Logger {

  object locally {
    inline def apply[A](inline f: locally.type ?=> A): A = f(using this)
  }

  // Logger Config
  private[chanterelle] transparent inline given level: Level = Level.Off

  @scala.annotation.publicInBinary
  private[Logger] val output = Output.StdOut
  @nowarn private def filter(msg: String, loc: String)(using Quotes) =
    Expr.summon[locally.type].isDefined
  enum Level {
    case Off, Debug, Info
  }

  object Level {
    given Ordering[Level] = Ordering.by(_.ordinal)
  }

  enum Output {
    case StdOut, Report

    final def print(msg: String, level: Level)(using Quotes) = {
      import quotes.reflect.*

      def colored(color: String & scala.Singleton)(msg: String) = s"$color$msg${Console.RESET}"
      def green(msg: String) = colored(Console.GREEN)(msg)
      def blue(msg: String) = colored(Console.BLUE)(msg)

      val loc = Thread.currentThread().getStackTrace().lift(2)

      val formattedMacroLoc = loc.map(pos => blue(s" [${pos.getFileName()}:${pos.getLineNumber()}] ")).mkString

      val location =
        Symbol.spliceOwner.pos
          .map(pos => s"${pos.sourceFile.name}:${pos.startLine}:${pos.startColumn}")
          .map(formatted => green(" [" + formatted + "]"))
          .getOrElse("")
      val formatted = s"${green(s"[${level.toString().toUpperCase()}]")}$location$formattedMacroLoc$msg"
      this match {
        case StdOut => if filter(msg, location) then println(formatted)
        case Report => if filter(msg, location) then quotes.reflect.report.info(formatted)
      }
    }
  }

  inline def loggedInfo[A](using
    Quotes
  )(
    inline msg: String
  )(value: A)(using Debug[A]) = {
    info(msg, value)
    value
  }

  inline def info(inline msg: String)(using quotes: Quotes): Unit =
    inline level match {
      case Level.Debug => if level <= Level.Info then output.print(msg, Level.Info)
      case Level.Info  => if level <= Level.Info then output.print(msg, Level.Info)
      case Level.Off   => ()
    }

  inline def info[A](
    inline msg: String,
    value: A
  )(using Debug[A], Quotes): Unit =
    info(s"$msg: ${Debug.show(value)}")

  inline def loggedDebug[A](using
    Quotes
  )(
    inline msg: String
  )(value: A)(using Debug[A]) = {
    debug(msg, value)
    value
  }

  inline def debug(inline msg: String)(using quotes: Quotes): Unit =
    inline level match {
      case Level.Debug => if level <= Level.Debug then output.print(msg, Level.Debug)
      case Level.Info  => if level <= Level.Debug then output.print(msg, Level.Debug)
      case Level.Off   => ()
    }

  inline def debug[A](inline msg: String, value: A)(using
    _debug: Debug[A],
    quotes: Quotes
  ): Unit =
    debug(s"$msg: ${Debug.show(value)}")
}

extension (ctx: StringContext) {
  private[chanterelle] def ds(args: Any*): String = {
    val pi = ctx.parts.iterator
    val ai = args.iterator
    val bldr = new StringBuilder(pi.next())
    while ai.hasNext do {
      bldr.append(ai.next().getClass().getSimpleName())
      bldr.append(pi.next())
    }
    bldr.toString
  }

}

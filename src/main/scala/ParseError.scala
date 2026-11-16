sealed trait ParseError

object ParseError:
  final case class NotANumber(raw: String) extends ParseError
  final case class OutOfRange(n: Int, max: Int) extends ParseError
  final case class UnknownAction(raw: String) extends ParseError
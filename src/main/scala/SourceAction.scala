enum SourceAction:
  case Add
  case Toggle(index: Int)
  case Remove(index: Int)
  case Edit(index: Int)
  case Back

object SourceAction:
  enum Kind:
    case Add, Toggle, Remove, Edit, Back
  
  def parseKind(input: String): Either[ParseError, Kind] =
    input.trim match
      case "1" => Right(Kind.Add)
      case "2" => Right(Kind.Toggle)
      case "3" => Right(Kind.Remove)
      case "4" => Right(Kind.Edit)
      case "0" => Right(Kind.Back)
      case other => Left(ParseError.UnknownAction(other))
  
  def parseIndex(input: String, max: Int): Either[ParseError, Int] =
    val trimmed = input.trim
    trimmed.toIntOption match
      case None => Left(ParseError.NotANumber(trimmed))
      case Some(n) if n < 1 || n > max => Left(ParseError.OutOfRange(n, max))
      case Some(n) => Right(n)
  
  def combine(kind: Kind, index: Int): SourceAction = kind match
    case Kind.Add    => SourceAction.Add
    case Kind.Toggle => SourceAction.Toggle(index)
    case Kind.Remove => SourceAction.Remove(index)
    case Kind.Edit   => SourceAction.Edit(index)
    case Kind.Back   => SourceAction.Back
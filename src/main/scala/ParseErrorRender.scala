object ParseErrorRender:
  def render(err: ParseError): String = err match
    case ParseError.NotANumber(raw) =>
      if raw.isEmpty then "Некорректный пункт меню."
      else s"Некорректный ввод: '$raw' не является числом."
    case ParseError.OutOfRange(n, max) =>
      s"Пункт $n вне допустимого диапазона 0..$max."
    case ParseError.UnknownAction(raw) =>
      s"Неизвестное действие: '$raw'."
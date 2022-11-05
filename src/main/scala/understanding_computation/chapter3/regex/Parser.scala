package understanding_computation.chapter3.regex

class Parser(input: String) {

  var index = 0

  implicit class EitherOps[L,R](self: Either[L,R]) {

    def orElse[S >: R](other: => Either[L,S]): Either[L,S] =
      if (self.isLeft) other else self
  }

  def parseRegex(): Either[String, Pattern] = {
    if(index >= input.length())
      Left("no match")
    else
    parseChoose() orElse
      parseRepeat() orElse
      parseEmpty() orElse
    parseLiteral() orElse
      parseConcatenate()
  }

  def peek(i: Int = index): Option[Char] =
    if(i<input.length()) Some(input(i))
    else None

  def parseEmpty(): Either[String, Empty] = {
    for {
      _ <- parseChar(''').right
      _ <- parseChar(''').right
    } yield Empty
  }

  def parseLiteral(): Either[String, Literal] =
    peek().filter(isLiteral)
      .map { c =>
      index += 1
      Right(Literal(c))
    }.getOrElse(Left("Expected a literal"))

  val reservedChars = Set('(',')','*','|')

  def isLiteral(c: Char): Boolean = !reservedChars.contains(c)

  def parseChar(c: Char): Either[String, Unit] =
    peek()
      .filter(_ == c)
      .map{_ =>
        index += 1
        Right(())
      }
      .getOrElse(Left(s"Expected '$c'"))

  def parseRepeat(): Either[String, Repeat] =
    for {
       _ <- parseChar('(').right
       pattern <- parseRegex().right
       _ <- parseChar(')').right
     } yield Repeat(pattern)

  def parseChoose(): Either[String, Choose] =
    for {
      p1 <- parseRegex().right
      _  <- parseChar('|').right
      p2 <- parseRegex().right
    } yield Choose(p1,p2)

  def parseConcatenate(): Either[String, Concatenate] =
    for {
      p1 <- parseRegex().right
      p2 <- parseRegex().right
    } yield Concatenate(p1,p2)


}

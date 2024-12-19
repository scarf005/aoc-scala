package prelude

val delimiter = raw"\r?\n".r

def dedentImpl(input: String) =
  val lines = delimiter.split(input).dropBothWhile(_.trim.isEmpty)

  val minIndent = lines.iterator
    .filter(_.trim.nonEmpty)
    .map(_.takeWhile(_ == ' ').length)
    .minOption
    .getOrElse(0)

  lines.map(_.drop(minIndent)).mkString("\n")

extension (sc: StringContext)
  def d(args: Any*): String = dedentImpl(sc.s(args*))

extension (string: String) def dedent: String = dedentImpl(string)

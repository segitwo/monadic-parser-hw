object CSVMonadParser:
  class MonadParser[T, Src](private val p: Src => (T, Src)):
    def flatMap[M](f: T => MonadParser[M, Src]): MonadParser[M, Src] = MonadParser { src =>
      val (word, rest) = p(src)
      f(word).p(rest)
    }

    def map[M](f: T => M): MonadParser[M, Src] = MonadParser { src =>
      val (word, rest) = p(src)
      (f(word), rest)
    }

    def parse(src: Src): T = p(src)._1

  object MonadParser:
    def apply[T, Src](p: Src => (T, Src)): MonadParser[T, Src] = new MonadParser(p)

  def stringColumnParser(using wordsSplitter: String): MonadParser[String, String] = MonadParser[String, String] { str =>
    val idx = str.indexOf(wordsSplitter)
    if idx > -1 then
      val word = str.substring(0, idx)
      val rest = str.substring(idx + 1)
      (word, rest)
    else
      (str, "")
  }

  def intColumnParser: MonadParser[Int, String] = stringColumnParser.map(_.toInt)

  def booleanColumnParser: MonadParser[Boolean, String] = stringColumnParser.map(_.toBoolean)

  case class Row(c1: Int, c2: String, c3: Boolean)

  given wordsSplitter: String = ","

  @main def start(): Unit =
    val parser: MonadParser[Row, String] = for
      c1 <- intColumnParser
      c2 <- stringColumnParser
      c3 <- booleanColumnParser
    yield Row(c1, c2, c3)
    end parser

    val src: String = "1,test1,true;2,test2,false;3,test3,true"
    println(src.split(";").map(parser.parse).mkString("Array(", ", ", ")"))
package jp.dip.poketo7878.snow

object HelloWorld {
  def main(args: Array[String]) {
    println(SnowParser.parse("(lambda (x) (+ x 10))"))
    println(SnowParser.parse("(let (f (lambda (x) x)) (f 10))"))
    println(SnowParser.parse("(define x 'y)"))
    println(SnowParser.parse("(lambda (x) (if (zerop x) 0 1))"))
    println(SnowParser.parse("(let (x 10) (+ x 10))"))
    println(SnowParser.parse("(if true 10 11)"))
    println(SnowParser.parse("(let-rec ((x a b c) (+ (+ a b) c)) (x 10 10 10))"))
    println(SnowParser.parse("(if (= 0 1) 'x 'y)"))
    println(SnowParser.parse("(- (+. 1.0 0.5))"))
    val res = SnowParser.parse("(let-rec ((x a b c) (+ (+ a b) c)) (x 10 10 10))").get
    println(Typing.f(res(0)))
  }
}


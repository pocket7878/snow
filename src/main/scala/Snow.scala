package jp.dip.poketo7878.snow

object HelloWorld {
  def main(args: Array[String]) {
    var res = SnowParser.parse("(let-rec ((x a b c) (+ (+ a b) c)) (x 10 10 10))").get
    println(Inline.f(Assoc.f(Beta.f(Alpha.f(KNormal.f(Typing.f(res)))))))
  }
}


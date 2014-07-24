package jp.dip.poketo7878.snow

object HelloWorld {
  def main(args: Array[String]) {
    var res = SnowParser.parse("(let-rec ((x a b c) (+ (+ a b) c)) (x 10 10 10))").get
    println(Elim.f(ConstFold.f(Inline.f(Assoc.f(Beta.f(Alpha.f(KNormal.f(Typing.f(res)))))))))
    res = SnowParser.parse("(let (x 3) (let (y 7) (+ x y)))").get
    println(Elim.f(ConstFold.f(Inline.f(Assoc.f(Beta.f(Alpha.f(KNormal.f(Typing.f(res)))))))))
  }
}


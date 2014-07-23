package jp.dip.poketo7878.snow

import jp.dip.poketo7878.snow.KNormal._

object Beta {

	def fnd(x: KToken, env: Map[KToken, KToken]): KToken = {
		env.get(x).getOrElse(x)
	}

	def g(env: Map[KToken, KToken], ex: KAST): KAST = {
		ex match {
			case KUnit() => KUnit()
			case KLong(l) => KLong(l)
			case KDouble(d) => KDouble(d)
			case KNeg(x) => KNeg(fnd(x, env))
			case KAdd(x, y) => KAdd(fnd(x, env), fnd(y, env))
			case KSub(x, y) => KSub(fnd(x, env), fnd(y, env))
			case KDNeg(x) => KDNeg(fnd(x, env))
			case KDAdd(x, y) => KDAdd(fnd(x, env), fnd(y, env))
			case KDSub(x, y) => KDSub(fnd(x, env), fnd(y, env))
			case KDMul(x, y) => KDMul(fnd(x, env), fnd(y, env))
			case KDDiv(x, y) => KDDiv(fnd(x, env), fnd(y, env))
			case KIfEq(x, y, e1, e2) => KIfEq(fnd(x, env), fnd(y, env), g(env, e1), g(env, e2))
			case KIfLE(x, y, e1, e2) => KIfLE(fnd(x, env), fnd(y, env), g(env, e1), g(env, e2))
			case KLet((x@KToken(name), t), e1, e2) => {
				g(env, e1) match {
					case y@KToken(_) => g(env ++ Map(x -> y), e2)
					case e1d => {
						val e2d = g(env, e2)
						KLet((x, t), e1d, e2d)
					}
				}
			}
			case KLetRec(KFundef((x@KToken(name), t), yts, e1), e2) => {
				KLetRec(KFundef((x, t), yts, g(env, e1)), g(env, e2))
			}
			case x@KToken(_) => fnd(x, env)
			case KApp(x, ys) => KApp(fnd(x, env), ys.map(y => fnd(y, env)))
			case KTuple(xs) => KTuple(xs.map(x => fnd(x, env)))
			case KExtFuncApp(x, ys) => KExtFuncApp(x, ys.map(y => fnd(y, env)))
		}
	}

	def f(ex: KAST): KAST = g(Map(), ex)
}
package jp.dip.poketo7878.snow

import jp.dip.poketo7878.snow.KNormal._
import jp.dip.poketo7878.snow.SnowParser.SType

object Alpha {
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
				val xd = SType.genid(name)
				KLet((KToken(xd.name), t), g(env, e1), g(env ++ Map(x -> KToken(xd.name)), e2))
			}
			case x@KToken(_) => fnd(x, env)
			case KLetRec(KFundef((x@KToken(name), t), yts, e1), e2) => {
				val newEnv = env ++ Map(x -> KToken(SType.genid(name).name))
				val ys = yts.map(y => y._1)
				val envd = env ++ ys.zip(ys.map(y => KToken(SType.genid(y.name).name))).toMap
				KLetRec(KFundef((fnd(x, newEnv),t), yts.map{case (y, t) => (fnd(y, envd), t)}, g(envd, e1)), g(newEnv, e2))
			}
			case KApp(x, ys) => KApp(fnd(x, env), ys.map(y => fnd(y, env)))
			case KTuple(xs) => KTuple(xs.map(x => fnd(x, env)))
			case KExtFuncApp(x, ys) => KExtFuncApp(x, ys.map(y => fnd(y, env)))
		}
	}

	def f(ex: KAST): KAST = g(Map(), ex)
}
package jp.dip.poketo7878.snow

import jp.dip.poketo7878.snow.KNormal._

object ConstFold {
	def meml(x: KToken, env: Map[KToken, KAST]): Boolean = {
		val res = env.get(x)
		res match {
			case Some(KLong(_)) => true
			case _ => false
		}
	}

	def memd(x: KToken, env: Map[KToken, KAST]): Boolean = {
		val res = env.get(x)
		res match {
			case Some(KDouble(_)) => true
			case _ => false
		}
	}

	def memt(x: KToken, env: Map[KToken, KAST]): Boolean = {
		val res = env.get(x)
		res match {
			case Some(KToken(_)) => true
			case _ => false
		}
	}

	def findl(x: KToken, env: Map[KToken, KAST]): Long = {
		env.get(x) match {
			case Some(KLong(l)) => l
			case _ => throw new Exception("Not_found")
		}
	}

	def findd(x: KToken, env: Map[KToken, KAST]): Double = {
		env.get(x) match {
			case Some(KDouble(d)) => d
			case _ => throw new Exception("Not_found")
		}
	}

	def findt(x: KToken, env: Map[KToken, KAST]): List[KToken] = {
		env.get(x) match {
			case Some(KTuple(ys)) => ys
			case _ => throw new Exception("Not_found")
		}
	}

	def g(env: Map[KToken, KAST], ex: KAST): KAST = {
		ex match {
			case x@KToken(_) if meml(x, env) => KLong(findl(x, env))
			case KNeg(x) if meml(x, env) => KLong(findl(x, env))
			case KAdd(x, y) if meml(x, env) && meml(y, env) => KLong(findl(x, env) + findl(y, env))
			case KSub(x, y) if meml(x, env) && meml(y, env) => KLong(findl(x, env) - findl(y, env))
			case KDAdd(x, y) if meml(x, env) && meml(y, env) => KDouble(findl(x, env) + findl(y, env))
			case KDSub(x, y) if meml(x, env) && meml(y, env) => KDouble(findl(x, env) - findl(y, env))
			case KDMul(x, y) if meml(x, env) && meml(y, env) => KDouble(findl(x, env) * findl(y, env))
			case KDDiv(x, y) if meml(x, env) && meml(y, env) => KDouble(findl(x, env) / findl(y, env))
			case KIfEq(x, y, e1, e2) if meml(x, env) && meml(y, env) => {
				if(findl(x, env) == findl(y, env)) {
					g(env, e1)
				} else {
					g(env, e2)
				}
			}
			case KIfEq(x, y, e1, e2) if memd(x, env) && memd(y, env) => {
				if(findd(x, env) == findd(y, env)) {
					g(env, e1)
				} else {
					g(env, e2)
				}
			}
			case KIfEq(x, y, e1, e2) => KIfEq(x, y, g(env, e1), g(env, e2))
			case KIfLE(x, y, e1, e2) if meml(x, env) && meml(y, env) => {
				if(findl(x, env) <= findl(y, env)) {
					g(env, e1)
				} else {
					g(env, e2)
				}
			}
			case KIfLE(x, y, e1, e2) if memd(x, env) && memd(y, env) => {
				if(findd(x, env) <= findd(y, env)) {
					g(env, e1)
				} else {
					g(env, e2)
				}
			}
			case KIfLE(x, y, e1, e2) => KIfLE(x, y, g(env, e1), g(env, e2))

			case KLet((x, t), e1, e2) => {
				val e1d = g(env, e1)
				val e2d = g(env ++ Map(x -> e1d), e2)
				KLet((x, t), e1d, e2d)
			}

			case KLetRec(KFundef(x, ys, e1), e2) => KLetRec(KFundef(x, ys, g(env, e1)), g(env, e2))
			case e => e
		}
	}

	def f(ex: KAST) = g(Map.empty, ex)
}
package jp.dip.poketo7878.snow

import jp.dip.poketo7878.snow.SnowParser._
import jp.dip.poketo7878.snow.KNormal._

object Inline {
	var threshold = 0

	def size(ex: KAST): Int = {
		ex match {
			case KIfEq(_, _, e1, e2) => 1 + size(e1) + size(e2)
			case KIfLE(_, _, e1, e2) => 1 + size(e1) + size(e2)
			case KLet(_, e1, e2) => 1 + size(e1) + size(e2)
			case KLetRec(KFundef(_, _, e1), e2) => 1 + size(e1) + size(e2)
			case _ => 1
		}
	}

	def g(env: Map[KToken, (List[(KToken, SType)], KAST)], ex: KAST): KAST = {
		ex match {
			case KIfEq(x, y, e1, e2) => KIfEq(x, y, g(env, e1), g(env, e2))
			case KIfLE(x, y, e1, e2) => KIfLE(x, y, g(env, e1), g(env, e2))
			case KLet(xt, e1, e2) => KLet(xt, g(env, e1), g(env, e2))
			case KLetRec(KFundef((x, t), yts, e1), e2) => {
				val newEnv = if (size(e1)  > threshold) {
					env
				} else {
					env ++ Map(x -> (yts, e1))
				}
				KLetRec(KFundef((x, t), yts, g(newEnv, e1)), g(newEnv, e2))
			}
			case KApp(x, ys) if env.contains(x) => {
				val (zs, e) = env.get(x).get
				val envd:Map[KToken, KToken] = {
					zs.zip(ys).foldLeft(Map.empty[KToken, KToken])((e, zty) => e ++ Map(zty._1._1 -> zty._2))
				}
				Alpha.g(envd, e)
			}
			case e => e
		}
	}

	def f(ex: KAST) = g(Map(), ex)
}
package jp.dip.poketo7878.snow

import jp.dip.poketo7878.snow.KNormal._

object Assoc {
	def f(ex: KAST): KAST = {
		ex match {
			case KIfEq(x, y, e1, e2) => KIfEq(x, y, f(e1), f(e2))
			case KIfLE(x, y, e1, e2) => KIfLE(x, y, f(e1), f(e2))
			case KLet(xt, e1, e2) => {
				def insert(ex: KAST): KAST = {
					ex match {
						case KLet(yt, e3, e4) => KLet(yt, e3, insert(e4))
						case KLetRec(fundefs, e) => KLetRec(fundefs, insert(e))
						case e => KLet(xt, e, f(e2))
					}
				}
				insert(f(e1))
			}
			case KLetRec(KFundef(xt, yts, e1), e2) => KLetRec(KFundef(xt, yts, f(e1)), f(e2))
			case e => e
		}
	}
}
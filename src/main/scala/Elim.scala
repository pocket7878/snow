package jp.dip.poketo7878.snow

import jp.dip.poketo7878.snow.KNormal._

object Elim {
	def effect(ex: KAST): Boolean = {
		ex match {
			case KLet(_, e1, e2) => effect(e1) || effect(e2)
			case KIfEq(_, _, e1, e2) => effect(e1) || effect(e2)
			case KIfLE(_, _, e1, e2) => effect(e1) || effect(e2)
			case KLetRec(_, e) => effect(e)
			case KApp(_,_) | KExtFuncApp(_,_) => true
			case _ => false
		}
	}

	def f(ex: KAST): KAST = {
		ex match {
			case KIfEq(x, y, e1, e2) => KIfEq(x, y, f(e1), f(e2))
			case KIfLE(x, y, e1, e2) => KIfLE(x, y, f(e1), f(e2))
			case KLet((x, t), e1, e2) => {
				val e1d = f(e1)
				val e2d = f(e2)
				if (effect(e1d) || KNormal.fv(e2d).contains(x)) {
					KLet((x, t), e1d, e2d)
				} else {
					e2d
				}
			}
			case KLetRec(KFundef((x, t), yts, e1), e2) => {
				val e2d = f(e2)
				if (KNormal.fv(e2d).contains(x)) {
					KLetRec(KFundef((x, t), yts, f(e1)), e2)
				} else {
					e2d
				}
			}
			case e => e
		}
	}
}
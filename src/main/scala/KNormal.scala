package jp.dip.poketo7878.snow

object KNormal {
	import jp.dip.poketo7878.snow.SnowParser.AToken
	import jp.dip.poketo7878.snow.SnowParser._
	//KNormalized AST
	trait KAST
	case class KToken(name: String) extends KAST
	case class KUnit() extends KAST
	case class KLong(value: Long) extends KAST
	case class KDouble(value: Double) extends KAST
	case class KNeg(id: KToken) extends KAST
	case class KAdd(e1: KToken, e2: KToken) extends KAST
	case class KSub(e1: KToken, e2: KToken) extends KAST
	case class KDNeg(id: KToken) extends KAST
	case class KDAdd(e1: KToken, e2: KToken) extends KAST
	case class KDSub(e1: KToken, e2: KToken) extends KAST
	case class KDMul(e1: KToken, e2: KToken) extends KAST
	case class KDDiv(e1: KToken, e2: KToken) extends KAST
	case class KIfEq(id1: KToken, id2: KToken, e1: KAST, e2: KAST) extends KAST
	case class KIfLE(id1: KToken, id2: KToken, e1: KAST, e2: KAST) extends KAST
	case class KLet(xt: (KToken, SType), e1: KAST, e2: KAST) extends KAST
	case class KFundef(name: (KToken, SType), args: List[(KToken, SType)], body: KAST) extends KAST
	case class KLetRec(fdef: KFundef, e1: KAST) extends KAST
	case class KApp(e: KToken, xs: List[KToken]) extends KAST
	case class KTuple(xs: List[KToken]) extends KAST
	case class KExtFuncApp(fn: KToken, args: List[KToken]) extends KAST

	//Free Variable
	def fv(ex: KAST): Set[KToken] = {
		ex match {
			case KUnit() | KLong(_) | KDouble(_) => Set()
			case KNeg(x) => Set(x)
			case KDNeg(x) => Set(x)
			case KAdd(x, y) => Set(x, y)
			case KSub(x, y) => Set(x, y)
			case KDAdd(x, y) => Set(x, y)
			case KDSub(x, y) => Set(x, y)
			case KDMul(x, y) => Set(x, y)
			case KDDiv(x, y) => Set(x, y)
			case KIfEq(x, y, e1, e2) => fv(e1).union(fv(e2)) + x + y
			case KIfLE(x, y, e1, e2) => fv(e1).union(fv(e2)) + x + y
			case KLet((x, t), e1, e2) => fv(e1).union(fv(e2) - x)
			case x@KToken(_) => Set(x)
			case KLetRec(KFundef((x, t), yts, e1), e2) => {
				val zs = fv(e1).diff(yts.map(y => y._1).toSet)
				zs.union(fv(e2)).diff(Set(x))
			}
			case KApp(x, ys) => (x +: ys).toSet
			case KTuple(xs) => xs.toSet
			case KExtFuncApp(_, xs) => xs.toSet
		}
	}

	def insert_let(et: (KAST, SType), k: (KToken => (KAST, SType))): (KAST, SType) = {
		et match {
			case (e, t) => {
				e match {
					case x@KToken(_) => k(x)
					case _ => {
						val x = SType.gen_tmp(t)
						val (e1, t1) = k(KToken(x.name))
						(KLet((KToken(x.name), t), e, e1), t1)
					}
				}
			}
		}
	}

	def g(env: Map[AToken, SType], ex: AST): (KAST, SType) = {
		ex match {
			case AUnit() => (KUnit(), SUnit())
			case ABool(b) => (KLong(if (b) {1} else {0}), SLong())
			case ALong(l) => (KLong(l), SLong())
			case ADouble(d) => (KDouble(d), SDouble())
			case ANot(e) => g(env, AIf(e, ABool(false), ABool(true)))
			case ANeg(e) => insert_let(g(env, e), {x => (KNeg(x), SLong())})
			case AAdd(e1, e2) => insert_let(g(env, e1), 
				{x => insert_let(g(env, e2), 
					{y => (KAdd(x, y), SLong())})})
			case ASub(e1, e2) => insert_let(g(env, e1), 
				{x => insert_let(g(env, e2), 
					{y => (KSub(x, y), SLong())})})
			case ADNeg(e) => insert_let(g(env, e),{x => (KDNeg(x), SDouble())})
			case ADAdd(e1, e2) => insert_let(g(env, e1), 
				{x => insert_let(g(env, e2), 
					{y => (KDAdd(x, y), SDouble())})})
			case ADSub(e1, e2) => insert_let(g(env, e1), 
				{x => insert_let(g(env, e2), 
					{y => (KDSub(x, y), SDouble())})})
			case ADMul(e1, e2) => insert_let(g(env, e1), 
				{x => insert_let(g(env, e2), 
					{y => (KDMul(x, y), SDouble())})})
			case ADDiv(e1, e2) => insert_let(g(env, e1), 
				{x => insert_let(g(env, e2), 
					{y => (KDDiv(x, y), SDouble())})})
			case cmp@AEq(_, _) => g(env, AIf(cmp, ABool(true), ABool(false)))
			case cmp@ALE(_, _) => g(env, AIf(cmp, ABool(true), ABool(false)))
			case AIf(ANot(e1), e2, e3) => g(env, AIf(e1, e3, e2))
			case AIf(AEq(e1, e2), e3, e4) => insert_let(g(env, e1),
				{x => insert_let(g(env, e2),
					{y => {
						val (e3d, t3) = g(env, e3)
						val (e4d, t4) = g(env, e4)
						(KIfEq(x, y, e3d, e4d), t3)}})})
			case AIf(ALE(e1, e2), e3, e4) => insert_let(g(env, e1),
				{x => insert_let(g(env, e2),
					{y => {
						val (e3d, t3) = g(env, e3)
						val (e4d, t4) = g(env, e4)
						(KIfLE(x, y, e3d, e4d), t3)}})})
			case AIf(e1, e2, e3) => g(env, (AIf(AEq(e1, ABool(false)), e3, e2)))
			case ALet((x, t), e1, e2) => {
				val (e1d, t1) = g(env, e1)
				val (e2d, t2) = g(env ++ Map(x -> t), e2)
				(KLet((KToken(x.name), t), e1d, e2d), t2)
			}
			case t@AToken(x) if env.contains(t) => (KToken(x), env.get(t).get)
			case ALetRec(AFundef((x, t), yts, e1), e2) => {
				val envd = env ++ Map(x -> t)
				val (e2d, t2) = g(envd, e2)
				val (e1d, t1) = g(yts.toMap ++ envd, e1)
				(KLetRec(KFundef((KToken(x.name), t), yts.map(y => (KToken(y._1.name), y._2)), e1d), e2d), t2)
			}
			case AApp(tok@AToken(f), e2s) if !env.contains(tok) => {
				Typing.extenv.get(tok) match {
					case Some(SFun(_, t)) => {
						def bind(xs: List[KToken], ls: List[AST]):(KAST, SType) = {
							ls match {
								case Nil => (KExtFuncApp(KToken(f), xs), t)
								case (e2 :: e2sd) => {
									insert_let(g(env, e2), 
										{x => bind(xs :+ x, e2sd)})
								}
							}
						}
						bind(List(), e2s)
					}
					case _ => error("Unknown external function")
				}
			}
			case AApp(e1, e2s) => {
				g(env, e1) match {
					case g_e1@(_, SFun(_, t)) => {
						insert_let(g_e1, 
							{f => {
								def bind(xs: List[KToken], ls: List[AST]):(KAST, SType) = {
									ls match {
										case Nil => (KApp(f, xs), t)
										case (e2 :: e2sd) => {
											insert_let(g(env, e2), 
												{x => bind(xs :+ x, e2sd)})
										}
									}
								}
								bind(List(), e2s)
								}})
					}
					case _ => error("Unknown function apply")
				}
			}
			case ATuple(es) => {
				def bind(xs: List[KToken], ts: List[SType], ls: List[AST]):(KAST, SType) = {
					ls match {
						case Nil => (KTuple(xs), STuple(ts))
						case (e :: esd) => {
							val g_e@(_, t) = g(env, e)
							insert_let(g_e,
								{x => bind(xs :+ x, (ts :+ t), esd)})
						}
					}
				}
				bind(List(), List(), es)
			}
		}
	}

	def f(ex: AST): KAST = g(Map(), ex)._1
}

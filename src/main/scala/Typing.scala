package jp.dip.poketo7878.snow

object Typing {
	import jp.dip.poketo7878.snow.SnowParser._

	var extenv: Map[AToken, SType] = Map()

	def deref_typ(ex: SType):SType = {
		ex match {
			case SFun(ax, b) => SFun(ax.map(a => deref_typ(a)), deref_typ(b))
			case STuple(xs) => STuple(xs.map(x => deref_typ(x)))
			case x@SVar(None) => {
				println("uninstantiated type variable detected; assuming long")
				x.typ = new Some(SLong())
				SLong()
			}
			case x@SVar(Some(y)) => {
				val t = deref_typ(y)
				x.typ = new Some(t)
				t
			}
			case t => t
		}
	}

	def deref_id_typ(id_typ: (AToken, SType)):(AToken, SType) = id_typ match {
		case (x, t) => (x, deref_typ(t))
	}

	def deref_term(ex: AST): AST = {
		ex match {
			case ANot(e) => ANot(deref_term(e))
			case ANeg(e) => ANeg(deref_term(e))
			case AAdd(e1, e2) => AAdd(deref_term(e1), deref_term(e2))
			case ASub(e1, e2) => ASub(deref_term(e1), deref_term(e2))
			case AEq(e1, e2) => AEq(deref_term(e1), deref_term(e2))
			case ALE(e1, e2) => ALE(deref_term(e1), deref_term(e2))
			case ADNeg(e) => ADNeg(deref_term(e))
			case ADAdd(e1, e2) => ADAdd(deref_term(e1), deref_term(e2))
			case ADSub(e1, e2) => ADSub(deref_term(e1), deref_term(e2))
			case ADMul(e1, e2) => ADMul(deref_term(e1), deref_term(e2))
			case ADDiv(e1, e2) => ADDiv(deref_term(e1), deref_term(e2))
			case AIf(e1, e2, e3) => AIf(deref_term(e1), deref_term(e2), deref_term(e3))
			case ALet(xt, e1, e2) => ALet(deref_id_typ(xt), deref_term(e1), deref_term(e2))
			case ALetRec(AFundef(xt, yts, e1), e2) => {
				ALetRec(AFundef(deref_id_typ(xt),yts.map(y => deref_id_typ(y)),deref_term(e1)), deref_term(e2))
			}
			case AApp(e, es) => AApp(deref_term(e), es.map(x => deref_term(x)))
			case ATuple(xs) => ATuple(xs.map(x => deref_term(x)))
			case e => e 
		}
	}


	def occur(t1: SType, t2: SType): Boolean = {
		t2 match {
			case SFun(ax, b) => ax.exists(x => occur(t1, x)) || occur(t1, b)
			case STuple(xs) => xs.exists(x => occur(t1, x))
			case SVar(x) if x == t1 => true
			case SVar(None) => false
			case SVar(Some(x)) => occur(t1, x)
			case _ => false
		}
	}
	def unify(e1: SType, e2: SType): Unit = {
		(e1, e2) match {
			case (_: SUnit, _: SUnit) => ()
			case (_: SBool, _: SBool) => ()
			case (_: SLong, _: SLong) => ()
			case (_: SDouble, _: SDouble) => ()
			case (SFun(arg1, rt1), SFun(arg2, rt2)) => {
				arg1.zip(arg2).map(x => unify(x._1, x._2))
				unify(rt1, rt2)
			}
			case (STuple(tx1), STuple(tx2)) => {
				tx1.zip(tx2).map(x => unify(x._1, x._2))
			}
			case (SVar(Some(t1)), SVar(Some(t2))) => {
				if (t1 == t2) {
					()
				}
			}
			case (SVar(Some(t1)), _) => unify(t1, e2)
			case (_, SVar(Some(t2))) => unify(e1, t2)
			case (x@SVar(None),_ ) => {
				if (occur(e1, e2)) {
					throw new Exception("Can't unify: " + e1 + " and " + e2)
				} else {
					x.typ = new Some(e2)
				}
			}
			case (_,x@SVar(None)) => {
				if (occur(e2, e1)) {
					throw new Exception("Can't unify: " + e2 + " and " + e1)
				} else {
					x.typ = new Some(e1)
				}
			}
			case (_, _) => {
				throw new Exception("Can't unify: " + e1 + " and " + e2)
			}
		}
	}

	def g(env: Map[AToken, SType], ex: AST): SType = {
		try {
			ex match {
				case ABool(_) => SBool()
				case ALong(_) => SLong()
				case ADouble(_) => SDouble()
				case ANot(e) => {
					unify(SBool(), g(env, e))
					SBool()
				}
				case ANeg(e) => {
					unify(SLong(), g(env, e))
					SLong()
				}
				case AAdd(e1, e2) => {
					unify(SLong(), g(env, e1))
					unify(SLong(), g(env, e2))
					SLong()
				}
				case ADNeg(e) => {
					unify(SDouble(), g(env, e))
					SDouble()
				}
				case ADAdd(e1, e2) => {
					unify(SDouble(), g(env, e1))
					unify(SDouble(), g(env, e2))
					SDouble()
				}
				case ADSub(e1, e2) => {
					unify(SDouble(), g(env, e1))
					unify(SDouble(), g(env, e2))
					SDouble()
				}
				case ADMul(e1, e2) => {
					unify(SDouble(), g(env, e1))
					unify(SDouble(), g(env, e2))
					SDouble()
				}
				case ADDiv(e1, e2) => {
					unify(SDouble(), g(env, e1))
					unify(SDouble(), g(env, e2))
					SDouble()
				}
				case AEq(e1, e2) => {
					unify(g(env, e1), g(env, e2))
					SBool()
				}
				case AIf(te, pro, neg) => {
					unify(g(env, te), SBool())
					val t2 = g(env, pro)
					val t3 = g(env, neg)
					unify(t2, t3)
					t2
				}
				case ALet((x, t), e1, e2) => {
					unify(t, g(env, e1))
					g((env ++ Map(x -> t)), e2)
				}
				case x@AToken(_) if env.contains(x) => {
					env.get(x).get
				}
				case x@AToken(_) if extenv.contains(x) => {
					extenv.get(x).get
				}
				case x@AToken(y) => {
					println("Found free variable " + y + " assumed as external")
					val t = genType()
					extenv = (extenv ++ Map(x -> t))
					t
				}
				case ALetRec(AFundef((x, t), yts, e1), e2) => {
					val en = env ++ Map(x -> t)
					unify(t, SFun(yts.map(y => y._2), g(yts.toMap ++ en, e1)))
					g(en, e2)
				}
				case AApp(e1, ex) => {
					val t = genType()
					unify(g(env, e1), SFun(ex.map(x => g(env, x)), t))
					t
				}
				case ATuple(ex) => STuple(ex.map(x => g(env, x)))
			}
		} catch {
			case e: Exception => {
				error("Unify Error")
			}
		}
	}

	def f(ex: AST) = {
		extenv = Map()
		try {
			unify(SUnit(), g(Map(), ex))
		} catch {
			case e: Exception => println("top level does not have type unit")
		}
		extenv = extenv.transform((k, v) => deref_typ(v))
		deref_term(ex)
	}
}
package jp.dip.poketo7878.snow

import scala.util.parsing.combinator.JavaTokenParsers

object SnowParser extends JavaTokenParsers {
	trait SType
	case class SUnit() extends SType
	case class SBool() extends SType
	case class SLong() extends SType
	case class SDouble() extends SType
	case class SString() extends SType
	case class SFun(args: List[SType], returnType: SType) extends SType
	case class STuple(values: List[SType]) extends SType
	case class SVar(var typ: Option[SType]) extends SType
	object SType {
		var counter = 0
		def id_of_typ(t: SType) = {
			t match {
				case SUnit() => "u"
				case SBool() => "b"
				case SLong() => "l"
				case SDouble() => "d"
				case SString() => "s"
				case SFun(_, _) => "f"
				case STuple(_) => "t"
				case SVar(_) => error("id_of_typ called on variable-type")
			}
		}
		def gen_tmp(t: SType): AToken = {
			val res = "T" + id_of_typ(t) + counter
			counter += 1
			AToken(res)
		}

		def genid(s: String): AToken = {
			counter += 1
			val res = s + counter
			AToken(res)
		}
	}
	def genType() = SVar(None)


	def parse(str: String) = parseAll(program, str)

	trait AST
	case class AUnit() extends AST
	case class ABool(value: Boolean) extends AST
	case class ALong(value: Long) extends AST
	case class ADouble(value: Double) extends AST
	case class AString(value: String) extends AST
	case class AToken(val name: String) extends AST
	case class ATuple(values: List[AST]) extends AST
	case class AIf(te: AST, pro: AST, ne: AST) extends AST
	case class ALet(bindVar: (AToken, SType), bindVal: AST, body: AST) extends AST
	case class ALetRec(fdef: AFundef, body: AST) extends AST
	case class AFundef(name: (AToken, SType), args: List[(AToken, SType)], body: AST) extends AST
	case class AApp(e1: AST, args: List[AST]) extends AST

	case class AAdd(e1: AST, e2: AST) extends AST
	case class ASub(e1: AST, e3: AST) extends AST
	case class ADAdd(e1: AST, e2: AST) extends AST
	case class ADSub(e1: AST, e2: AST) extends AST
	case class ADMul(e1: AST, e2: AST) extends AST
	case class ADDiv(e1: AST, e2: AST) extends AST
	case class AEq(e1: AST, e2: AST) extends AST
	case class ALE(e1: AST, e2: AST) extends AST

	case class ANot(e1: AST) extends AST
	case class ANeg(e1: AST) extends AST
	case class ADNeg(e1: AST) extends AST

	val reservedWord = List("if", "let", "true", "false", "let-rec",
	 "+", "-","not", "+.", "-.", "*.", "/.", "=", "<=", ">=", ">")

	def addType(i: AToken) = (i, genType())

	def program: Parser[AST] = exp
	def exp: Parser[AST] = ( 
		real
		| hexInteger
		| integer
		| boolean
		| tuple
		| literal
		| app
		| token
		| if_then_else
		| let_expr
		| let_rec_expr
		| binOp
		| neg
		| dneg
		)
	def app: Parser[AST] = "("~>exp~rep(exp)<~")" ^^ {
		case e1~ex => AApp(e1, ex)
	}
	def integer: Parser[ALong] = wholeNumber ^^ (n => ALong(n.toLong))
	def real: Parser[ADouble] = ( """\-?\d+\.\d*([eE]\-?\d+)?""".r ^^ (d => ADouble(d.toDouble))
		| """\-?\d+[eE]\-?\d+""".r ^^ (d => ADouble(d.toDouble)) )
	def hexInteger: Parser[ALong] = """\-?0x[\da-fA-F]+""".r ^^ (n => ALong(java.lang.Long.parseLong(n.substring(2), 16)))
	def boolean: Parser[ABool] = """true|false""".r ^^ {
		case "true" => ABool(true)
		case "false" => ABool(false)
	}
	def token: Parser[AToken] = """[^() ]+""".r ^?{case n if !reservedWord.contains(n) => n} ^^ (n => AToken(n.toString))
	def literal: Parser[AString] = stringLiteral ^^ (l => AString(l.tail.init))
	def tuple:Parser[ATuple] = "["~>rep(exp)<~"]" ^^ (e => ATuple(e))
	def if_then_else: Parser[AIf] = "("~>"if"~>(exp~exp~exp)<~")" ^^{
		case te~pro~neg => AIf(te, pro, neg)
	}

	def let_bind: Parser[((AToken, SVar), AST)] = ("("~>token)~exp<~")"^^{
		case t~e => (addType(t), e)
	}

	def let_expr: Parser[ALet] = ("("~>"let"~>let_bind)~exp<~")"^^{
		case ((t, typ), lb)~body => ALet((t, typ), lb, body)
	}

	def fun_arg: Parser[List[(AToken, SVar)]] = rep(token)^^{
		case tx => tx.map(t => addType(t))
	}

	def fundef: Parser[AFundef] = "("~>("("~>token)~(fun_arg<~")")~exp<~")"^^{
		case fname~args~body => AFundef(addType(fname), args, body)
	}

	def let_rec_expr: Parser[ALetRec] = "("~>"let-rec"~>fundef~exp<~")"^^{
		case fdef~body => ALetRec(fdef, body)
	}

	def neg: Parser[AST] = "("~>"-"~>exp<~")"^^{
		case ADouble(f) => ADouble(-f)
		case e => ANeg(e)
	}

	def dneg: Parser[AST] = "("~>"-."~>exp<~")"^^{
		case e => ADNeg(e)
	}

	def binOp: Parser[AST] = "("~>"""\+\.|-\.|\*\.|/\.|\+|-|<>|<=|=|>=|>""".r ~ exp ~ exp <~")"^^{
		case op~e1~e2 => op match {
			case "+." => ADAdd(e1, e2)
			case "-." => ADSub(e1, e2)
			case "*." => ADMul(e1, e2)
			case "/." => ADDiv(e1, e2)
			case "+"  => AAdd(e1, e2)
			case "-"  => ASub(e1, e2)
			case "<=" => ALE(e1, e2)
			case "="  => AEq(e1, e2)
			case ">=" => ALE(e2, e1)
			case ">"  => ANot(ALE(e1, e2))
			case "<>" => ANot(AEq(e1, e2))
		}
	}
}
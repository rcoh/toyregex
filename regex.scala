import scala.util.parsing.combinator._
import collection.mutable
object App {
  def main(args: Array[String]) = {
  	val nfa = NFA.regexToNFA(RegexParser("abc*").get)
  	println(GraphVizGenerator.toGraphViz(nfa))
  	
  }
}

abstract class RegexExpr
case class Literal(c: Char) extends RegexExpr
case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr
case class Concat(first: RegexExpr, second: RegexExpr) extends RegexExpr
case class Repeat(expr: RegexExpr) extends RegexExpr
case class Plus(expr: RegexExpr) extends RegexExpr

object RegexParser extends RegexParsers {
	// Precedence:
	// *, + binds the most tightly
	// then Concat
	// | is the weakest

	def charLit: Parser[RegexExpr] = ("""\w""".r | ".") ^^ { char => assert(char.length == 1); Literal(char.head) }
	def parLit: Parser[RegexExpr] = "(" ~> highExpr <~ ")"
	def lit: Parser[RegexExpr] = charLit | parLit
	def repeat: Parser[RegexExpr] = lit <~ "*" ^^ { case l => Repeat(l) } 
	def plus: Parser[RegexExpr] = lit <~ "+" ^^ { case p => Plus(p) }
	def lowExpr: Parser[RegexExpr] = repeat | plus | lit

	def concat: Parser[RegexExpr] = rep(lowExpr) ^^ { case list => listToConcat(list)}
	def midExpr: Parser[RegexExpr] = concat | lowExpr

	def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr ^^ { case l ~ "|" ~ r => Or(l, r)}
	def highExpr: Parser[RegexExpr] = or | midExpr

	def listToConcat(l: List[RegexExpr]): RegexExpr = l match {
		case head :: Nil => head
		case head :: rest => Concat(head, listToConcat(rest))
	}

	def apply(input: String): Option[RegexExpr] = parseAll(highExpr, input) match {
	  case Success(result, _) => Some(result)
	  case failure : NoSuccess => None
	}
}

object State {
	var nextId = 0
	// Provide unique Ids to the states
	def getId = {
		nextId += 1
		nextId
	}
}

abstract class State {
	val id = State.getId
	override def toString: String = {
		this match {
			case Consume(c, _) => c.toString
			case s: Split => "split"
			case Match() => "match"
		}
	}	

	override def equals(obj: Any) = obj match {
		case s: State => s.id == id
		case _ => false
	}

	override def hashCode = id
}
case class Consume(c: Char, var out: State) extends State
case class Split(out1: State, out2: State) extends State
case class Match() extends State {
	// Ensure all matches are always equal
	override val id = -1
}

case class WaitingToBeBound() extends State

// consume from tree, append
object NFA {
	def regexToNFA(regex: RegexExpr): State = regexToNFA(regex, Match())
	
	private def regexToNFA(regex: RegexExpr, andThen: State): State = {
		regex match {
			case Literal(c) => Consume(c, andThen)
			case Concat(first, second) => {
				regexToNFA(first, regexToNFA(second, andThen))
			}
			case Or(l, r) => Split(
				regexToNFA(l, andThen), 
				regexToNFA(r, andThen)
			)

			case Repeat(r) => 
				val split = Split(
					// One path goes to andThen, the other path goes back to r
					regexToNFA(r, WaitingToBeBound()),
					andThen
				)
				bindUnboundOutputs(split, split)
				split

			case Plus(r) =>
				val split = Split(
					regexToNFA(r, andThen),
					regexToNFA(r, WaitingToBeBound())
				)
				bindUnboundOutputs(split, split)
				split
		}
	}

	private def bindUnboundOutputs(input: State, to: State, visited: mutable.Set[State] = mutable.Set[State]()): Unit = {
		if (!(visited contains input)) {
			visited.add(input)
			input match {
				case c @ Consume(_, WaitingToBeBound()) => c.out = to
				case c: Consume => bindUnboundOutputs(c.out, to, visited)
				case Split(out1, out2) => 
					bindUnboundOutputs(out1, to, visited)
					bindUnboundOutputs(out2, to, visited)
				case Match() => 
			}
		}
	}
}

object NFAEvaluator {
	def evaluate(nfa: State, input: String): Boolean = evaluate(List(nfa), input)

	def evaluate(nfas: List[State], input: String): Boolean = {
		println(f"evaluating: $nfas")
		input match {
			case "" => nfas.flatMap(nfa => evaluate(nfa, None)).exists(_ == Match())
			case s => evaluate(nfas.flatMap(nfa => evaluate(nfa, s.headOption)), s.tail)
		}
	}

	def evaluate(nfa: State, input: Option[Char]): List[State] = {
		nfa match {
			case Consume(c, next) => if (Some(c) == input || c == '.') List(next) else Nil
			case Split(out1, out2) => evaluate(out1, input) ++ evaluate(out2, input)
			case Match() => if (input.isDefined) Nil else List(Match())
		}
	}
}

object Regex {
	def fullMatch(input: String, pattern: String) = {
		val parsed = RegexParser(pattern).getOrElse(throw new RuntimeException("Failed to parse regex"))
		val nfa = NFA.regexToNFA(parsed)
		NFAEvaluator.evaluate(nfa, input)
	}	

	def matchAnywhere(input: String, pattern: String) = fullMatch(input, ".*" + pattern + ".*")
}

import scala.util.parsing.combinator._
import collection.mutable

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

abstract class State

class Consume(val c: Char, var out: State) extends State
class Split(var out1: State, var out2: State) extends State
class WaitingToBeBound() extends State
case class Match() extends State

// consume from tree, append
object NFA {
	def regexToNFA(regex: RegexExpr): State = regexToNFA(regex, Match())
	
	private def regexToNFA(regex: RegexExpr, andThen: State): State = {
		regex match {
			case Literal(c) => new Consume(c, andThen)
			case Concat(first, second) => {
				regexToNFA(first, regexToNFA(second, andThen))
			}
			case Or(l, r) => new Split(
				regexToNFA(l, andThen), 
				regexToNFA(r, andThen)
			)

			case Repeat(r) => 
				val placeholder = new WaitingToBeBound()
				val split = new Split(
					// One path goes to andThen, the other path goes back to r
					regexToNFA(r, placeholder),
					andThen
				)
				bindUnboundOutputs(split, split, target = placeholder)
				split

			case Plus(r) =>
				val placeholder = new WaitingToBeBound()
				val split = new Split(
					regexToNFA(r, andThen),
					regexToNFA(r, placeholder)
				)
				bindUnboundOutputs(split, split, target = placeholder)
				split
		}
	}

	private def bindUnboundOutputs(input: State, to: State, visited: mutable.Set[State] = mutable.Set[State](), target: State): Unit = {
		if (!(visited contains input)) {
			visited.add(input)
			input match {
				case c: Consume => c.out match {
					case w: WaitingToBeBound if w == target => c.out = to
					case _ => bindUnboundOutputs(c.out, to, visited, target)
				}
				case s: Split => {
					s.out1 match {
						case w: WaitingToBeBound if w == target => s.out1 = to
						case _ => bindUnboundOutputs(s.out1, to, visited, target)
					}
					s.out2 match {
						case w: WaitingToBeBound if w == target => s.out2 = to
						case _ => bindUnboundOutputs(s.out2, to, visited, target)
					}
				}
	
				case _: Match =>
				case _: WaitingToBeBound =>
			}
		}
	}
}

object NFAEvaluator {
	def evaluate(nfa: State, input: String): Boolean = evaluate(Set(nfa), input)

	def evaluate(nfas: Set[State], input: String): Boolean = {
		input match {
			case "" => evaluateStates(nfas, None).exists(_ == Match())
			case string => evaluate(evaluateStates(nfas, input.headOption), string.tail)
		}
	}

	def evaluateStates(nfas: Set[State], input: Option[Char]): Set[State] = {
		val visitedStates = mutable.Set[State]()
		nfas.flatMap(state => evaluateState(state, input, visitedStates))
	}

	def evaluateState(currentState: State, input: Option[Char], visitedStates: mutable.Set[State]): Set[State] = {
		if (visitedStates contains currentState) {
			Set()
		} else {
			visitedStates.add(currentState)
			currentState match {
				case consume: Consume => if (Some(consume.c) == input || consume.c == '.') Set(consume.out) else Set()
				case s: Split => evaluateState(s.out1, input, visitedStates) ++ evaluateState(s.out2, input, visitedStates)
				case m: Match => if (input.isDefined) Set() else Set(Match())
			}
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

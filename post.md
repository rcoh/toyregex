Until pretty recently, regular expressions seemed magical to me. Turns out they aren't. Here's how I implemented a basic regular expression engine in under 200 lines of code.
## The Spec ##
We'll support a regular expression language with the following features:
* `.`: Match any character
* ``|``: Match ``abc`` or ``cde`` 
* ``+``: Match one or more of the previous pattern
* ``*``: Match 0 or more of the previous pattern
* ``(`` and ``)`` for grouping

While this is a small set of options, we'll still be able to make some cool (although cumbersome) regexes:
* `(..)*`: All even length strings
* `(1|2|3|4|5|6|7|8|9)(1|2|3|4|5|6|7|8|9)(1|2|3|4|5|6|7|8|9)-...`: Find a phone number

## The Plan of Attack ##

Our evaluator will have three phases:
1. Parse the regular expression into a syntax tree.
2. Convert the syntax tree into a finite-automata.
3. Evaluate the state machine against our string.

We'll use an [NFA](http://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) to evaluate regular expressions using the approach originally outlined in Thompson's 1968 CACM paper. It's a simplification of how non-backtracking engines like RE2 evaluate regular expressions in provably linear time. My approach roughly follows the strategy Russ Cox outlines in his [excellent blog post](http://swtch.com/~rsc/regexp/regexp1.html).

## Parsing The Regular Expression ##
I'm not going to talk about building in parsers in a high level of detail -- you'll have to wait for my next post. Here, I'll give a high level overview of how I parsed regular expressions with Scala's parser combinator library.

Before we can hope to evaluate a regular expression, we need to convert it into data structure composed of the regular expression's component parts. Our parser will convert a string into an ``Option[SyntaxTree]`` (if it fails to parse, it will be ``None``). There are 5 distinct objects we'll extract from the parse tree. They'll be under a single parent class:

	abstract class RegexExpr

Literal characters like ``a``,``b`` or ``.``:

	case class Literal(c: Char) extends RegexExpr
	case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr

One expression followed by another, eg. ``ab`` would be roughly ``Concat(a, b)`` Concat will compose greater than two sequential items: ``abc`` would be ``Concat(a, Concat(b, c))``.

	case class Concat(first: RegexExpr, second: RegexExpr) extends RegexExpr
The star character, eg. ``a*``:

	case class Repeat(expr: RegexExpr) extends RegexExpr
The plus character, eg. ``a+``:

	case class Plus(expr: RegexExpr) extends RegexExpr 

To do the parsing we're going to Scala's convenient parser combinator library. This will let us write a parser by just writing the grammar. It uses a lot of obtuse symbols, but I hope you'll be able to look through the noise to see what's happening.

I'm just using RegexParsers because it has some handy features, like automatic parsing of literal strings. I'm not cheating ;-).

	object RegexParser extends RegexParsers {

Different operators "bind" with different strengths -- as an analogy, ``*`` binds more tightly than ``+`` in expressions like 5+6*4. If we consider the resulting tree), the weakest operators end up on top. 

[![multex.png](https://d23f6h5jpj26xu.cloudfront.net/sstquqt5mol21w_small.png)](http://img.svbtle.com/sstquqt5mol21w.png)

It follows that we should parse the weakest operators first, followed by the stronger operators. When parsing, you can imagine it as extracting an operator, adding it to the root of the tree, then recursing the remaining 2 parts of the string. In regular expressions the order of binding strength is:
1. Character Literal & parentheses
2. `+` and `*`
3. "Concatenation" -- a is after b
4. `|`

We break the 4 classes into `lit`, `lowExpr`, `midExpr`, and `highExpr` respectively.  

		def charLit: Parser[RegexExpr] = ("""\w""".r | ".") ^^ { 
			char => assert(char.length == 1); Literal(char.head) 
		}

This allows us to put anything in parenthesis, but have parenthesis bind the strongest:

		def parenExpr: Parser[RegexExpr] = "(" ~> highExpr <~ ")"
		def lit: Parser[RegexExpr] = charLit | parenExpr
		def repeat: Parser[RegexExpr] = lit <~ "*" 
			^^ { case l => Repeat(l) } 
		def plus: Parser[RegexExpr] = lit <~ "+" 
			^^ { case p => Plus(p) }
		def lowExpr: Parser[RegexExpr] = repeat | plus | lit

		def concat: Parser[RegexExpr] = rep(lowExpr) 
			^^ { case list => listToConcat(list)}
		def midExpr: Parser[RegexExpr] = concat | lowExpr

		def or: Parser[RegexExpr] = midExpr ~ "|" ~ midExpr 
			^^ { case l ~ "|" ~ r => Or(l, r)}

highExpr is an ``or``, the weakest binding operator, or if there isn't an ``or``, a ``midExpr``.

		def highExpr: Parser[RegexExpr] = or | midExpr

		def listToConcat(list: List[RegexExpr]): RegexExpr = list match {
			case head :: Nil => head
			case head :: rest => Concat(head, listToConcat(rest))
		}

		def apply(input: String): Option[RegexExpr] = {
			parseAll(highExpr, input) match {
		  		case Success(result, _) => Some(result)
		  		case failure : NoSuccess => None
			}
		}
	}

This will turn our regular expression into a tree. For example, ``b+a`` would become:

[![syntaxex.png](https://d23f6h5jpj26xu.cloudfront.net/utcttrpai2yaw_small.png)](http://img.svbtle.com/utcttrpai2yaw.png)

## Converting the parse to an NFA ##

We can't actually evaluate the regular expression until we turn it into an NFA. We have something that looks like:

[![syntaxex.png](https://d23f6h5jpj26xu.cloudfront.net/iapx4sy8xf3cg_small.png)](http://img.svbtle.com/iapx4sy8xf3cg.png)

But we wished it looked like:

[![nfaex.png](https://d23f6h5jpj26xu.cloudfront.net/qqmdqnjzu5fq_small.png)](http://img.svbtle.com/qqmdqnjzu5fq.png)

This transformation is orthogonal to the transformation a compiler does when it translates from an abstract syntax tree to a control flow graph -- thankfully, it's much simpler in our case :-). We'll construct this recursively from the syntax tree. The only tricky bit is tracking what the output for an arbitrary tree element should be, but we'll see that that isn't too hard either.

It's going to be helpful if we have a unique id to identify each state in our NFA:

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

Since our data structures will be cyclical, we need to override toString, or it will stack-overflow when called.

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

An arrow that consumes one character of input. Unfortunately, having immutable cyclic graphs is a bit impractical. We'll sacrifice immutability in this one location for simplicity:
	
	case class Consume(c: Option[Char], var out: State) extends State

A branch that can represent an Or, Plus, or Star

	case class Split(out1: State, out2: State) extends State

The end state.

	case class Match() extends State {
		// Ensure all matches are always equal
		override val id = -1
	}

In order to track locations that we still need to attach a "next" to, we create this additional type:

	case class WaitingToBeBound() extends State

To actually transform from a regular expression to an NFA, we'll be following Thompson's transformations -- in truth, they're all relatively straightforward. Our implementation recursively deconstructs the tree, then se ``andThen``.

	object NFA {

Turn the regex into an NFA, then append a `Match()`:

		def regexToNFA(regex: RegexExpr): State = 
			regexToNFA(regex, Match())
		
		private def regexToNFA(
			regex: RegexExpr, 
			andThen: State): State = {
			
			regex match {

A character literal translates into consuming a literal:

				case Literal(c) => Consume(c, andThen)

A concatenation is the first, followed by the second, followed by ``andThen``.

				case Concat(first, second) => {
					regexToNFA(first, regexToNFA(second, andThen))
				}

To create an Or, split into 2 NFAs, and then point them both to ``andThen``.

				case Or(l, r) => Split(
					regexToNFA(l, andThen), 
					regexToNFA(r, andThen)
				)

Now things get a bit tricky. To translate a "*" we need to create one path that skips the component (the naked ``andThen``) and one path that returns to the root of the split operator. Here, we use ``WaitingToBeBound()`` to set up the NFA output that will point back to the root of the split.
				
				case Repeat(r) => 
					val split = Split(
						// One path goes to andThen, 
						// the other path goes back to r
						regexToNFA(r, WaitingToBeBound()),
						andThen
					)
					// Find unbound outputs below split 
					// and replace them with split
					bindUnboundOutputs(split, split)
					split

``Plus`` is very similar to ``Repeat``. The only difference is that both paths must go through ``r``:

				case Plus(r) =>
					val split = Split(
						regexToNFA(r, andThen),
						regexToNFA(r, WaitingToBeBound())
					)
					bindUnboundOutputs(split, split)
					split
			}
		}

This function simply does a DFS search for ``WaitingToBeBound``, replacing all occurences with ``to``:

		private def bindUnboundOutputs(
	      input: State, 
		  to: State, 
		  visited: mutable.Set[State] = mutable.Set[State]()): Unit = {
			
			if (!(visited contains input)) {
				visited.add(input)
				input match {
					case c @ Consume(_, WaitingToBeBound()) => 
						c.out = to
					case c: Consume => 
						bindUnboundOutputs(c.out, to, visited)
					case Split(out1, out2) => 
						bindUnboundOutputs(out1, to, visited)
						bindUnboundOutputs(out2, to, visited)
					case Match() => 
				}
			}
		}
	}

The translation from syntax tree to NFA is perhaps the trickiest part. If you have a simpler way to do it, I'd love to hear it!

## Evaluating the NFA ##

Now that we're finally done turning our regex into something useful (an NFA), it's time to check if an input actually matches it. First a bit of theory:

### NFAs, DFAs and Regular Expressions
There are two types of finite automata, deterministic and non-deterministic. They have one key difference: A non-deterministic finite automata can have multiple paths out of the same node for the same token as well as paths for ``epsilon``, that it can take without consuming any tokens. In terms of power, NFAs, DFAs and regular expressions are all equivalent in the patterns that they can express. Lets first consider a regular expression ``abc*`` expressed in a DFA:

[![regexdfa.png](https://d23f6h5jpj26xu.cloudfront.net/cthi9nvdpg1p1a_small.png)](http://img.svbtle.com/cthi9nvdpg1p1a.png)

Evaluating a DFA is straightforward: simply move through the states by consuming the input string. If you finish consuming input in the match state, match, otherwise don't. Our state machine, on the other hand, is an NFA. The NFA our code generates for this regular expression is:

[![dfavsnfa.png](https://d23f6h5jpj26xu.cloudfront.net/jgl025kfustnta_small.png)](http://img.svbtle.com/jgl025kfustnta.png)

Note that there are multiple unlabeled edges that we can follow without consuming a character. How can we evaluate that efficiently? The answer is surprisingly simple. Just keep a list of states that the engine is currently in. When you encounter a fork, take both paths (turning one state into two). When a state can't proceed, remove it from the list.

	object NFAEvaluator {
		def evaluate(nfa: State, input: String): Boolean = 
			evaluate(List(nfa), input)

		def evaluate(nfas: List[State], input: String): Boolean = {
			input match {

If we have no input, advance any states that can move without further input and check for a match.

				case "" => 
					nfas.flatMap(nfa => evaluate(nfa, None))
					.exists(_ == Match())
				case s => 
					evaluate(nfas.flatMap { nfa => 
						evaluate(nfa, s.headOption)
					}, s.tail)
			}
		}

		def evaluate(nfa: State, input: Option[Char]): List[State] = {
			nfa match {
				case Consume(c, next) => 
					if (Some(c) == input || c == '.') 
						List(next) 
					else 
						Nil
				case Split(out1, out2) => 
					evaluate(out1, input) ++ evaluate(out2, input)

``Match`` will only survive a state transition if there is no more input. This creates exact matching semantics.


				case Match() => if (input.isDefined) 
					Nil 
				else 
					List(Match())
			}
		}
	}

And thats it!

## Put a bow on it ##

	object Regex {
		def fullMatch(input: String, pattern: String) = {
			val parsed = RegexParser(pattern).getOrElse(
				throw new RuntimeException("Failed to parse regex")
			)
			val nfa = NFA.regexToNFA(parsed)
			NFAEvaluator.evaluate(nfa, input)
		}	

It's also easy to add "match anywhere" semantics by prepending and appending ``.*``.

		def matchAnywhere(input: String, pattern: String) = fullMatch(input, ".*" + pattern + ".*")
	}

To use it:
	Regex.fullMatch("aaaaab", "a*b") // True
	Regex.fullMatch("aaaabc", "a*b") // False
	Regex.matchAnywhere("abcde", "cde") // True

That's all there is to it (mostly). A semi-functional regex implementation in just 160 lines. There's a number of things that could be added but I decided added complexity without enough value:

1. Character classes
2. Value extraction
3. ``?``
4. Escape characters
5. Any many more.

I hope this simple implementation helps you understand what's going on under the hood!
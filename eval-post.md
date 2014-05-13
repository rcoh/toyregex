



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
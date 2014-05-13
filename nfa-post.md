The code for this post, as well as the post itself, are on [github](https://github.com/rcoh/toyregex).

## Converting the parse to an NFA ##

In the last post, we tranformed the string representation of a regex to the hierarchical parse tree. In this post we'll transform from the parse tree to a state machine. 

*Why make yet another transformation just to match a regex?* 

It would certainly be possible to translate directly from a string to a state machine. You could even attempt to evaluate the regex directly from the parse tree or even the string. Doing this, however, would likely involve significantly more complicated code. By slowly lowering the level abstraction of abstraction, we can ensure the code at each stage is easy to understand and reason about. This is especially important when building something like a regular expression evaluator with literally infinite edge cases.

## NFAs, DFAs, and You ##
We'll be creating a state machine called an NFA, or nondeterministic finite automata. It has two types of components: states & transitions. When we diagram it, states will be represented as circles and transitions will be arrows. The match state will be a double circle. If a transition is labeled with a character, it means that we must consume that character from the input to make that transition. Transitions can also be unlabeled -- that means that we can make the transition without consuming input. 

Our nodes can have multiple valid next steps for a given input. This is in contrast to deterministic finite automata, where there can only be exactly one future state for a given input. This property will make evaluating it a bit more tricky, but it makes it much easier to generate them from our tree, as we'll see shortly.

## Transforming, in Theory ##
Lets layout a strategy for transforming our parse tree info an NFA. While it may seem daunting, we'll see breaking the process into composable pieces, it can be straightforward. Recall the syntax elements that we'll need to transform:
1. Characters Literals
2. ``*``
3. ``+``
4. Concatenation
5. ``|``

The transformations that follow were initially outlined by Thompson in his 1968 paper. Here are the transition rules for each of the types. Note that in the diagrams the states can actually be proxies for entire regex sub-components.

Let's start with character literals. A character literal is just a transition from one state to another that consumes input:

[[char_literal_transition.png]]

The next most straightforward is concatenation. To concatenate to components of the parse tree, we just need to link them together with a empty wire. ``Concat(regexa, regexb)`` would transform to:

[[regex_concat.png]]

Next is Or. To represent ``Or(regexa, regexb)`` we need to split into the two separate states:

[[regex_split.png]]

Lets consider ``*``. Star can be 0 or more repetitions of a pattern. To do that, we want one wire pointing directly to the next state, and one looping back to our current state. ``a*`` would look like:

[[regex_star.png]]

For ``+``, we'll use a little trick. ``a+`` is just ``aa*``. Generalizing, ``Plus(regexa)`` can be rewritten to ``Concat(regexa, Repeat(a))``  We'll rewrite it to that, rather than designing a special patten for it.

## Transforming, in practice

Now that we've made a theoretical plan, lets dive in to how we'll actually code it. We'll be creating a mutable graph to store our tree. While I'd prefer immutability, making immutable graphs is just annoyingly difficult, and I'm lazy.

Here are our 3 NFA component classes:
	
	abstract class State

	class Consume(val c: Char, var out: State) extends State
	class Split(var out1: State, var out2: State) extends State
	case class Match() extends State

It turns out that figuring out what comes next when constructing the NFA is a bit tricky. We'll use ``WaitingToBeBound`` in places where we don't know what to put yet. When we figure it out in the future, we'll recursively replace all instances of this type with the object that belongs.

	class WaitingToBeBound() extends State

Our code will recursively traverse our syntax tree, keeping an ``andThen`` object as a parameter. ``andThen`` is the what we will attach to the free hanging outputs from our expression. When we don't know what comes next, or we need to point the outputs to ourselves, we'll put in a placeholder. When we know what belongs there, we'll do a "find and replace" on the tree (``bindUnboundOutputs``).

To start things off, ``andThen`` is ``Match()`` -- this means that we'll create a state machine matching our regex which can then transfer to the match state without consuming anymore input. Lets dive into the code: 

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

When binding unbound outputs, we need to traverse looking for the matching output objects. They could be sitting in the output of a ``Consume`` or a ``Split``. Since we made them mutable, we'll just swap the output for what we want there instead.

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

And we're done! I also wrote a quick script to generate ``dot`` files from NFAs so I could look at the NFAs I was actually generating to debug issues. Note that the NFAs this code generates contain lots of unnecessary transitions and states -- When writing this, I've tried to keep the code as simple as possible, at the cost of performance.

``(..)*``

[evenstrs.dot.png]

``ab``

[ab.dot.png]

``a|b``

[a|b.dot.png]

``(a+|b)*``

[(a+|b)*]

Now that we have an NFA (the hard part) all we have to do is evaluate it (the easy part). Come back next week for the wrapup post on writing the evaluator, or just read the code for yourself on [github](http://www.github.com/rcoh/toyregex).
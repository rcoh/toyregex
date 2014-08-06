The code for this post, as well as the post itself, are on [github](https://github.com/rcoh/toyregex).

## Converting the Parse Tree to an NFA ##

In the last post, we transformed the flat string representation of a regular expression to the hierarchical parse tree form. In this post we'll transform the parse tree to a state machine. The state machine linearizes the regex components into a graph, producing an "a follows b follows c" relationship. The graph representation will make it easier to evaluate against a potential string.

*Why make yet another transformation just to match a regex?* 

It would certainly be possible to translate directly from a string to a state machine. You could even attempt to evaluate the regex directly from the parse tree or the string. Doing this, however, would likely involve significantly more complicated code. By slowly lowering the level of abstraction, we can ensure that the code at each stage is easy to understand and reason about. This is especially important when building something like a regular expression evaluator with literally infinite edge cases.

## NFAs, DFAs, and You ##
We'll be creating a state machine called an NFA, or [nondeterministic finite automata](http://en.wikipedia.org/wiki/Nondeterministic_finite_automaton). It has two types of components: states and transitions. When we diagram it, states will be represented as circles and transitions will be arrows. The match state will be a double circle. If a transition is labeled, it means that we must consume that character from the input to make that transition. Transitions can also be unlabeled -- that means that we can make the transition without consuming input. *Aside: In other literature, you may see this represented as ε.* This state machine represents the simple regex "ab":

[![ab.dot.png](https://d23f6h5jpj26xu.cloudfront.net/4rucysbdafej2g_small.png)](http://img.svbtle.com/4rucysbdafej2g.png)

Our nodes can have multiple valid subsequent states for a given input, as in the following diagram where there are two paths consuming the same input leaving a node:

[![nfa.dot.png](https://d23f6h5jpj26xu.cloudfront.net/zm3gxeigihunmg_small.png)](http://img.svbtle.com/zm3gxeigihunmg.png)

Contrast this with deterministic finite automata, where, as the name suggests, a given input can only result in one state change. This relaxation in constraints will make evaluating it a bit more tricky, but it makes it much easier to generate them from our tree, as we'll see shortly. Fundamentally, NFAs and DFAs are equivalent in the state machines they can represent.

## Transforming, in Theory ##
Let's lay out a strategy for transforming our parse tree info an NFA. While it may seem daunting, we'll see that by breaking the process into composable pieces, it can be straightforward. Recall the syntax elements that we'll need to transform:
1. Characters Literals: ``Lit(c: Char)``
2. ``*``: ``Repeat(r: RegexExpr)``
3. ``+``: ``Plus(r: RegexExpr)``
4. Concatenation: ``Concat(first: RegexExpr, second: RegexExpr)``
5. ``|``: ``Or(a: RegexExpr, b: RegexExpr)``


The transformations that follow were initially outlined by Thompson in his 1968 paper. In the transformation diagrams, ``In`` will refer the to the entry point of the state machine and ``Out`` will refer to the exit. In practice those can be the "Match" state, the "Start" state, or other regex components. The ``In`` / ``Out`` abstraction allows us to compose and combine state machines, a key insight. For example, if we have a state machine that matches "abc" and another that matches "cde", connecting them sequentially will yield a state machine that matches "abccde". We'll apply this principle more generally to transform from each element in our syntax tree into a composable state machine.
 
Let's start with character literals. A character literal is a transition from one state to another that consumes input. Consuming the literal 'a' would look like this:

[![lit.dot.png](https://d23f6h5jpj26xu.cloudfront.net/rwoluadeoefga_small.png)](http://img.svbtle.com/rwoluadeoefga.png)

Next lets examine concatenation. To concatenate two components of the parse tree, we just need to link them together with a empty arrow. Note that in this example, just as a ``Concat`` can contain the concatenation of two arbitrary regular expressions, ``A`` and ``B`` in the diagram can both be state machines, not just individual states. Something a little bit weird happens if ``A`` and ``B`` are both literals. How do we connect to arrows together with no intervening states? The answer is that when necessary, literals can have phantom states at each end to make our state machine sound. ``Concat(A, B)`` would transform to:

[![concat.dot.png](https://d23f6h5jpj26xu.cloudfront.net/t3rizd7fdw3ljw_small.png)](http://img.svbtle.com/t3rizd7fdw3ljw.png)

To represent ``Or(A, B)`` we need to branch from the start state into the two separate states. Once those machines terminate, they must both point to the subsequent (``Out``) state:

[![split.dot.png](https://d23f6h5jpj26xu.cloudfront.net/nhbxkuxnlcx70q_small.png)](http://img.svbtle.com/nhbxkuxnlcx70q.png)

Lets consider ``*``. Star can be 0 or more repetitions of a pattern. To do that, we want one wire pointing directly to the next state, and one looping back to our current state via ``A``. ``A*`` would look like:

[![astar.dot.png](https://d23f6h5jpj26xu.cloudfront.net/40md7csdwgopq_small.png)](http://img.svbtle.com/40md7csdwgopq.png)

For ``+``, we'll use a little trick. ``a+`` is just ``aa*``. Generalizing, ``Plus(A)`` can be rewritten to ``Concat(A, Repeat(A))``  We'll rewrite it to that, rather than designing a special pattern for it. I've included ``+`` in the language for a specific reason: to demonstrate how other, more complicated regular expression elements that I've omitted can often be expressed in terms of our language.

## Transforming, in practice

Now that we've made a theoretical plan, lets dive in to how we'll actually code it. We'll be creating a mutable graph to store our tree. While I'd prefer immutability, making immutable graphs is annoyingly difficult, and I'm lazy.

If we were to try to boil the above diagrams into core components, we'd end up with three types of components: arrows that consume input, the match state, and one state splitting into two states. I know this seems a little odd, and potentially incomplete. You'll have to trust me that this choice leads to the cleanest code. Here are our 3 NFA component classes:
    
    abstract class State

    class Consume(val c: Char, val out: State) extends State
    class Split(val out1: State, val out2: State) extends State
    case class Match() extends State

*Aside: I made ``Match`` a [``case class``](http://stackoverflow.com/questions/2312881/what-is-the-difference-between-scalas-case-class-and-class) instead of a regular class. Case classes in Scala bring certain sensible defaults to your class. I used it because it gives value based equality. This makes all ``Match`` objects equal, a useful property. For the other types of NFA components, we want reference equality.* 

Our code will recursively traverse our syntax tree, keeping an ``andThen`` object as a parameter. ``andThen`` is the what we will attach to the free hanging outputs of our expression. This is required because from an aribtrary branch in your syntax tree, you lack the context of what comes next -- ``andThen`` allows us pass that context down as we recurse. It also gives us an easy way to append the ``Match`` state.

When it comes time to handle ``Repeat`` we'll run into a problem. The ``andThen`` for ``Repeat`` is the split operator itself. To deal with this issue, we'll introduce a placeholder to let us bind it later. We'll represent the placeholder with this class:

    class Placeholder(var pointingTo: State) extends State

The ``var`` in ``Placeholder`` means the ``pointingTo`` is mutable. It is the isolated bit of mutability allowing us easily create a cyclical graph. All the other members are immutable.

To start things off, ``andThen`` is ``Match()`` -- this means that we'll create a state machine matching our Regex which can then transfer to the ``Match`` state without consuming anymore input. The code is short but dense: 

    object NFA {
        def regexToNFA(regex: RegexExpr): State = 
            regexToNFA(regex, Match())
        
        private def regexToNFA(regex: RegexExpr, 
                               andThen: State): State = {       
            regex match {
                case Literal(c) => new Consume(c, andThen)
                case Concat(first, second) => {
                    // Convert first to an NFA. The "out" of that is
                    // the result of converting second to an NFA.
                    regexToNFA(first, regexToNFA(second, andThen))
                }
                case Or(l, r) => new Split(
                    regexToNFA(l, andThen), 
                    regexToNFA(r, andThen)
                )

                case Repeat(r) => 
                    val placeholder = new Placeholder(null)
                    val split = new Split(
                        // One path goes to the placeholder, 
                        // the other path goes back to r
                        regexToNFA(r, placeholder),
                        andThen
                    )
                    placeholder.pointingTo = split
                    placeholder

                case Plus(r) => 
                    regexToNFA(Concat(r, Repeat(r)), andThen)   
            }
        }
    }

And we're done! The word to line of code ratio in this post is high -- each line encodes a lot of information, but boils down the the transformations we discussed in the previous section. I should clarify I didn't just sit down and write the code like this -- getting it this short and to the point was the result of several iterations on the data-structures and algorithm. Clean code is hard.

In the debugging process, I wrote a quick script to generate ``dot`` files from NFAs so I could look at the NFAs I was actually generating to debug issues. Note that the NFAs this code generates contain lots of unnecessary transitions and states -- When writing this, I've tried to keep the code as simple as possible, at the cost of performance. Here are some examples for simple regular expressions:

``(..)*``

[![evenstrs.dot.png](https://d23f6h5jpj26xu.cloudfront.net/bg6rbxw6xxdfvg_small.png)](http://img.svbtle.com/bg6rbxw6xxdfvg.png)

``ab``

[![ab.dot.png](https://d23f6h5jpj26xu.cloudfront.net/junr45ukmznkww_small.png)](http://img.svbtle.com/junr45ukmznkww.png)

``a|b``

[![aorb.dot.png](https://d23f6h5jpj26xu.cloudfront.net/mfdc04hix1g8kg_small.png)](http://img.svbtle.com/mfdc04hix1g8kg.png)

Now that we have an NFA (the hard part) all we have to do is evaluate it (the easier part). Come back next week for the wrapup post on writing the evaluator, or just read the code for yourself on [github](http://www.github.com/rcoh/toyregex).

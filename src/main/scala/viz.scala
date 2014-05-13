import collection.mutable
import java.io._
object App {
  def main(args: Array[String]) = {
  	val input = args(0)
  	val outfilename = args(1)
  	val output = GraphVizGenerator.toGraphViz(NFA.regexToNFA(RegexParser(input).get))
    val writer = new PrintWriter(new File(outfilename))
    writer.write(output)
    writer.close()
  }
}

case class Edge(start: Int, end: Int, label: Option[String] = None) {
	def toDot = f"""node[label=""] $start -> $end """ + label.map(l => f"[label=$l]").getOrElse("")
}

object GraphVizGenerator {
	def toGraphViz(input: State): String = {
		val edges = toEdgeList(input).map(_.toDot).mkString("\n")
		f"""digraph { 
			{
				-1 [shape=doublecircle][label=""]
				-2 [label="WAIT"]
			}
			$edges 
		}"""
	}

	def getId(s: State) = s match {
		case m: Match => -1
		case w: WaitingToBeBound => -2
		case _ => s.hashCode
	}

	def toEdgeList(input: State, visited: mutable.Set[State] = mutable.Set[State]()): List[Edge] = {
		if (visited contains input) {
			Nil
		} else {
			visited.add(input)
			input match {
				case c: Consume => Edge(getId(c), getId(c.out), Some(c.c.toString)) :: 
					toEdgeList(c.out, visited)
				case split: Split => Edge(getId(split), getId(split.out1)) :: 
					Edge(getId(split), getId(split.out2)) :: (toEdgeList(split.out1, visited) ++ toEdgeList(split.out2, visited))
				case m: Match => Nil
				case w: WaitingToBeBound => Nil
			}
		}		
	}
}

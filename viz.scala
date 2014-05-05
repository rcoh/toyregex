import collection.mutable
case class Edge(start: Int, end: Int, label: Option[String] = None) {
	def toDot = f"""node[label=""] $start -> $end """ + label.map(l => f"[label=$l]").getOrElse("")
}

object GraphVizGenerator {
	def toGraphViz(input: State): String = {
		val edges = toEdgeList(input).map(_.toDot).mkString("\n")
		f"""digraph { 
			{

				-1 [shape=doublecircle][label=""]
			}
			$edges 
		}"""
	}

	def toEdgeList(input: State, visited: mutable.Set[State] = mutable.Set[State]()): List[Edge] = {
		if (visited contains input) {
			Nil
		} else {
			visited.add(input)
			input match {
				case c @ Consume(char, next) => Edge(c.id, next.id, Some(char.toString)) :: 
					toEdgeList(next, visited)
				case split @ Split(l, r) => Edge(split.id, l.id) :: 
					Edge(split.id, r.id) :: (toEdgeList(l, visited) ++ toEdgeList(r, visited))
				case Match() => Nil
			}
		}		
	}
}

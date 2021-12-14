import scala.io.Source

object Day14 {
    def split_initial_state(str: String) : List[String] = if (str.length() > 2) (str.slice(0,2)::split_initial_state(str.slice(1,str.length())))   else List(str.slice(0,2))

    def count_occurences[A](l: List[A]) =l.distinct map(x => (x, l.count(_ == x).longValue))

    def do_step(state:(String, Long), rules: Map[String, String]) = List((state._1(0) + rules(state._1), state._2), (rules(state._1) + state._1(1), state._2)) 

    def merge_counts[A](counts:List[(A, Long)]) = counts.groupBy(_._1).toList.map(t => (t._1, t._2.unzip._2.sum))
    
    def do_steps(states:List[(String, Long)], rules: Map[String, String], steps: Int):List[(String, Long)] = {
        steps match {
            case 0 => states
            case _ => do_steps(merge_counts(states.map(s => do_step(s, rules)).flatten), rules, steps - 1) 
        }
    }
    
    def solve(states:List[(String, Long)], rules: Map[String, String], steps: Int) = {
        var result = do_steps(states, rules, steps)
        var molecules = merge_counts(result.map(x => List((x._1(0), x._2), (x._1(1), x._2))).flatten).sortBy(_._2)  
        var molecule_counts = molecules.unzip._2

        // Divide by 2 since we overcount everything
        println((molecule_counts.max/2) - (molecule_counts.min/2) + 1)
    }

    def main(args: Array[String]) = {
        var lines = Source.fromFile("input.txt").getLines().toList
        var initial_state = count_occurences(split_initial_state(lines(0).toString))
        var rules = lines.slice(2,lines.length).toList.map(x => (x split " -> " take 2)).map(x => (x(0), x(1))).toMap
        solve(initial_state, rules, 10)
        solve(initial_state, rules, 40)
    }
}
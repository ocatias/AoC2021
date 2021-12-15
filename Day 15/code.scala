import scala.io.Source
import Array._
import scala.util.control.Breaks._




object Day15 {
    def find_path_cost(board:Array[Array[Int]]) {
        var height = board.length
        var width = board(0).length

        // INITIALIZE
        var cost = ofDim[Int](height, width)

        var distance = ofDim[Int](height, width)
        var predecessor = ofDim[(Int, Int)](height, width)
        var Q = List[(Int,Int)]((0,0))
        var already_visited = ofDim[Boolean](height, width)
        for( y <- 0 until height; x <- 0 until width){
            cost(y)(x) = 999999999
            already_visited(y)(x) = false
        }
        cost(0)(0) = 0
        
        while(Q.length > 0){
            var (curr_y,curr_x) = Q(0)
            var lowest_cost = cost(curr_y)(curr_x)
    
            // Remove found element from Q
            Q = Q.drop(1)
            already_visited(curr_y)(curr_x) = true

            var neighbors = List[(Int,Int)]()
            if (curr_y < height -1)
                neighbors = (curr_y+1,curr_x)::neighbors
            if (curr_y > 0)
                neighbors = (curr_y-1,curr_x)::neighbors
            if (curr_x > 0)
                neighbors = (curr_y,curr_x-1)::neighbors
            if (curr_x < width - 1)
                neighbors = (curr_y,curr_x+1)::neighbors


            for ((y, x) <- neighbors)
            {
                if (!already_visited(y)(x))
                {
                    var new_dist = cost(curr_y)(curr_x) + board(y)(x)
                    if (new_dist < cost(y)(x))
                    {
                        cost(y)(x) = new_dist
                        predecessor(y)(x) = (curr_y, curr_x)
                    }

                    if (!Q.contains((y,x)))
                        Q = (y,x)::Q
                }
            }
            Q = Q.sortWith((p1, p2) => cost(p1._1)(p1._2) < cost(p2._1)(p2._2))
        }
        println(cost(height-1)(width-1).toString)
    }

    def main(args: Array[String]) = {

        var board = Source.fromFile("input.txt").getLines().toArray.map(l => l.toArray.map(c => c.asDigit))
        var height = board.length
        var width = board(0).length

        var board2 = ofDim[Int](5*height, 5*width)
        for( y <- 0 until height; x <- 0 until width; delta_y <- 0 until 5; delta_x <- 0 until 5){
            board2(y+delta_y*height)(x+delta_x*width) = ((board(y)(x) + delta_x + delta_y - 1) % 9) +  1
        }
        find_path_cost(board)
        find_path_cost(board2)
    }
}
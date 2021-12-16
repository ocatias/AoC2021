import scala.io.Source
import Array._
import scala.util.control.Breaks._




object Day16 {
 

    case class Package(version: Int, id: Int, value: Long, subpackages: List[Package])

    // Returns: found package, remaining string, remaining max_length
    def parse_string(str: String, max_length: Int): (Package, String) = {
        var working_str:String = str
        var length = max_length
        var version = Integer.parseInt(working_str.slice(0,3), 2)
        var id = Integer.parseInt(working_str.slice(3,6), 2)
        working_str = working_str.substring(6, working_str.length)
        length = length - 6

        // Literal
        if (id == 4)
        {
            var bits_value = ""
            var continue = true
            while(continue)
            {
                var new_values = working_str.slice(0,5)
                working_str = working_str.substring(5, working_str.length)
                bits_value += new_values.slice(1,5)

                if (new_values(0) == '0')
                    continue = false

                length = length - 5
            }

            var value = BigInt(bits_value, 2).toLong
            (Package(version, id, value, List[Package]()), working_str)

        }

        // Operator
        else {
            var packages = List[Package]()
            var return_string = ""
            if (working_str(0) == '0')
            {
                length = Integer.parseInt(working_str.substring(1, 16), 2)
                working_str = working_str.substring(16, working_str.length)
                return_string = working_str.substring(length, working_str.length)
                working_str = working_str.substring(0, length)
                
                while(working_str.contains('1'))
                {
                    var (new_package, remaining_string) = parse_string(working_str, length)
                    packages ::= new_package
                    working_str = remaining_string
                }
            }
            else
            {
                var nr_packages = Integer.parseInt(working_str.substring(1, 12), 2)
                working_str = working_str.substring(12, working_str.length)
                for(_ <- 0 until nr_packages)
                {
                    var (new_package, remaining_string) = parse_string(working_str, length)
                    packages ::= new_package
                    working_str = remaining_string
                }
                return_string = working_str
            }

            // We add new packages in the front instead of the end of the list -> need to reverse it
            packages = packages.reverse
            (Package(version, id, 0, packages), return_string)
       }
    }

    def part1(p:Package):Int = {
        p.version + p.subpackages.map(p => part1(p)).sum
    }

    def part2(p:Package):Long = {
        p.id match {
            case 0 => p.subpackages.map(p => part2(p)).sum
            case 1 => p.subpackages.map(p => part2(p)).foldLeft(1:Long)((x, y) => x * y)
            case 2 => p.subpackages.map(p => part2(p)).min
            case 3 => p.subpackages.map(p => part2(p)).max
            case 4 => p.value
            case 5 => {
                if (part2(p.subpackages(0)) > part2(p.subpackages(1))) {1} else {0}
            }
            case 6 => {
                if (part2(p.subpackages(0)) < part2(p.subpackages(1))) {1} else {0}
            }
            case 7 => {
                if (part2(p.subpackages(0)) == part2(p.subpackages(1))) {1} else {0}
            }
        }
    }

    def main(args: Array[String]) = {
        var line_as_hex = Source.fromFile("input.txt").getLines.toList(0).toList

        var hexmap:Map[Char, String] = Map(
            '0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011",
            '4' -> "0100", '5' -> "0101", '6' -> "0110", '7' -> "0111",
            '8' -> "1000", '9' -> "1001", 'A' -> "1010", 'B' -> "1011",
            'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")

        var input = line_as_hex.map(c => hexmap(c)).flatten.mkString

        var bits = parse_string(input, input.length)
        println("Part 1")
        println(part1(bits._1))
        println("Part 2")
        println(part2(bits._1))
    }
}
import scala.io.Source

/**
 * Based on "em.c"
 */
class em {

    /* read in from main of file */
    val labels = Array[Label]()

    /* read in from top of file */
    var numLabels   = 0
    var numLabelers = 0
    var numImages   = 0
    var forPriorZ1  = 0.0  // I made this but it makes life easier

    /* arrays sized according to above */

    // set as 1 in his code
    val priorAlpha = Array[Double]()
    val priorBeta  = Array[Double]()

    // from E-step
    val priorProbZ1 = Array[Double]()
    val priorProbZ0 = Array[Double]()

    // from M-step
    val alpha = Array[Double]()
    val beta  = Array[Double]()

    // set as 0.5 by file
    val priorZ1 = Array[Double]()
}

object em {
    def main(args: Array[String]) {
        println("hello world")
        val dataLocation = "../../OptimalLabelingRelease1.0.3/data.txt"
        val lines = Source.fromFile(dataLocation).getLines()
        val runIt = new em()
        val something = lines.next().split(" ")
        runIt.numLabels = something(0).toInt
        runIt.numLabelers = something(1).toInt
        runIt.numImages = something(2).toInt
        runIt.forPriorZ1 = something(3).toDouble

        val a = 9  // doofus line

        
        for(line <- lines) {
            val lineData = line.split(" ")
            // more code shall go here
        }
    }
}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Based on "em.c"
 */

class Label {
    var imageIdx  = 0
    var labelerId = 0
    var label     = 0
}

class em {

    /* read in from main of file */
    val labels = new ArrayBuffer[Label]()

    /* read in from top of file */
    var numLabels   = 0
    var numLabelers = 0
    var numImages   = 0
    var forPriorZ1  = 0.0  // I made this but it makes life easier

    /* arrays sized according to above */

    // set as 1 in his code
    var priorAlpha = Array[Double]()
    var priorBeta = Array[Double]()

    // from E-step
    val priorProbZ1 = Array[Double]()
    val priorProbZ0 = Array[Double]()

    // from M-step
    val alpha = Array[Double]()
    val beta  = Array[Double]()

    // set as 0.5 by file
    var priorZ1 = Array[Double]()
}

object em {
    def main(args: Array[String]) {
        println("hello world")

        /* Read Data */
        val dataLocation = "../../OptimalLabelingRelease1.0.3/data.txt"
        val lines = Source.fromFile(dataLocation).getLines()
        val runIt = new em()
        val something = lines.next().split(" ")

        // extract metadata from first line
        runIt.numLabels   = something(0).toInt
        runIt.numLabelers = something(1).toInt
        runIt.numImages   = something(2).toInt
        runIt.forPriorZ1  = something(3).toDouble

        // initialize priors
        runIt.priorAlpha = Array.fill[Double](runIt.numLabelers)(1.0)
        runIt.priorBeta  = Array.fill[Double](runIt.numImages)(1.0)
        runIt.priorZ1    = Array.fill[Double](runIt.numImages)(runIt.forPriorZ1)

        // read in the data
        for(line <- lines) {
            val lineData = line.split(" ")
            val label = new Label()
            label.imageIdx = lineData(0).toInt
            label.labelerId = lineData(1).toInt
            label.label = lineData(2).toInt
            runIt.labels += label
        }

        /* Run EM */
        
    }
}
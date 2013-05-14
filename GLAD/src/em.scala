import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import math._


class Label {
    var imageIdx  = 0
    var labelerId = 0
    var label     = 0
}

object em {
    // read in from main of file
    val labels = new ArrayBuffer[Label]()

    // read in from top of file
    var numLabels   = 0
    var numLabelers = 0
    var numImages   = 0
    var forPriorZ1  = 0.0  // I made this but it makes life easier

    /* arrays sized according to above: */

    // generated from forPriorZ1
    val priorProbZ1 = Array[Double]()
    val priorProbZ0 = Array[Double]()

    // set as 1 in his code
    var priorAlpha = Array[Double]()
    var priorBeta = Array[Double]()

    // from E-step
    var probZ1 = Array[Double]()
    var probZ0 = Array[Double]()

    // from M-step
    val alpha = Array[Double]()
    val beta  = Array[Double]()

    // set as 0.5 by file
    var priorZ1 = Array[Double]()

    // zero for both intents and purposes
    val THRESHOLD: Double = 1E-5

    def eStep() {

        // this is probably terribly inefficient (I could try both @ once)
        // it might be 'cons'ing into a brand new array every time...
        probZ1 = priorZ1.map(log(_))
        probZ0 = priorZ1.map(1 - log(_))

        for (label <- labels) {
            val i = label.labelerId
            val j = label.imageIdx
            val lij = label.label

            // TODO I think this is what needs to be MULTICLASS
            probZ1(j) += logProbL(lij, 1, alpha(i), beta(j))
            probZ0(j) += logProbL(lij, 0, alpha(i), beta(j))
        }

        // "Exponentiate and renormalize"
        for (j <- 0 until numImages) {
            probZ1(j) = exp(probZ1(j))
            probZ0(j) = exp(probZ0(j))
            probZ1(j) = probZ1(j) / (probZ1(j) + probZ0(j))
            probZ0(j) = 1 - probZ1(j)
        }
    }

    def logProbL (l: Int, z: Int, alphaI: Double, betaJ: Double) = {
        if (z == l)
            0 - log(1 + exp(0 - exp(betaJ) * alphaI))
        else
            0 - log(1 + exp(exp(betaJ) * alphaI))
    }

    def computeQ() = {
        var Q = probZ1.map(x => x * log(x)).sum
        Q += probZ0.map(x => x * log(1 - x)).sum


        Q
    }

    def EM () {
        println("sum thin")

        /* initialize starting values */
        priorAlpha copyToArray alpha
        priorBeta copyToArray beta

        var Q = 0.0
        eStep()
        Q = computeQ()
    }
    def main(args: Array[String]) {
        println("hello world")

        /* Read Data */
        val dataLocation = "../../OptimalLabelingRelease1.0.3/data.txt"
        val lines = Source.fromFile(dataLocation).getLines()
        val something = lines.next().split(" ")

        // extract metadata from first line
        numLabels   = something(0).toInt
        numLabelers = something(1).toInt
        numImages   = something(2).toInt
        forPriorZ1  = something(3).toDouble

        // initialize priors
        priorAlpha = Array.fill[Double](numLabelers)(1.0)
        priorBeta  = Array.fill[Double](numImages)(1.0)
        priorZ1    = Array.fill[Double](numImages)(forPriorZ1)

        // read in the data
        for(line <- lines) {
            val lineData = line.split(" ")
            val label = new Label()
            label.imageIdx = lineData(0).toInt
            label.labelerId = lineData(1).toInt
            label.label = lineData(2).toInt
            labels += label
        }

        /* Run EM */
        em.EM()
    }
}
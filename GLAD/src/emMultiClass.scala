import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import math._

/* Implementation of GLAD algorithm (Whitehill et al) adapted from their C-code,
 * as well as a trial run of the Scala programming language
 */

class MultiLabel(val i: Int, val j: Int, val lij: Int) {
    def delta(k: Int) = if (k == lij) 1 else 0
}

object emMultiClass {

    // read in from main of file

    val labels  = new ArrayBuffer[MultiLabel]()
    val workers = new mutable.HashMap[String,Int]()
    val items   = new mutable.HashMap[String,Int]()
    val classes = new mutable.HashMap[String,Int]()

    // read in from gold file if exists
    val goldLabels = new mutable.HashMap[String, String]()

    // set according to data read in
    var numLabels   = 0
    var numLabelers = 0
    var numItems    = 0
    var numClasses  = 0
    var priorZk     = 0.0  // set as 1/K in main()

    /* arrays sized according to above: */

    // set as 1
    var priorAlpha = Array[Double]()
    var priorBeta  = Array[Double]()

    // from E-step
    var probZX = Array[Array[Double]]()

    // from M-step
    var alpha    = Array[Double]()
    var beta     = Array[Double]()
    var dQdAlpha = Array[Double]()
    var dQdBeta  = Array[Double]()

    // statistics
    var correct = 0
    var incorrect = 0

    def eStep() {

        for (lbl <- labels; categ <- 0 until numClasses)
            probZX(lbl.j)(categ) = log(priorZk) + logProbL(lbl.lij, categ, alpha(lbl.i), beta(lbl.j))

        /* "Exponentiate and renormalize" */
        for (j <- 0 until numItems) {
            probZX(j) = probZX(j).map(exp(_))
            val sum = probZX(j).sum
            probZX(j) = probZX(j).map(_ / sum)
        }
    }

    def logProbL (l: Int, z: Int, alpha: Double, beta: Double): Double = {
        if (z == l)
            getLogSigma(alpha, beta)
        else
            -log(numClasses - 1) + getLogOneMinusSigma(alpha, beta)
    }

    def gaussPDF(x: Double): Double = 1/sqrt(2*Pi) * exp(-pow(x,2)/2)

    def getSigma(alpha: Double, beta: Double): Double = 1/(1 + exp(-exp(beta) * alpha))

    def getLogSigma(alpha_i: Double, beta_j: Double): Double = {
        var logSigma = log(getSigma(alpha_i, beta_j))
        /* NOTE: "WHY THE IF" (No longer gets tripped though)
         * http://www.wolframalpha.com/input/?i=exp%28-exp%28x%29*y%29+with+x+from+0+to+10%2C+y+from+-10+to+1
         */
        if (logSigma isNegInfinity) {
            logSigma = -exp(beta_j) * alpha_i
//            println("logSigma isNegInfinity")
        }
        logSigma
    }

    def getLogOneMinusSigma(alpha_i: Double, beta_j: Double): Double = {
        var logOneMinusSigma = log(1 - getSigma(alpha_i, beta_j))
        if (logOneMinusSigma isNegInfinity) {
            logOneMinusSigma = -exp(beta_j) * alpha_i
//            println("logOneMinusSigma isNegInfinity")
        }
        logOneMinusSigma
    }

    def computeQ(): Double = {
        /* formula given as "Q = ..." on pg. 3 */
        var Q = probZX.flatten.map(_ * log(priorZk)).sum

        for (label <- labels; k <- 0 until numClasses)
            Q += probZX(label.j)(k) * logProbL(label.lij, k, alpha(label.i), beta(label.j))

        /* Add Gaussian (standard normal) prior for alpha and beta*/
        for (i <- 0 until numLabelers)
            Q += log(gaussPDF(alpha(i) - priorAlpha(i)))

        for (j <- 0 until numItems)
            Q += log(gaussPDF(beta(j) - priorBeta(j)))  // had bug in original code

        return Q
    }

    def ascend(stepSize: Double) {
        for (i <- 0 until numLabelers)
            alpha(i) += stepSize * dQdAlpha(i)
        for (j <- 0 until numItems)
            beta(j) = beta(j) + stepSize * dQdBeta(j)
    }

    def doGradientAscent(iterations: Int, stepSize: Double, tolerance: Double) {
        var iteration = 0
        var oldQ = computeQ()
        var Q = oldQ
        var cloneTuple = (alpha.clone(),beta.clone())
        do {
            oldQ = Q
            cloneTuple = (alpha.clone(),beta.clone())
            calcGradient()
            ascend(stepSize)
            Q = computeQ()
            iteration += 1
        } while (iteration < iterations && ((Q - oldQ)/oldQ).abs > tolerance && Q > oldQ)
        if (Q < oldQ) {
            alpha = cloneTuple._1  // no way to do multiple RE-assignment like in python :(
            beta = cloneTuple._2  // "Won't Fix": https://issues.scala-lang.org/browse/SI-1324
            println("\nAfter "+iteration+" iterations of M-step, Q-score fell.")
        }
    }

    // One must make the step size very small in order for every iteration
    // to actually increase the Q-score. This may be because there are so many
    // many parameters adjusted on each iteration that they cumulatively have a large effect
    // on the Q-score.
    def MStep() { doGradientAscent(25, 1E-2, 1E-2) }

    def calcGradient() {

        /* NOTE: dQdBeta is in terms of the REAL beta,
                    whereas the array is in terms of LOG beta */

        // Original had this part
        for (i <- 0 until numLabelers)
            dQdAlpha(i) = -(alpha(i) - priorAlpha(i))

        for (j <- 0 until numItems)
            dQdBeta(j) = -(beta(j) - priorBeta(j))

        /* I think doing this functionally is much slower
        dQdAlpha = (alpha, priorAlpha).zipped.map(_ - _)
        dQdBeta  = (beta, priorBeta).zipped.map(exp(_) - exp(_))
        */

        // Doing this instead seems to make no difference
        //        dQdAlpha = Array.fill(numLabelers)(0.0)
        //        dQdBeta  = Array.fill(numItems)(0.0)

        for (lbl <- labels) {
            val sigma = getSigma(beta(lbl.j),alpha(lbl.i))
            for (k <- 0 until numClasses; val delta: Int = lbl.delta(k)) {
                dQdAlpha(lbl.i) += probZX(lbl.j)(k) * ((delta - sigma) * exp(beta(lbl.j)) +
                        (1 - delta) * log(numClasses - 1))
                dQdBeta(lbl.j) += probZX(lbl.j)(k) * ((delta - sigma) * alpha(lbl.i) +
                        (1 - delta) * log(numClasses - 1))
            }
        }
    }

    def outputResults() {

        val flippedWorkers = workers map {_.swap}
        val flippedItems   = items   map {_.swap}
        val flippedClasses = classes map {_.swap}
        for (i <- 0 until numLabelers)  // needs this loop to print the index
            printf("Alpha[%d] = %f: %s\n", i, alpha(i), flippedWorkers.getOrElse(i, "ERROR"))

        for (j <- 0 until numItems)
            printf("Beta[%d] = %f: %s\n", j, exp(beta(j)), flippedItems.getOrElse(j, "ERROR"))

                for (j <- 0 until numItems; k <- 0 until numClasses) {
                        val theClass: String = flippedClasses.getOrElse(k, "ERROR")
                        val theItem: String = flippedItems.getOrElse(j, "ERROR")
                        printf("P(%s = %s) = %f\n", theItem, theClass, probZX(j)(k))
                }

        for (j <- 0 until numItems; k <- 0 until numClasses) {
            val theClass: String = flippedClasses.getOrElse(k, "ERROR")
            val theItem: String = flippedItems.getOrElse(j, "ERROR")
            if (probZX(j)(k) == probZX(j).max) {
                printf("%s: %s", theClass, theItem)
                /* print gold label info and calculate accuracy */
                if (goldLabels.size > 0) {
                    val goldLabel = goldLabels.getOrElse(theItem, "?")
                    if (goldLabel == "?")
                        println(": No gold label")
                    else if (goldLabel == theClass) {
                        println(": Correct")
                        correct += 1
                    } else {
                        println(": Incorrect")
                        incorrect += 1
                    }
                }
                else println()
            }
        }
        /* print Accuracy */
        if (goldLabels.size > 0) {
            println("#Correct = "+correct)
            println("#Incorrect = "+incorrect)
            val accuracy = correct.asInstanceOf[Double] / (correct + incorrect)
            println("Accuracy = "+accuracy)
        }
    }

    def EM () {
        println("beginning EM")
        var Q = computeQ()
        var lastQ = Q  // must be initialized outside loop for use in while condition
        var change = 0.0
        do {
            lastQ = Q
            /* Estimate P(Z|L,alpha,beta) */
            eStep()
            printf("\nAfter E-Step: Q = %f\n", computeQ())
            /* Estimate alpha and beta */
            MStep()
            Q = computeQ()
            change = ((Q-lastQ)/lastQ).abs
            printf("\nAfter M-Step: Q = %f\n", Q)
            printf("change-ratio is %.7f\n\n", change)
        } while (change > 1E-3)
        eStep()
        printf("Final Q = %f\n", computeQ())
        outputResults()
    }

    def main(args: Array[String]) {
        println("Reading Data")
//        val dataFile = "/Users/Ethan/ischool/crowdData/adaptedData/rawFiles/GAL/responses/AdultContent2_Responses.txt"
//        val dataFile = "/Users/Ethan/ischool/crowdData/adaptedData/rawFiles/WVSCM" +
//                "/responses/WVSCM_Responses.txt"
                val dataFile = "../../OptimalLabelingRelease1.0.3/dataNoMeta.txt"

//        val goldFile = "/Users/Ethan/ischool/crowdData/adaptedData/rawFiles/WVSCM/" +
//                "gold/WVSCM_gold.txt"
        val goldFile = ""

        /* extract metadata from the data itself */
        for (line <- Source.fromFile(dataFile).getLines()) {
            val crowdLabel = line.split("\t")
            // store all metadata in array
            if (!workers.contains(crowdLabel(1)))
                workers += (crowdLabel(1) -> workers.size)
            if (!items.contains(crowdLabel(0)))
                items += (crowdLabel(0) -> items.size)
            if (!classes.contains(crowdLabel(2)))
                classes += (crowdLabel(2) -> classes.size)

            labels += new MultiLabel(
                workers getOrElse (crowdLabel(1),-1),
                items   getOrElse (crowdLabel(0),-1),
                classes getOrElse (crowdLabel(2),-1)
            )
        }

        /* read in goldFile */
        if (goldFile != "") {
            for (line <- Source.fromFile(goldFile).getLines()) {
                val goldData = line.split("\t")
                goldLabels += ((goldData(0),goldData(1)))
            }
        }

        numLabels   = labels.length
        numLabelers = workers.size
        numItems    = items.size
        numClasses  = classes.size

        // initializations
        priorAlpha = Array.fill(numLabelers)(1.0)
        priorBeta  = Array.fill(numItems)(1.0)
        priorZk    = 1.0 / numClasses  // set p(z_j = k) = 1/k
        probZX     = new Array[Array[Double]](numItems)
        for (i <- 0 until numItems)  // gets null pointers without it
            probZX(i) = new Array[Double](numClasses)

        alpha = priorAlpha.clone()
        beta  = priorBeta.clone()
        dQdAlpha = new Array[Double](numLabelers)
        dQdBeta  = new Array[Double](numItems)

        EM()
    }
}

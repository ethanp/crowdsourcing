import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import math._

/*
    This is configured to read the GAL data "AdultContentX_Responses"

    The way to clean this up a lot is to read the data in a different object,
    then instantiate the emMultiClass object to perform all the calculations.
     In that way it would use a lot more vals and a lot less vars.
 */

class MultiLabel(val i: Int, val j: Int, val lij: Int) {
    def delta(k: Int) = if (k == lij) 1 else 0
}

object emMultiClass {

    /* fields go in the heap */

    // read in from main of file
    val labels = new ArrayBuffer[MultiLabel]()

    var numLabels   = 0
    var numLabelers = 0
    var numItems    = 0
    var numCategs   = 0
    val workers = new mutable.ArrayBuffer[String]()
    val items   = new mutable.ArrayBuffer[String]()
    val categs  = new mutable.ArrayBuffer[String]()

    /* arrays sized according to above: */

    // set as 1 in his code
    var priorAlpha = Array[Double]()
    var priorBeta  = Array[Double]()

    // from E-step
    var probZX = Array[Array[Double]]()

    // from M-step
    var alpha    = Array[Double]()
    var beta     = Array[Double]()
    var dQdAlpha = Array[Double]()
    var dQdBeta  = Array[Double]()

    // set as 1/K in main()
    var priorZk = 0.0

    def eStep() {

        for (array <- 0 until numCategs)
            probZX(array) = Array.fill[Double](numCategs)(log(priorZk))

        for (label <- labels; categ <- 0 until numCategs)
            probZX(label.j)(categ) += logProbL(label.lij, categ, alpha(label.i), beta(label.j))

        // "Exponentiate and renormalize"
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
            -log(numCategs - 1) + getLogOneMinusSigma(alpha, beta)
    }

    def zScore(x: Double): Double = 1/sqrt(2*Pi) * exp(-pow(x,2)/2)

    def getSigma(alpha: Double, beta: Double): Double = 1/(1 + exp(-exp(beta) * alpha))

    def getLogSigma(alpha_i: Double, beta_j: Double): Double = {
        var logSigma = log(getSigma(alpha_i, beta_j))
        /* NOTE: "WHY THE IF"
         * this would be neg-infinity if exp(-exp(beta(j)) * alpha(i))) ~= infinity
         * which would happen if alpha_i < 0 && beta_j > 4-ish
         * which would happen if it was a nefarious rater on an easy picture
         * e.g. using alpha,beta = -5,5 = e^(5e^5) = 1.9 E 322 !
         * a bad PLOT can be found here:
         * http://www.wolframalpha.com/input/?i=exp%28-exp%28x%29*y%29+with+x+from+0+to+10%2C+y+from+-10+to+1
         * The idea is that we're replacing it with e^b * a; IDK why they chose that,
         * but anyways that replacement would still be negative, but with
         * a much smaller absolute value */
        if (logSigma isNegInfinity) // this does happen periodically
            exp(beta_j) * alpha_i

        logSigma
    }

    def getLogOneMinusSigma(alpha_i: Double, beta_j: Double): Double = {
        var logOneMinusSigma = log(1 - getSigma(alpha_i, beta_j))

        // I don't understand why this makes sense, it DOES happen periodically though
        if (logOneMinusSigma isNegInfinity)
            -exp(beta_j) * alpha_i

        logOneMinusSigma
    }

    def computeQ(): Double = {
        var Q = 0.0
        /* formula given as "Q = ..." on pg. 3 */
        Q += probZX.flatten.map(_ * log(priorZk)).sum

        for (label <- labels) {
            for (k <- 0 until numCategs)
                Q += probZX(label.j)(k) * logProbL(label.lij, k, alpha(label.i), beta(label.j))
        }

        /* Add Gaussian (standard normal) prior for alpha and beta*/
        for (i <- 0 until numLabelers)
            Q += log(zScore(alpha(i) - priorAlpha(i)))

        for (j <- 0 until numItems)
            Q += log(zScore(beta(j) - priorBeta(j)))

        return Q
    }

    def ascend(stepSize: Double) {
        for (i <- 0 until numLabelers)
            alpha(i) += stepSize * dQdAlpha(i)
        for (j <- 0 until numItems)
            beta(j) += stepSize * dQdBeta(j)
    }

    def doGradientAscent(iterations: Int, stepSize: Double, tolerance: Double) {
        var iteration = 0
        var oldQ = computeQ()
        var Q = oldQ
        do {
            oldQ = Q
            calcGradient()
            ascend(stepSize)
            Q = computeQ()
            iteration += 1
        } while (iteration < iterations && abs(Q - oldQ) > tolerance)
    }

    def MStep () {
        // the algorithm is very sensitive to the settings of these parameters
        // for some value-sets, it won't ever terminate
        doGradientAscent(2, .001, .01)
    }

    def calcGradient () {

        // Theirs had this part
        dQdAlpha = for ((a, pA) <- alpha zip priorAlpha) yield a - pA
        dQdBeta  = for ((b, pB) <- beta zip priorBeta) yield b - pB

        // Mine does this instead (it seems to make no difference)
        /*
        dQdAlpha = Array.fill(numLabelers)(0.0)
        dQdBeta  = Array.fill(numItems)(0.0)
        */

        for (lbl <- labels) {
            val sigma = getSigma(beta(lbl.j),alpha(lbl.i))
            for (k <- 0 until numCategs) {
                dQdAlpha(lbl.i) += probZX(lbl.j)(k) * ((lbl.delta(k) - sigma) * exp(beta(lbl.j)) +
                        (1 - lbl.delta(k)) * log(numCategs - 1))
                dQdBeta(lbl.j) += probZX(lbl.j)(k) * ((lbl.delta(k) - sigma) * alpha(lbl.i) +
                        (1 - lbl.delta(k)) * log(numCategs - 1))
            }
        }
    }

    def outputResults() {

        for (i <- 0 until numLabelers)
            printf("Alpha[%d] = %f\n", i, alpha(i))

        for (j <- 0 until numItems)
            printf("Beta[%d] = %f\n", j, exp(beta(j)))

        for (j <- 0 until numItems; k <- 0 until numCategs)
                printf("P(%-40s = %s) = %f\n", items(j), categs(k), probZX(j)(k))

        for (j <- 0 until numItems; k <- 0 until numCategs)
             if (probZX(j)(k) == probZX(j).max)
                printf("%s: %s\n", categs(k), items(j))
    }

    def EM () {
        println("beginning EM")

        /* initialize starting values */
        alpha = priorAlpha.clone()
        beta  = priorBeta.clone()
        dQdAlpha = new Array[Double](numLabelers)
        dQdBeta  = new Array[Double](numItems)

        var Q = computeQ()
        var lastQ = Q

        do {
            lastQ = Q
            /* "Re-estimate P(Z|L,alpha,beta)" */
            eStep()
            println("\nAfter E-Step:")
            printf("Q = %f\n", computeQ())

            MStep()
            Q = computeQ()
            printf("\nAfter M-Step:\n")
            printf("Q = %f\n", Q)
            printf("difference is %.7f\n\n", abs((Q-lastQ)/lastQ))
        } while (abs((Q-lastQ)/lastQ) > 1E-3)
        eStep()
        printf("Q = %f\n", computeQ())
        outputResults()
    }
    def main(args: Array[String]) {
        println("Reading Data")

        /* Read Data */
        val dataFile = "/Users/Ethan/ischool/crowdData/adaptedData/rawFiles/GAL/responses/AdultContent2_Responses.txt"
//        val dataFile = "/Users/Ethan/ischool/crowdData/adaptedData/WVSCM/WVSCM/workerResponsesMatlab.txt"

        /* extract metadata from the data itself */
        for (line <- Source.fromFile(dataFile).getLines()) {
            val stringArr = line.split("\t")
            // store all metadata in array
            if (!workers.contains(stringArr(0)))
                workers += stringArr(0)
            if (!items.contains(stringArr(1)))
                items += stringArr(1)
            if (!categs.contains(stringArr(2)))
                categs += stringArr(2)

            labels += new MultiLabel(
                workers.indexOf(stringArr(0)),
                items.indexOf(stringArr(1)),
                categs.indexOf(stringArr(2))
            )
        }

        numLabels   = labels.length
        numLabelers = workers.size
        numItems    = items.size
        numCategs   = categs.size

        // initialize priors
        priorAlpha = Array.fill[Double](numLabelers)(1.0)
        priorBeta  = Array.fill[Double](numItems)(1.0)
        priorZk    = 1.0 / numCategs  // set p(z_j = k) = 1/k
        probZX     = new Array[Array[Double]](numItems)
        for (i <- 0 until numItems)
            probZX(i) = new Array[Double](numCategs)

        EM()
    }
}


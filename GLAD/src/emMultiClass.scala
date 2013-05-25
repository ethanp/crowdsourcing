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
    var numClasses  = 0
    val workers = new mutable.ArrayBuffer[String]()
    val items   = new mutable.ArrayBuffer[String]()
    val classes  = new mutable.ArrayBuffer[String]()

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

        for (lbl <- labels; categ <- 0 until numClasses)
            probZX(lbl.j)(categ) = log(priorZk) + logProbL(lbl.lij, categ, alpha(lbl.i), beta(lbl.j))

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
            -log(numClasses - 1) + getLogOneMinusSigma(alpha, beta)
    }

    // pdf of Gaussian Distribution
    def zScore(x: Double): Double = 1/sqrt(2*Pi) * exp(-pow(x,2)/2)

    def getSigma(alpha: Double, beta: Double): Double = 1/(1 + exp(-exp(beta) * alpha))

    def getLogSigma(alpha_i: Double, beta_j: Double): Double = {
        val logSigma = log(getSigma(alpha_i, beta_j))
        /* NOTE: "WHY THE IF"
         * this would be neg-infinity if exp(-exp(beta(j)) * alpha(i))) ~= infinity
         * a bad PLOT can be found here:
         * http://www.wolframalpha.com/input/?i=exp%28-exp%28x%29*y%29+with+x+from+0+to+10%2C+y+from+-10+to+1
         */
        if (logSigma isNegInfinity) {// this does happen periodically
            exp(beta_j) * alpha_i
            println("logSigma isNegInfinity")
        }
        logSigma
    }

    def getLogOneMinusSigma(alpha_i: Double, beta_j: Double): Double = {
        val logOneMinusSigma = log(1 - getSigma(alpha_i, beta_j))
        if (logOneMinusSigma isNegInfinity) {
            -exp(beta_j) * alpha_i
            println("logOneMinusSigma isNegInfinity")
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
            Q += log(zScore(alpha(i) - priorAlpha(i)))

        for (j <- 0 until numItems)
            Q += log(zScore(exp(beta(j)) - exp(priorBeta(j))))

        return Q
    }

    def ascend(stepSize: Double) {
        for (i <- 0 until numLabelers)
            alpha(i) += stepSize * dQdAlpha(i)
        for (j <- 0 until numItems)
            beta(j) = log(exp(beta(j)) + stepSize * dQdBeta(j))
    }

    def doGradientAscent(iterations: Int, stepSize: Double, tolerance: Double) {
        var iteration = 0
        var oldQ = computeQ()
        var Q = oldQ
        var alphas = alpha.clone()
        var betas = beta.clone()
        do {
            oldQ = Q
            alphas = alpha.clone()
            betas = beta.clone()
            calcGradient()
            ascend(stepSize)
            Q = computeQ()
            iteration += 1
        } while (iteration < iterations && abs((Q - oldQ)/oldQ) > tolerance && Q > oldQ)
        if (Q < oldQ) {
            alpha = alphas.clone()
            beta = betas.clone()
            println("\nAfter "+iteration+" iterations of M-step, Q-score fell.")
        }
    }

    // It seems you have to make the step size very small in order for every iteration
    // to actually increase the Q-score. I believe this is because there are so many
    // adjustments to so many parameters on each iteration that it has a large affect
    // on the resulting Q-score.
    def MStep() { doGradientAscent(10000, 1E-4, 1E-10) }

    def calcGradient() {

        /* NOTE: dQdBeta is in terms of the REAL beta,
                    whereas the array is in terms of LOG beta */

        // Theirs had this part
        dQdAlpha = for ((a, pA) <- alpha zip priorAlpha) yield a - pA
        dQdBeta  = for ((b, pB) <- beta zip priorBeta) yield exp(b) - exp(pB)

        // Mine does this instead (it seems to make no difference)
//        dQdAlpha = Array.fill(numLabelers)(0.0)
//        dQdBeta  = Array.fill(numItems)(0.0)

        for (lbl <- labels) {
            val sigma = getSigma(beta(lbl.j),alpha(lbl.i))
            for (k <- 0 until numClasses) {
                dQdAlpha(lbl.i) += probZX(lbl.j)(k) * ((lbl.delta(k) - sigma) * exp(beta(lbl.j)) +
                        (1 - lbl.delta(k)) * log(numClasses - 1))
                dQdBeta(lbl.j) += probZX(lbl.j)(k) * ((lbl.delta(k) - sigma) * alpha(lbl.i) +
                        (1 - lbl.delta(k)) * log(numClasses - 1))
            }
        }
    }

    def outputResults() {

        for (i <- 0 until numLabelers)
            printf("Alpha[%d] = %f: %s\n", i, alpha(i), workers(i))

        for (j <- 0 until numItems)
            printf("Beta[%d] = %f: %s\n", j, exp(beta(j)), items(j))

//        for (j <- 0 until numItems; k <- 0 until numClasses)
//                printf("P(%-40s = %s) = %f\n", items(j), classes(k), probZX(j)(k))

//        for (j <- 0 until numItems; k <- 0 until numClasses)
//             if (probZX(j)(k) == probZX(j).max)
//                printf("%s: %s\n", classes(k), items(j))
    }

    def EM () {
        println("beginning EM")

        /* initialize starting values */
        alpha = priorAlpha.clone()
        beta  = priorBeta.clone()
        dQdAlpha = new Array[Double](numLabelers)
        dQdBeta  = new Array[Double](numItems)

        var Q = computeQ()
        var lastQ = Q  // must be initialized outside loop for use in while condition

        do {
            lastQ = Q
            /* "Re-estimate P(Z|L,alpha,beta)" */
            eStep()
            printf("\nAfter E-Step: Q = %f\n", computeQ())

            /* Re-estimate alphas and betas */
            MStep()
            Q = computeQ()
            printf("\nAfter M-Step: Q = %f\n", Q)
            printf("change-ratio is %.7f\n\n", abs((Q-lastQ)/lastQ))
        } while (abs((Q-lastQ)/lastQ) > 1E-3)
        eStep()
        printf("Q = %f\n", computeQ())
        outputResults()
    }
    def main(args: Array[String]) {
        println("Reading Data")

        /* Read Data */
        val dataFile = "/Users/Ethan/ischool/crowdData/adaptedData/rawFiles/GAL/responses/AdultContent2_Responses.txt"

//        val dataFile = "/Users/Ethan/ischool/crowdData/adaptedData/rawFiles/WVSCM" +
//                "/responses/WVSCM_Responses.txt"

//        val dataFile = "../../OptimalLabelingRelease1.0.3/dataNoMeta.txt"

        /* extract metadata from the data itself */
        for (line <- Source.fromFile(dataFile).getLines()) {
            val crowdLabel = line.split("\t")
            // store all metadata in array
            if (!workers.contains(crowdLabel(0)))
                workers += crowdLabel(0)
            if (!items.contains(crowdLabel(1)))
                items += crowdLabel(1)
            if (!classes.contains(crowdLabel(2)))
                classes += crowdLabel(2)

            labels += new MultiLabel(
                workers indexOf crowdLabel(0),
                items   indexOf crowdLabel(1),
                classes indexOf crowdLabel(2)
            )
        }
//        println(workers.indexOf("AZ1S5BIG5R6P6"))

        numLabels   = labels.length
        numLabelers = workers.size
        numItems    = items.size
        numClasses  = classes.size

        // initialize priors
        priorAlpha = Array.fill[Double](numLabelers)(1.0)
        priorBeta  = Array.fill[Double](numItems)(1.0)
        priorZk    = 1.0 / numClasses  // set p(z_j = k) = 1/k
        probZX     = new Array[Array[Double]](numItems)
        for (i <- 0 until numItems)
            probZX(i) = new Array[Double](numClasses)

        EM()
    }
}


import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import math._

/*
    This is configured to read the GAL data "AdultContentX_Responses"
 */

class MultiLabel {
    var itemIdx  = 0
    var labelerId = 0
    var label     = 0
}

object emMultiClass {
    // read in from main of file
    val labels = new ArrayBuffer[MultiLabel]()

    var numLabels   = 0
    var numLabelers = 0
    var numItems    = 0
    var numCategs   = 0

    /* arrays sized according to above: */

    // generated from forPriorZ1
    val priorProbZ1 = Array[Double]()
    val priorProbZ0 = Array[Double]()

    // set as 1 in his code
    var priorAlpha = Array[Double]()
    var priorBeta  = Array[Double]()

    // from E-step
    var probZ1 = Array[Double]()
    var probZ0 = Array[Double]()

    // from M-step
    var alpha      = Array[Double]()
    var beta       = Array[Double]()
    var dQdAlpha   = Array[Double]()
    var dQdBeta    = Array[Double]()

    // set as 0.5 by file
    var priorZk = 0.0

    // for both intents and purposes
    val THRESHOLD: Double = 1E-5

    def eStep() {

        probZ1 = Array.fill[Double](numItems)(log(priorZk))
        probZ0 = Array.fill[Double](numItems)(1-log(priorZk))

        for (label <- labels) {
            val i   = label.labelerId
            val j   = label.itemIdx
            val lij = label.label

            probZ1(j) += logProbL(lij, 1, alpha(i), beta(j))
            probZ0(j) += logProbL(lij, 0, alpha(i), beta(j))
        }

        // "Exponentiate and renormalize"
        for (j <- 0 until numItems) {
            probZ1(j) = exp(probZ1(j))
            probZ0(j) = exp(probZ0(j))
            probZ1(j) = probZ1(j) / (probZ1(j) + probZ0(j))
            probZ0(j) = 1 - probZ1(j)
        }
    }

    def logProbL (l: Int, z: Int, alphaI: Double, betaJ: Double): Double = {
        if (z == l)
            0 - log(1 + exp(- exp(betaJ) * alphaI))
        else
            0 - log(1 + exp(exp(betaJ) * alphaI))
    }

    def zScore(x: Double): Double = 1/sqrt(2*Pi) * exp(-pow(x,2)/2)

    def computeQ(): Double = {

        var Q = 0.0

        /* "Start with the expectation of the sum of priors over all images" */
        // first line of pg. 2: SS{ p(k) * ln(p(z)) }
        for (j <- 0 until numItems) {
            Q += probZ1(j) * log(priorZk)
            Q += probZ0(j) * log(1 - priorZk)
        }

        // second line of pg. 2: SS{ p(k) * ln(p(l|z,a,b)) }
        for (label <- labels) {
            val i   = label.labelerId
            val j   = label.itemIdx
            val lij = label.label

            /* "Do some analytic manipulation first for numerical stability!" */
            // the supp. calls this "ln\sigma"
            var logSigma = -log(1 + exp(-exp(beta(j)) * alpha(i)))

            /* NOTE: "WHY THE IF"
             * this would be neg-infinity if exp(-exp(beta(j)) * alpha(i))) ~= infinity
             * which would happen if alpha_i < 0 && beta_j > 4-ish
             * which would happen if it was a nefarious rater on an easy picture
             * e.g. using alpha,beta = -5,5 = e^(5e^5) = 1.9 E 322 !
             * a bad PLOT can be found here:
             * http://www.wolframalpha.com/input/?i=exp%28-exp%28x%29*y%29+with+x+from+0+to+10%2C+y+from+-10+to+1
             * The idea is that we're replacing it with e^b * a; IDK why they chose that,
             * but anyways that replacement would still be negative, but with
             * a much smaller absolute value
             */
            if (logSigma isNegInfinity)
                logSigma = exp(beta(j)) * alpha(i)


            var logOneMinusSigma = -log( 1 + exp( exp(beta(j)) * alpha(i) ) )

            // see NOTE above, except now it's for /positive/ alpha_i
            if (logOneMinusSigma isNegInfinity)
                logOneMinusSigma = -exp(beta(j)) * alpha(i)

            // from the final formulation of Q(a,b), midway down pg. 2
            Q += probZ1(j) * (lij * logSigma + (1 - lij) * logOneMinusSigma) +
                    probZ0(j) * ((1 - lij) * logSigma + lij * logOneMinusSigma)
        }

        // this isn't specified by the model, but seemed like a good idea to the authors:
        /* Add Gaussian (standard normal) prior for alpha and beta*/
        for (i <- 0 until numLabelers)
            Q += log(zScore(alpha(i) - priorAlpha(i)))
        for (j <- 0 until numItems)
            Q += log(zScore(beta(j) - priorBeta(j)))

        return Q
    }

    def logistic(x: Double): Double = 1.0 / (1 + exp(-x))

    /* The Likelihood is not used by the model in determining values this method
        is simply there to increase awareness of how the model is performing */
    def computeLikelihood(): Double = {
        var L = 0.0

        for (j <- 0 until numItems) {
            var P1 = priorZk(j)
            var P0 = 1 - priorZk(j)
            for (idx <- 0 until numLabels) {
                if (labels(idx).itemIdx == j) {
                    val i = labels(idx).labelerId
                    val lij = labels(idx).label
                    val sigma = logistic(exp(beta(j)) * alpha(i))
                    P1 *= pow(sigma, lij) * pow(1 - sigma, 1 - lij)
                    P0 *= pow(sigma, 1 - lij) * pow(1 - sigma, lij)
                }
            }
            L += log(P1 + P0)
        }

        /* Add Gaussian (standard normal) prior for alpha and beta */
        for (i <- 0 until numLabelers)
            L += log(zScore(alpha(i) - priorAlpha(i)))
        for (j <- 0 until numItems)
            L += log(zScore(beta(j) - priorBeta(j)))

        return L
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
            calcGradientComponents()
            ascend(stepSize)
            Q = computeQ()
            iteration += 1
        } while (iteration < iterations && Q - oldQ > tolerance)
    }

    def MStep () {
        printf("Before GradientAscent Q = %f\n", computeQ())

        // the algorithm is very sensitive to the settings of these parameters
        // I have found that (25, .001, .01) works well and matches the given output
        doGradientAscent(25, .001, .01)

        printf("After GradientAscent Q = %f\n", computeQ())
    }

    // TODO This is NOT quite the formula they worked out in the notes !!
    // I'm going to use it anyway; to change it to their derivation would be easy
    def calcGradientComponents () {

        // Where did this part come from?
        for (i <- 0 until numLabelers)
            dQdAlpha(i) = alpha(i) - priorAlpha(i)
        for (j <- 0 until numItems)
            dQdBeta(j) = beta(j) - priorBeta(j)

        // Why isn't it this?:
        /*
        for (i <- 0 until numLabelers)
            dQdAlpha(i) = 0.0
        for (j <- 0 until numImages)
            dQdBeta(j) = 0.0
        */

        for (label <- labels) {
            val i     = label.labelerId
            val j     = label.itemIdx
            val lij   = label.label
            val sigma = logistic(exp(beta(j)) * alpha(i))

            // What is this?
            dQdAlpha(i) += (probZ1(j) * (lij - sigma) +
                    probZ0(j) * (1 - lij - sigma)) * exp(beta(j))
            dQdBeta(j) += (probZ1(j) * (lij - sigma) +
                    probZ0(j) * (1 - lij - sigma)) * alpha(i) * exp(beta(j))
        }
    }

    def outputResults() {

        for (i <- 0 until numLabelers)
            printf("Alpha[%d] = %f\n", i, alpha(i))

        for (j <- 0 until numItems)
            printf("Beta[%d] = %f\n", j, exp(beta(j)))

        for (j <- 0 until numItems)
            printf("P(Z(%d)=1) = %f\n", j, probZ1(j))
    }

    def EM () {
        println("sum thin")

        /* initialize starting values */
        alpha = priorAlpha.map(x => x)
        beta = priorBeta.map(x => x)
        dQdAlpha = new Array[Double](numLabelers)
        dQdBeta = new Array[Double](numItems)

        var Q = 0.0
        var lastQ = 0.0

        eStep()         // not sure why these 2 lines are in here,
        Q = computeQ()  // they are repeated in the loop

        printf("Q = %f\n", Q)
        do {
            lastQ = Q
            /* "Re-estimate P(Z|L,alpha,beta)" */
            eStep()
            Q = computeQ()
            println("\nAfter E-Step:")
            printf("Q = %f; L = %f\n", Q, computeLikelihood())

            /*outputResults(data) [not implemented]*/
            MStep()

            Q = computeQ()
            printf("\nAfter M-Step:\n")
            printf("Q = %f; L = %f\n", Q, computeLikelihood())
            printf("difference is %.7f\n\n", abs((Q-lastQ)/lastQ))
        } while (abs((Q-lastQ)/lastQ) > THRESHOLD)

        outputResults()
    }
    def main(args: Array[String]) {
        println("hello world")

        /* Read Data */
//        val dataLocation = "../../OptimalLabelingRelease1.0.3/data.txt"  // original data
        val dataLocation = "../../AashishsCode/Crowd_data/rawFiles/GAL/responses" +
                "/AdultContent1_Responses.txt"
        val lines = Source.fromFile(dataLocation).getLines()

        /*
        // extract metadata from first line
        numLabels   = something(0).toInt
        numLabelers = something(1).toInt
        numImages   = something(2).toInt
        forPriorZ1  = something(3).toDouble
        */

        // extract metadata from the data itself
        val workers = new mutable.ArrayBuffer[String]()
        val items = new mutable.ArrayBuffer[String]()
        val categs = new mutable.ArrayBuffer[String]()
        var stringArr = Array[String]()

        for (line <- lines) {
            stringArr = line.split("\t")
            // store all metadata in array (implicit map [int -> dataName])
            if (!workers.contains(stringArr(0))) workers  += stringArr(0)
            if (!items.contains(stringArr(1)))   items    += stringArr(1)
            if (!categs.contains(stringArr(1)))  categs   += stringArr(2)

            val label = new MultiLabel()

            // store data itself in MultiLabel objects using mapped int from above arrays
            label.labelerId = workers indexOf stringArr(0)
            label.itemIdx   = items   indexOf stringArr(1)
            label.label     = categs  indexOf stringArr(2)
            labels += label
        }

        numLabels   = lines.length
        numLabelers = workers.size
        numItems    = items.size
        numCategs   = categs.size

        // initialize priors
        priorAlpha = Array.fill[Double](numLabelers)(1.0)
        priorBeta  = Array.fill[Double](numItems)(1.0)
        priorZk    = 1.0 / numCategs  // set p(z_j = k) = 1/k

        EM()
    }
}


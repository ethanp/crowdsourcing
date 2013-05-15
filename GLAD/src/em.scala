import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import math._
import org.apache.commons.math3.optim.nonlinear.vector.MultivariateVectorOptimizer._

/** TODO: Make it MultiClass */
/* TODO when debugging, he has a lot of builtin printf's I can utilize */

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
    var alpha = Array[Double]()
    var beta  = Array[Double]()

    // set as 0.5 by file
    var priorZ1 = Array[Double]()

    // for both intents and purposes
    val THRESHOLD: Double = 1E-5

    def eStep() {

        // this is probably terribly inefficient (I could try both @ once)
        // it might be 'cons'ing into a brand new array every time...
        probZ1 = priorZ1.map(log(_))
        probZ0 = priorZ1.map(1 - log(_))

        for (label <- labels) {
            val i   = label.labelerId
            val j   = label.imageIdx
            val lij = label.label

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
        for (j <- 0 until numImages) {
            Q += probZ1(j) * log(priorZ1(j))
            Q += probZ0(j) * log(1 - priorZ1(j))
        }

        // second line of pg. 2: SS{ p(k) * ln(p(l|z,a,b)) }
        for (label <- labels) {
            val i   = label.labelerId
            val j   = label.imageIdx
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
        for (j <- 0 until numImages)
            Q += log(zScore(beta(j) - priorBeta(j)))

        return Q
    }

    def logistic(x: Double): Double = 1.0 / (1 + exp(-x))

    /* The Likelihood is not used by the model in determining values this method
        is simply there to increase awareness of how the model is performing */
    def computeLikelihood(): Double = {
        var L = 0.0

        for (j <- 0 until numImages) {
            var P1 = priorZ1(j)
            var P0 = 1 - priorZ1(j)
            for (idx <- 0 until numLabels) {
                if (labels(idx).imageIdx == j) {
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
        for (j <- 0 until numImages)
            L += log(zScore(beta(j) - priorBeta(j)))

        return L
    }

    /* "packX" into vector, first alphas, then betas */
    def pack(vector: Array[Double]) {
        for (i <- 0 until numLabelers)
            vector(i) = alpha(i)
        for (j <- numLabelers until numLabelers + numImages)
            vector(j) = beta(j-numLabelers)
    }

    /* "unpackX" from vector into alpha and beta arrays */
    def unpack(vector: Array[Double]) {
        for (i <- 0 until numLabelers)
            alpha(i) = vector(i)
        for (j <- numLabelers until numLabelers + numImages)
            beta(j-numLabelers) = vector(j)
    }

    def MStep () {
        val vector = new Array[Double](numLabelers + numImages)
        pack(vector)

    }

    def EM () {
        println("sum thin")

        /* initialize starting values */
        alpha = priorAlpha.map(x => x)
        beta = priorBeta.map(x => x)

        /* THESE DIDN'T DO IT
        priorAlpha.copyToArray(alpha)
        priorBeta copyToArray beta
        */

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
        EM()
    }
}
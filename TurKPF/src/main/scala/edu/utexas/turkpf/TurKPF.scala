/**
 * Adapts TurKontrol into a Particle Filter
 * TurKontrol was presented in "Decision-Theoretic Control of Crowd-Sourced Workflows"
 * by Peng Dai, Mausam, and Daniel S. Weld (2010)
 *
 * Author:          Ethan Petuchowski
 * Date Started:    6/18/13
 * License:         Unknown
 */

import math._
import org.apache.commons.math3.distribution.{BetaDistribution, NormalDistribution}
import scala.util.Random

/* this means one can choose a set of parameters by replacing this line with
 *  import SecondExperiment._  and so on  */
import FirstExperiment._

/* TODO: the LookAhead */
/* TODO: it shouldn't let you spend more than your allowance */

/* Particle Filter representation of artifact quality probability density functions */
case class QualityDistribution(numParticles: Int, particles: Array[Double])
{
    def this(n: Int) = this(n, new BetaDistribution(1,9).sample(n))

    def this(particles: Array[Double]) = this(NUM_PARTICLES, particles)

    def this() = this(NUM_PARTICLES, new BetaDistribution(1,9).sample(NUM_PARTICLES))

    // [DTC] (eq. 13)
    def find_improvementFunctionMean(qlty: Double): Double = {
        val accuracy: Double = qstn.wrkrs.accuracy(qlty)
        qlty + 0.5 * ((1 - qlty) * (accuracy - 0.5) + qlty * (accuracy - 1))
    }

    // [DTC] § Experimental Setup
    def improvementDistr(qlty: Double) = {
        val mu = find_improvementFunctionMean(qlty)
        new BetaDistribution(10 * mu, 10 * (1 - mu))
    }

    // [DTC] (eq. 1), q => generate f_{ Q' | particle.q } (q') and sample from it
    /* This function is possibly the most important of this whole thing.
     *
     * THE WAY THIS WORKS:
     * We go through each particle, and using its value (which is an estimate of the qlty),
     * we generate an "improvement distribution" which is f_{ Q' | particle.q } (q').
     * This distribution describes how much we can expect [an avg.] worker to improve the
     * quality of the artifact. And since (eq. 1) is integrating over (q) and not (q'),
     * we don't need to use the entire "improvement distribution", instead we want to take a
     * "random" stab at where quality will be after the "improvement job". So we can sample
     * from the "improvement distribution" instead of doing anything snazzier with it.
     */
    // TODO: This might be a faster way to implement this: (generating directly, not sampling)
    // http://doodleproject.sourceforge.net/numerics/numerics4j/1.3/api/net/sf/doodleproject/numerics4j/random/BetaRandomVariable.html
    def predict: QualityDistribution =
        new QualityDistribution(particles map {improvementDistr(_).sample})

    /* fold ALL the partBs through prob_true() with EACH partA
     * [DTC] (eq. 5-6)
     */
    def weight_and_sample(vote: Boolean, that: QualityDistribution):
    QualityDistribution = {

        // get P( b_{n+1} | q ) :  [DTC] (eq. 6)
        val rawWeights = this.particles map { partA =>
            (0.0 /: that.particles)((sum, partB) =>
                sum + qstn.prob_true_given_Qs(partA, partB))
        }

        // get f_{ Q | b_{n+1} } (q) :  [DTC] (eq. 5)
        val weightNorm = rawWeights.sum
        val weights = rawWeights map { _ / weightNorm } // normalize weights, works
        QualityDistribution(NUM_PARTICLES,
            (1 to NUM_PARTICLES).toArray map {
                _ => random_sample_given_weights(weights)
            }
        )
    }

    /* algorithm for sampling from given set of points with associated weights:
     * generate a random number in [0,1), use the CDF of the weights to use the
     * random number to figure out what the sampled value is
     *
     * I debugged this function and it works as expected
     */
    def random_sample_given_weights(weights: Array[Double]): Double = {
        val rand = Random.nextDouble
        var accrue = 0.0
        for ((weight, index) <- weights.zipWithIndex) {
            accrue += weight
            if (rand < accrue)
                return this.particles(index)
        }
        throw new RuntimeException // shouldn't ever get here
        return particles(particles.length-1)
    }


    // avg loc of particles in associated Particle Filter
    // this doesn't actually make an appearance in the algorithm,
    // it's just for debugging
    def meanQltyEst: Double = particles.sum / NUM_PARTICLES
}

/* I am modelling all workers with just one worker-independent model */
case class Workers(trueGX: Double)
{
    val LEARNING_RATE = 0.05
    var estGX: Double = 1    // set to the mean of the true distribution

    // [DTC] (eq. 3)
    def generateVote(difficulty: Double): Boolean = random < accuracy(difficulty)

    // [DTC] (above eq. 3)
    def accuracy(difficulty: Double) = 0.5 * (1 + pow(1-difficulty, estGX))

    /* I'm not sure about my whole "bigger/smaller" logic it was a guess...
     *  plus there HAS to be a less unsightly way of accomplishing the same thing */
    // higher GX means Worse worker
    def updateGX(votes: List[Boolean]) {    // [DTC] (below eq. 12)
        val (trues, falses) = votes.partition(_ == true)
        val trueBigger = trues.length > falses.length
        val bigger  = if (trueBigger) trues.length  else falses.length
        val smaller = if (trueBigger) falses.length else trues.length
        val d = qstn.artifact_difficulty
        estGX -= bigger * d * LEARNING_RATE
        estGX += smaller * (1 - d) * LEARNING_RATE
    }
}

case class Question(trueAnswer: Boolean)
{
    var balance = INITIAL_ALLOWANCE
    var qlty = INITIAL_QUALITY
    var qltyPrime = 0.0

    // [DTC] trueGX > 0; code is worker_dist-agnostic
    var workerTrueGm = WORKER_DIST.sample
    while (workerTrueGm < 0) workerTrueGm = WORKER_DIST.sample

    val wrkrs = Workers(workerTrueGm)

    // [DTC] § Experimental Setup
    var f_Q_of_q = new QualityDistribution  // defaults to BetaDist(1,9)

    // my goal here is to initialize the qPrime prior a little higher than the q prior
    // a better way to do this would be nice
    var f_Q_of_qPrime = new QualityDistribution(
        new BetaDistribution(2,9).sample(NUM_PARTICLES)
    )

    def artifact_difficulty: Double = difficulty(qlty, qltyPrime)

    // [DTC] (eq. 2)
    def difficulty(qlty: Double, qltyPrime: Double): Double =
        1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)

    def artifact_utility: Double =
        convolute_Utility_with_Particles(f_Q_of_q) // + balance

    // the math for this checks out
    def convolute_Utility_with_Particles(dist: QualityDistribution): Double =
        (0.0 /: dist.particles)(_ + estimate_artifact_utility(_)) / NUM_PARTICLES

    // [DTC] (eq. 12)
    // this is O(numParticles^2)...they also note that this equation takes a while
    def dStar: Double = {
        val normQ = f_Q_of_q.particles.sum
        val normQP = f_Q_of_qPrime.particles.sum
        (0.0 /: f_Q_of_q.particles)((sum, q) =>
            sum + (0.0 /: f_Q_of_qPrime.particles)((sum2, qPrime) =>
                sum2 + q * qPrime * difficulty(q, qPrime) / normQ
            ) / normQP
        )
    }

    def submit_final() = {
        println("Final Utility: " + artifact_utility)
        sys.exit(0)
    }

    // I had written that this is what the paper says to do,
    // Thinking it over again, it makes sense now too
    // [DTC] (top-right of page 4)
    def utility_of_improvement_job: Double =
        utility_of_stopping_voting - IMPROVEMENT_COST * UTILITY_OF_$$$

    // [DTC] (eq. 9)
    def utility_of_stopping_voting: Double = { max(
        convolute_Utility_with_Particles(f_Q_of_q),  // [DTC] (eq. 10)
        convolute_Utility_with_Particles(f_Q_of_qPrime)  // [DTC] (eq. 11)
    )}

    /***************************** BALLOT JOB STUFF *******************************/
    // [DTC] (bottom-left Pg. 4)
    def utility_of_voting: Double = {
        val probYes = probability_of_yes_vote
        println("probYes: " + probYes)
        max(
            expVal_OLD_artifact_with_addnl_vote(probYes),
            expVal_NEW_artifact_with_addnl_vote(probYes)
        ) - BALLOT_COST * UTILITY_OF_$$$
    }

    // [DTC] (bottom-left Pg. 4)
    /* PSEUDOCODE for calculating P(b_{n+1}):
     * For each particle in the "normal" set
     *  "Convolute" the [entire] "predicted" set of particles with the accuracy according to whether
     *  the particle in the predicted set has a higher value than the one in the normal set (eq. 3)
     *   This convolution will yield a scalar
     * This [outer] summation will yield another scalar (our result, P(b_{n+1}))
     */
    def probability_of_yes_vote = {
        (0.0 /: f_Q_of_q.particles)((sumA, particleA) =>
            sumA + particleA * (0.0 /: f_Q_of_qPrime.particles)((sumB, particleB) =>

                // TODO check that multiplying particleB in makes sense
                sumB + prob_true_given_Qs(particleA, particleB) * particleB
            ) / f_Q_of_qPrime.particles.sum
        ) / f_Q_of_q.particles.sum
    }

    // I checked and this function works properly
    def prob_true_given_Qs(q: Double, qPrime: Double): Double =
        qstn.invertIfFalse(q < qPrime, wrkrs.accuracy(qstn.difficulty(q, qPrime)))

    /* [DTC] (bottom-left Pg. 4)
     * PSEUDOCODE for calculating E[ U( Q | b_{n} + 1 ) ]:
     * Then, For each particle in the result of performing (eq. 5)
     *   For each vote outcome \in { 0, 1 }
     *     Multiply U(q) * particle.q * P(b_{n+1} = {0,1})
     */
    def expVal_OLD_artifact_with_addnl_vote(probYes: Double): Double =
        expVal_after_a_vote(dist_Q_after_vote, probYes)

    // E[ U( Q' | b_{n} + 1 ) ]; the same thing as above
    def expVal_NEW_artifact_with_addnl_vote(probYes: Double) =
        expVal_after_a_vote(dist_QPrime_after_vote, probYes)

    def expVal_after_a_vote(f: Boolean => QualityDistribution, probYes: Double): Double = {
        if (probYes > 1 || probYes < 0) throw new RuntimeException
        convolute_Utility_with_Particles(f(true)) * probYes +
        convolute_Utility_with_Particles(f(false)) * (1 - probYes)
    }


    // [DTC] (eq. 5)
    /* What this Does:
     * Creates a posterior distribution (estimate) of the quality of the artifact
     *  given one more ballot
     */
    def dist_Q_after_vote(vote: Boolean): QualityDistribution =
        f_Q_of_q.weight_and_sample(vote, f_Q_of_qPrime)

    /* [DTC] (eq. 7-8) the same as above, but the order in
     *   which the distributions are used is switched
     */
    def dist_QPrime_after_vote(vote: Boolean): QualityDistribution =
        f_Q_of_qPrime.weight_and_sample(vote, f_Q_of_q)

//    // [DTC] (eq. 6)
//    // NOTE: I now think that this is supposed to be RESAMPLING the particles
//    def weight_and_sample(vote: Boolean, arrA: Array[Double], arrB: Array[Double]):
//    QualityDistribution = {
//        val normB = arrB.sum
//        QualityDistribution(NUM_PARTICLES,
//            arrA map { partA =>
//                partA * (0.0 /: arrB)((sum, partB) => // [DTC] (eq. 6)
//                {
//                    val probTrue = wrkrs.prob_true_given_Qs(partA, partB)
//                    sum + partB * invertIfFalse(vote, probTrue) / normB
//                }
//                )  // I deleted normA from here after looking back at (eq. 5)
//            }      // and the voteUtility went up about 30x; was that right?
//        )
//    }

    def get_addnl_ballot_and_update_dists(): Boolean = {
        balance -= BALLOT_COST  // pay for it
        val vote: Boolean = wrkrs.generateVote(artifact_difficulty)
        printf("vote :: %s #L293\n\n", vote.toString.map(_.toUpper))
        f_Q_of_q      = dist_Q_after_vote(vote)  // [DTC] (eqs. 4,5,6,7,8)
        f_Q_of_qPrime = dist_QPrime_after_vote(vote)
        votes ::= vote
        print("(")
        votes.foreach(printf("%b, ",_))
        println(")\n")
        vote
    }

    var votes = List[Boolean]()

    def invertIfFalse(t: Boolean, v: Double): Double = if (t) v else 1-v
    /******************************************************************************/

    def improvement_job() {
        // pay for it
        balance -= IMPROVEMENT_COST

        // choose to continue with alpha or alphaPrime [DTC top-right pg. 4]
        if (convolute_Utility_with_Particles(f_Q_of_q)
          < convolute_Utility_with_Particles(f_Q_of_qPrime))
            f_Q_of_q = f_Q_of_qPrime

        // use majority vote to update GX
        wrkrs.updateGX(votes)

        // clear votes out
        votes = List[Boolean]()

        // create new prior for quality of improved artifact
        f_Q_of_qPrime = f_Q_of_q.predict
    }

    def choose_action() {
        val artifactUtility    = artifact_utility
        val voteUtility        = utility_of_voting
        val improvementUtility = utility_of_improvement_job

        println("current balance: " + balance)
        print("(")
        qstn.f_Q_of_q.particles.foreach(printf("%.2f, ",_))
        println(qstn.f_Q_of_q.particles.sum/NUM_PARTICLES + ")\n")

        println("artifactUtility: "    + artifactUtility)
        println("voteUtility: "        + voteUtility)
        println("improvementUtility: " + improvementUtility)
        println("meanQltyEst: "        + qstn.f_Q_of_q.meanQltyEst)
        println("PrimeMeanQltyEst: "   + qstn.f_Q_of_qPrime.meanQltyEst)

        if (improvementUtility > voteUtility
        && improvementUtility > artifactUtility
        && balance > IMPROVEMENT_COST)
        {
            println("improvement job\n")
            improvement_job()
        }
        else if (voteUtility > artifactUtility
                && balance > BALLOT_COST)
        {
            println("ballot job\n")
            get_addnl_ballot_and_update_dists()
        }
        else submit_final()
    }
}

object FirstExperiment
{
    /* [DTC] gmX "follow a bell shaped distribution"
     *           "average error coefficient gm=1",
     *             where gmX > 0 */
    val WORKER_DIST = new NormalDistribution(1,0.2)

    /* this is a method so that it can be set to generate a random
     * initial quality every time a question starts */
    def INITIAL_QUALITY = .01 // new BetaDistribution(1,9).sample

    // [DTC] § Experimental Setup
    def estimate_artifact_utility(qlty: Double): Double =
        1000 * (exp(qlty) - 1) / (exp(1) - 1)

    val IMPROVEMENT_COST    = .05
    val BALLOT_COST         = .01
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 3  // not using this at this point
    val NUM_QUESTIONS       = 10000
    val INITIAL_ALLOWANCE   = 10.0
    val NUM_PARTICLES       = 100
    val UTILITY_OF_$$$      = .05  // let's just say it's "1" for simplicity

    /* so that MANY Questions can be run Per Experiment
     * I'ma try to get just one Question working first though */
    val qstn = Question(trueAnswer=true)
}

object TestStuff extends App { while(true) FirstExperiment.qstn.choose_action() }

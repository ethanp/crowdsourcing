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


// add "normalize" to Array[Dbl] to make ||Array|| = 1
abstract class addNorm(a: Array[Double]) { def normalize: Array[Double] }
object apples {
    implicit def addNorm(a: Array[Double]): addNorm = new addNorm(a) {
        def normalize = {
            val norm = a.sum
            a map {_/norm}
        }
    }
}
import apples._

/* Particle Filter representation of artifact-quality Probability Density Functions */
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

    /** [DTC] (eq. 1), q => generate f_{ Q' | particle.q } (q') and sample from it
     * IN WORDS:
     * We go through each particle, and using its value (which is an estimate of the qlty),
     * we generate an "improvement distribution" describing how much we can expect [an avg.]
     * worker to improve the quality of the artifact. We take a "random" stab at the new
     * quality after the "improvement job" by sampling from the "improvement distribution".
     */
    def predict: QualityDistribution = new QualityDistribution(
        particles map { q =>  // [DTC] § Experimental Setup
            val mu = find_improvementFunctionMean(q)
            new BetaDistribution(10 * mu, 10 * (1 - mu)).sample
    })

    /* fold ALL the partBs through prob_true() with EACH partA :  [DTC] (eq. 5-6) */
    def weight_and_sample(vote: Boolean, that: QualityDistribution, prime: Boolean):
    QualityDistribution = {
        // get P( b_{n+1} | q ) :  [DTC] (eq. 6) ; [TK=>PF] (eq. 8-9)
        val weights = {
            this.particles map { partA =>
                (0.0 /: that.particles)((sum, partB) => {
                    val inTup = if (prime) (partB, partA) else (partA, partB)
                    sum + qstn.invertIfFalse(vote, qstn.prob_true_given_Qs(inTup._1, inTup._2))
                })
            }}.normalize

        // get f_{ Q | b_{n+1} } (q) :  [DTC] (eq. 5)
        new QualityDistribution( particles map { _ => repopulate(weights) } )
    }

    /* algorithm for sampling from given set of points with associated weights:
     * generate a random number in [0,1), use the CDF of the weights to use the
     * random number to figure out what the sampled value is
     */
    def repopulate(weights: Array[Double]): Double = {
        val rand = Random.nextDouble
        var accrue = 0.0
        for ((weight, index) <- weights.zipWithIndex) {
            accrue += weight
            if (rand < accrue)
                return particles(index)
        }
        throw new RuntimeException // shouldn't ever get here
        particles(particles.length-1)
    }
}

/* model all workers with just one worker-independent model */
case class Workers(trueGX: Double)
{
    val LEARNING_RATE = 0.05
    var estGX: Double = 1    // set to the mean of the true distribution
                             // could be altered to test robustness

    // [DTC] (above eq. 3)
    def accuracy(difficulty: Double) = 0.5 * (1 + pow(1 - difficulty, estGX))

    // higher GX means Worse worker
    def updateGX(votes: List[Boolean]) {    // [DTC] (below eq. 12)
        val (trues, falses) = votes.partition(_ == true)
        val trueBigger = trues.length > falses.length
        val bigger  = if (trueBigger) trues.length  else falses.length
        val smaller = if (trueBigger) falses.length else trues.length
        val d = qstn.dStar
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
        convolute_Utility_with_Particles(f_Q_of_q)

    // the math for this checks out
    def convolute_Utility_with_Particles(dist: QualityDistribution): Double =
        (0.0 /: dist.particles)(_ + estimate_artifact_utility(_)) / NUM_PARTICLES

    // [DTC] (eq. 12)
    // this is O(numParticles^2)...they also note that this equation takes a while
    def dStarOLD: Double = {
        val normQ = f_Q_of_q.particles.sum
        val normQP = f_Q_of_qPrime.particles.sum
        (0.0 /: f_Q_of_q.particles)((sum, q) =>
            sum + (0.0 /: f_Q_of_qPrime.particles)((sum2, qPrime) =>
                sum2 + q * qPrime * difficulty(q, qPrime) / normQ
            ) / normQP
        )
    }

    def dStar: Double = {
        (0.0 /: f_Q_of_q.particles)((sum, q) =>
            sum + (0.0 /: f_Q_of_qPrime.particles)((sum2, qPrime) =>
                sum2 + difficulty(q, qltyPrime)
            )
        ) / (NUM_PARTICLES * NUM_PARTICLES)
    }


    def submit_final() = {
        println("Final Utility: " + artifact_utility)
        sys exit 0
    }

    // [DTC] (top-right of page 4)
    def utility_of_improvement_job: Double =
        utility_of_stopping_voting - IMPROVEMENT_COST * UTILITY_OF_$$$

    // [DTC] (eq. 9)
    def utility_of_stopping_voting: Double = { max(
        convolute_Utility_with_Particles(f_Q_of_q.predict),  // [DTC] (eq. 10)
        convolute_Utility_with_Particles(f_Q_of_qPrime.predict)  // [DTC] (eq. 11)
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
                sumB + prob_true_given_Qs(particleA, particleB)
            ) / NUM_PARTICLES
        ) / NUM_PARTICLES
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
    def dist_Q_after_vote(vote: Boolean): QualityDistribution =
        f_Q_of_q.weight_and_sample(vote, f_Q_of_qPrime, false)

    /* [DTC] (eq. 7-8) the same as above, but the order in
     *   which the distributions are used is switched
     */
    def dist_QPrime_after_vote(vote: Boolean): QualityDistribution =
        f_Q_of_qPrime.weight_and_sample(vote, f_Q_of_q, true)

    def get_addnl_ballot_and_update_dists(): Boolean = {
        balance -= BALLOT_COST  // pay for it
        val vote = random < probability_of_yes_vote // heh heh.
        printf("vote :: %s #L293\n\n", vote.toString.toUpperCase)
        f_Q_of_q      = dist_Q_after_vote(vote)  // [DTC] (eqs. 4,5,6,7,8)
        f_Q_of_qPrime = dist_QPrime_after_vote(vote)
        votes ::= vote
        print("(" + votes.mkString(", ") + ")\n")
        vote
    }

    var votes = List[Boolean]()

    def invertIfFalse(t: Boolean, v: Double): Double = if (t) v else 1-v
    /****************************** END BALLOT JOB STUFF **************************/

    def improvement_job() {
        // pay for it
        balance -= IMPROVEMENT_COST

        // choose to continue with alpha or alphaPrime [DTC top-right pg. 4]
        if (convolute_Utility_with_Particles(f_Q_of_q)
          < convolute_Utility_with_Particles(f_Q_of_qPrime))
            f_Q_of_q = f_Q_of_qPrime

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

//        println("\nParticles:\n(" + qstn.f_Q_of_q.particles.mkString(", ") + ")\n")
//        println("current balance     " + balance)
//        println("artifactUtility:    " + artifactUtility)
//        println("voteUtility:        " + voteUtility)
//        println("improvementUtility: " + improvementUtility)
        println("Original Utility:   " + convolute_Utility_with_Particles(qstn.f_Q_of_q))
        println("Prime Utility:      " + convolute_Utility_with_Particles(qstn.f_Q_of_qPrime))

        if (improvementUtility > voteUtility
        && improvementUtility > artifactUtility
        && balance > IMPROVEMENT_COST)
        {
            println("\n=> | IMPROVEMENT job |")
            println("\n\n****************************************************")
            improvement_job()
        }
        else if (voteUtility > artifactUtility
                && balance > BALLOT_COST)
        {
            println("\n=> | BALLOT job |")
            get_addnl_ballot_and_update_dists()
            println("\n\n****************************************************")
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

    val IMPROVEMENT_COST    = 3
    val BALLOT_COST         = .75
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 3  /* TODO: THE LookAhead */
    val NUM_QUESTIONS       = 10000
    val INITIAL_ALLOWANCE   = 10.0
    val NUM_PARTICLES       = 1000
    val UTILITY_OF_$$$      = 1  // let's just say it's "1" for simplicity

    /* so that MANY Questions can be run Per Experiment
     * I'ma try to get just one Question working first though */
    val qstn = Question(trueAnswer=true)
}

object TestStuff extends App { while(true) FirstExperiment.qstn.choose_action() }

/* Code from commit called  "Cleaned ballot fix. better example"

probYes: 0.5642297185063473
Original Utility:   66.28891896336619
Prime Utility:      123.18249548850702

=> | IMPROVEMENT job |


****************************************************
probYes: 0.6444673369717048
Original Utility:   123.18249548850702
Prime Utility:      242.2767724466584

=> | BALLOT job |
vote :: TRUE #L293

(true)


****************************************************
probYes: 0.6855813830386255
Original Utility:   109.97694551854808
Prime Utility:      261.92207315336105

=> | BALLOT job |
vote :: TRUE #L293

(true, true)


****************************************************
probYes: 0.7174521771497379
Original Utility:   101.52560532255704
Prime Utility:      284.924979974636

=> | IMPROVEMENT job |


****************************************************
probYes: 0.5672242350914865
Original Utility:   284.924979974636
Prime Utility:      344.810406913463

=> | BALLOT job |
vote :: FALSE #L293

(false)


****************************************************
probYes: 0.46976228685705484
Original Utility:   326.95478970361347
Prime Utility:      294.4953065977107
Final Utility: 326.95478970361347

Process finished with exit code 0
*/

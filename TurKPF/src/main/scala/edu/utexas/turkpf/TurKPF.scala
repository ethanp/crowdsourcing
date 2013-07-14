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

/* this means one can choose a set of parameters by replacing this line with
 *  import SecondExperiment._  and so on  */
import FirstExperiment._

/* TODO: the LookAhead */

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
    def predict: QualityDistribution =
        new QualityDistribution(particles map {improvementDistr(_).sample})


    // TODO
    def re_estimate { ??? }

    // TODO
    def sample { ??? }

    // avg loc of particles in associated Particle Filter
    def meanQltyEst: Double = particles.sum / NUM_PARTICLES
}

/* I am modelling all workers with just one worker-independent model */
case class Workers(trueGX: Double)
{
    val learningRate = 0.05
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
        estGX -= bigger * d * learningRate
        estGX += smaller * (1 - d) * learningRate
    }

    // I checked and this function works properly
    def GENERAL_prob_true_given_Qs(q: Double, qPrime: Double): Double = {
        qstn.invertIfFalse(q < qPrime, accuracy(qstn.difficulty(q, qPrime)))
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
    def difficulty(qlty: Double, qltyPrime: Double): Double = {
        1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)
    }

    // TODO: decide which of these to use
    def artifact_utility: Double = {
        convolute_Utility_with_Particles(f_Q_of_q) + balance * UTILITY_OF_$$$
//        estimate_artifact_utility(f_Q_of_q.meanQltyEst) + balance * UTILITY_OF_$$$
    }

    def convolute_Utility_with_Particles(dist: QualityDistribution): Double = {
        dist.particles.foldLeft(0.0)((sum, particle) =>
                sum + estimate_artifact_utility(particle) / NUM_PARTICLES
        )
    }

    // [DTC] (eq. 12)
    // this is O(numParticles^2)...they also note that this equation takes a while
    def dStar: Double = {
        f_Q_of_q.particles.foldLeft(0.0)((sum, q) =>
            sum + f_Q_of_qPrime.particles.foldLeft(0.0)((sum2, qPrime) =>
                sum2 + q * qPrime * difficulty(q, qPrime) / NUM_PARTICLES
            ) / NUM_PARTICLES
        )
    }

    def submit_final() = {
        println("Final Utility: " + artifact_utility)
        sys.exit(0)
    }

    // I had written that this is what the paper says to do,
    // Thinking it over again, it makes sense now too
    // [DTC] (top-right of page 4)
    def utility_of_improvement_job: Double = {
        utility_of_stopping_voting - IMPROVEMENT_COST * UTILITY_OF_$$$
    }

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
        f_Q_of_q.particles.foldLeft(0.0)((sumA, particleA) =>
            sumA + particleA * f_Q_of_qPrime.particles.foldLeft(0.0)((sumB, particleB) =>
                sumB + wrkrs.GENERAL_prob_true_given_Qs(particleA, particleB) * particleB
            ) / NUM_PARTICLES
        ) / NUM_PARTICLES
    }

    /* [DTC] (bottom-left Pg. 4)
     * PSEUDOCODE for calculating E[ U( Q | b_{n} + 1 ) ]:
     * Then, For each particle in the result of performing (eq. 5)
     *   For each vote outcome \in { 0, 1 }
     *     Multiply U(q) * particle.q * P(b_{n+1} = {0,1})
     */
    def expVal_OLD_artifact_with_addnl_vote(probYes: Double): Double = {
        expVal_after_a_vote(dist_Q_after_vote, probYes)
    }

    // E[ U( Q' | b_{n} + 1 ) ]; the same thing as above
    def expVal_NEW_artifact_with_addnl_vote(probYes: Double) = {
        expVal_after_a_vote(dist_QPrime_after_vote, probYes)
    }

    def expVal_after_a_vote(f: Boolean => QualityDistribution, probYes: Double): Double = {
        expVal_given_dist(f(true), probYes) + expVal_given_dist(f(false), probYes)
    }

    def expVal_given_dist(d: QualityDistribution, probYes: Double): Double = {
        d.particles.foldLeft(0.0)((sum, particle) =>
            sum + particle * probYes * estimate_artifact_utility(particle)
        ) / NUM_PARTICLES
    }

    // [DTC] (eq. 5)
    /* What this Does:
     * Creates a posterior distribution (estimate) of the quality of the artifact
     *  given one more ballot
     * I now think this same function can be easily used for both f_Q and f_Q'
     *  (just pass in the two particle-sets as parameters, and switch the order)
     *
     * PSEUDOCODE:
     * Create a new Particle_Filter-based distribution from the old one
     * For each particle in f_{Q|bn},
     *   multiply it against the convolution of f_{Q'|bn} with P(b|q,q')
     * to obtain f_{Q|(bn + 1)}
     */
    def dist_Q_after_vote(vote: Boolean): QualityDistribution = {
        dist_after_vote_helper(vote, f_Q_of_q, f_Q_of_qPrime)
    }

    /* [DTC] (eq. 7-8) the same as above, but the order in
     *   which the distributions are used is switched
     */
    def dist_QPrime_after_vote(vote: Boolean): QualityDistribution = {
        dist_after_vote_helper(vote, f_Q_of_qPrime, f_Q_of_q)
    }

    def dist_after_vote_helper(vote: Boolean, a: QualityDistribution, b: QualityDistribution):
    QualityDistribution = {
        QualityDistribution(NUM_PARTICLES,
            a.particles.map { partA =>
                partA * b.particles.foldLeft(0.0)((sum, partB) => // [DTC] (eq. 6)
                {
                    val probTrue = wrkrs.GENERAL_prob_true_given_Qs(partA, partB)
                    sum + partB * invertIfFalse(vote, probTrue) / NUM_PARTICLES
                }
                ) / NUM_PARTICLES
            }
        )
    }

    def get_addnl_ballot_and_update_dists(): Boolean = {
        balance -= BALLOT_COST  // pay for it
        val vote: Boolean = wrkrs.generateVote(artifact_difficulty)
        f_Q_of_q      = dist_Q_after_vote(vote)  // [DTC] (eqs. 4,5,6,7,8)
        f_Q_of_qPrime = dist_QPrime_after_vote(vote)
        votes ::= vote
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
        val artifactUtility = artifact_utility
        val voteUtility = utility_of_voting
        val improvementUtility = utility_of_improvement_job

        println("artifactUtility: " + artifactUtility)
        println("voteUtility: " + voteUtility)
        println("improvementUtility: " + improvementUtility)
        println("meanQltyEst: " + qstn.f_Q_of_q.meanQltyEst)
        println("PrimeMeanQltyEst: " + qstn.f_Q_of_qPrime.meanQltyEst)

        if (artifactUtility > voteUtility
          && artifactUtility > improvementUtility) {
            submit_final()
        }
        else if (voteUtility > improvementUtility) {
            println("ballot job\n")
            get_addnl_ballot_and_update_dists()
        }
        else {
            println("improvement job\n")
            improvement_job()
        }
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
    def estimate_artifact_utility(qlty: Double): Double = {
        1000 * (exp(qlty) - 1) / (exp(1) - 1)
    }

    val IMPROVEMENT_COST    = .05
    val BALLOT_COST         = .01
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 3  // not using this at this point
    val NUM_QUESTIONS       = 10000
    val INITIAL_ALLOWANCE   = 400.0
    val NUM_PARTICLES       = 100
    val UTILITY_OF_$$$      = .005

    /* so that MANY Questions can be run Per Experiment
     * I'ma try to get just one Question working first though */
    val qstn = Question(trueAnswer=true)
}

object TestStuff extends App {
    while(true) FirstExperiment.qstn.choose_action()
}

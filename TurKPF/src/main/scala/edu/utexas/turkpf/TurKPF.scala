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

/* TODO: abstract out all the convolutions so they become more legible */

/* Particle Filter representation of
 * artifact quality probability density functions
 *      f_{Q}(q)
 *      f_{Q'}(q')
 *      f_{Q|bn}(q)
 *      f_{Q'|bn}(q)
 */
case class QualityDistribution(numParticles: Int,
                               particles: Array[Double])
{
    def this(numParticles: Int) = this(numParticles, new BetaDistribution(1,9).sample(numParticles))

    def this(particles: Array[Double]) = this(NUM_PARTICLES, particles)

    def this() = this(NUM_PARTICLES, new BetaDistribution(1,9).sample(numParticles))

    // [DTC] (eq. 13)
    def find_improvementFunctionMean(qlty: Double): Double = {
        val accuracy: Double = qstn.wrkrs.accuracy(qstn.artifact_difficulty)
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

    // [DTC] (eqs. 4,5,6,7,8)
    def observe(vote: Boolean) {
        qstn.f_Q_of_q      = qstn.dist_Q_after_vote(vote)
        qstn.f_Q_of_qPrime = qstn.dist_QPrime_after_vote(vote)
        qstn.votes ::= vote
    }

    // TODO
    def re_estimate { ??? }

    // TODO
    def sample { ??? }

    // avg loc of particles in associated Particle Filter
    def meanQltyEst: Double = particles.sum / NUM_PARTICLES
}

/* I am modelling all workers with just one instance
 *  instead of having one worker-independent model
 *  and multiple worker-dependent models
 * in order to simply my life a bit
 */
case class Workers(trueGX: Double)
{
    val learningRate = 0.05
    var estGX: Double = 1    // set to the mean of the true distribution

    // [DTC] (eq. 3)
    def generateVote(difficulty: Double): Boolean = random < accuracy(difficulty)

    // [DTC] (above eq. 3)
    def accuracy(difficulty: Double) = 0.5 * (1 + pow(1-difficulty, estGX))

    // higher GX means Worse worker
    def updateGX(votes: List[Boolean]) {    // [DTC] (below eq. 12)
    val (correct, incorrect) = votes.partition(_ == qstn.trueAnswer)
        estGX -= correct.length * qstn.artifact_difficulty * learningRate
        estGX += incorrect.length * (1 - qstn.artifact_difficulty) * learningRate
    }

    def THE_prob_true_given_Qs: Double = {
        GENERAL_prob_true_given_Qs(
            qstn.f_Q_of_q.meanQltyEst,
            qstn.f_Q_of_qPrime.meanQltyEst
        )
    }

    def GENERAL_prob_true_given_Qs(q: Double, qP: Double): Double = {
        if (q < qP)
            accuracy(qstn.difficulty(q, qP))
        else
            1 - accuracy(qstn.difficulty(q, qP))
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

    var f_Q_of_qPrime = new QualityDistribution


    def artifact_difficulty: Double = difficulty(qlty, qltyPrime)

    // [DTC] (eq. 2)
    def difficulty(qlty: Double, qltyPrime: Double): Double = {
        1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)
    }

    // was including $$ correct?
    /* TODO this is incorrect, bc qlty represents the TRUE quality, not the est'd quality,
     * which is represented by a probability distribution.
     *  So here, I /should/ be passing in the mean of that distribution or something.
     */
    def artifact_utility: Double = estimate_artifact_utility(qlty) + balance * UTILITY_OF_$$$

    def convolute_Utility_with_Particles(dist: QualityDistribution): Double = {
        dist.particles.foldLeft(0.0)(
            (sum, particle) =>
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

    def choose_action() {
        if (artifact_utility > utility_of_voting
          && artifact_utility > utility_of_improvement_job)
            submit_final()

        else if (utility_of_voting > utility_of_improvement_job)
            get_addnl_ballot()

        else
            improvement_job()
    }

    // note this is a copy-paste of utility_of_stopping_voting (besides the $$ part),
    // that IS what the paper says to do though.
    // [DTC] (top-right of page 4)
    def utility_of_improvement_job: Double = {
        max(
            convolute_Utility_with_Particles(qstn.f_Q_of_q),
            convolute_Utility_with_Particles(qstn.f_Q_of_qPrime)
        ) - IMPROVEMENT_COST * UTILITY_OF_$$$
    }

    /******* Was class BallotJob, moved it in here bc that's not a separate entity *****/

    /* TODO doesn't incorporate the observation value of having obtained the ballots */
    // [DTC] (eq. 9)
    // TODO this eq. is also used to decide which of the artifacts to keep [DTC top-right pg. 4]
    def utility_of_stopping_voting: Double = { max(
        convolute_Utility_with_Particles(f_Q_of_q),  // [DTC] (eq. 10)

        convolute_Utility_with_Particles(f_Q_of_qPrime)  // [DTC] (eq. 11)
    )}

    // [DTC] (eq. 5)
    // TODO: Don't use f_Q_of_q, use f_Q_of_q_given_Bn
    /* What this Does:
     * Creates a posterior distribution (estimate) of the quality of the artifact
     *  given one more ballot
     * I now think this same function can be easily used for both f_Q and f_Q'
     *  (just pass in the two particle-sets as parameters, and switch the order)
     *
     * PSEUDOCODE:
     * Create a new Particle Filter-based distribution from the old one
     * For each particle in f_Q|bn,
     *   multiply it against the convolution of f_Q'|bn with P(b|q,q')
     * to obtain f_Q|(bn + 1)
     */
    def dist_Q_after_vote(vote: Boolean): QualityDistribution = {
        val predictedParticles = f_Q_of_qPrime.particles
        QualityDistribution(NUM_PARTICLES,
            f_Q_of_q.particles.map { particle =>
                particle * predictedParticles.foldLeft(0.0)((sum, particlePrime) => // [DTC] (eq. 6)
                    {
                        val probTrue: Double = wrkrs.GENERAL_prob_true_given_Qs(particle, particlePrime)
                        sum + particlePrime * invertIfFalse(vote, probTrue) / NUM_PARTICLES
                    }
                ) / NUM_PARTICLES
            }
        )
    }

    /* [DTC] (eq. 7-8) basically the same as above, but the order in
     *   which the distributions are used is switched
     */
    def dist_QPrime_after_vote(vote: Boolean): QualityDistribution = {
        QualityDistribution(NUM_PARTICLES,
            f_Q_of_qPrime.particles.map {particlePrime =>
                particlePrime * f_Q_of_q.particles.foldLeft(0.0)((sum, particle) =>
                    {
                        val probTrue: Double = wrkrs.GENERAL_prob_true_given_Qs(particle, particlePrime)
                        sum + particle * invertIfFalse(vote, probTrue) / NUM_PARTICLES
                    }
                ) / NUM_PARTICLES
            }
        )
    }

    def invertIfFalse(vote: Boolean, value: Double): Double = if (vote) value else 1-value

    // [DTC] (bottom-left Pg. 4)
    // this set of equations is the most intimidating set in this thing
    /* PSEUDOCODE for calculating P(b_{n+1}):
     * For each particle in the "normal" set
     *  "Convolute" the [entire] "predicted" set of particles with the accuracy according to whether
     *  the particle in the predicted set has a higher value than the one in the normal set (eq. 3)
     *   This convolution will yield a scalar
     * This [outer] summation will yield another scalar (our result, P(b_{n+1}))
     */
    def probability_of_yes_vote = {
        f_Q_of_q.particles.foldLeft(0.0)((sum, particle) =>
            sum + particle * f_Q_of_qPrime.particles.foldLeft(0.0)((sum2, primeParticle) =>
                sum2 + wrkrs.GENERAL_prob_true_given_Qs(particle, primeParticle)
                  * primeParticle / NUM_PARTICLES
            ) / NUM_PARTICLES
        )
    }

    /* PSEUDOCODE for calculating E[ U( Q | b_{n} + 1 ) ]:
     * First, I need to use (eq. 5) (as-yet unimplemented) to generate f_{ Q | b_{n} + 1 } (q)
     * Then, For each particle in the result of performing (eq. 5)
     *   For each vote outcome \in { 0, 1 }
     *     Multiply U(q) * particle.q * P(b_{n+1} = {0,1})
     */
    // TODO
    def expVal_OLD_artifact_with_addnl_vote = ???

    // E[ U( Q' | b_{n} + 1 ) ] basically the same thing as above
    // TODO
    def expVal_NEW_artifact_with_addnl_vote = ???

    // [DTC] (bottom-left Pg. 4)
    def utility_of_voting: Double = {
        max(
            expVal_OLD_artifact_with_addnl_vote,
            expVal_NEW_artifact_with_addnl_vote
        ) - BALLOT_COST * UTILITY_OF_$$$
    }

    def get_addnl_ballot(): Boolean = {
        balance -= BALLOT_COST  // pay for it
        val vote: Boolean = wrkrs.generateVote(artifact_difficulty)
        votes ::= vote
        vote
    }

    var votes = List[Boolean]()

    def re_estimate_worker_accuracy() { wrkrs.updateGX(votes) }

    def improvement_job() {
        // pay for it
        balance -= IMPROVEMENT_COST

        // choose to continue with \alpha or \alphaPrime
        if (dist_Q_after_vote.meanQltyEst <
          dist_QPrime_after_vote.meanQltyEst)
            f_Q_of_q = f_Q_of_qPrime

        // use majority vote to update GX
        re_estimate_worker_accuracy()

        // clear votes out
        votes = List[Boolean]()

        // create prediction for quality of improved artifact
        f_Q_of_qPrime = f_Q_of_q.predict
    }

    /******* UNIMPLEMENTED **********/
    // TODO
    def update_posteriors_for_alphas() = ???
}

object FirstExperiment
{
    /* [DTC] gmX "follow a bell shaped distribution" "average error coefficient gm=1",
     *      although note that gmX > 0 */
    val WORKER_DIST = new NormalDistribution(1,0.2)

    /* this is a method so that it generates a new initial quality every time a question starts */
    def INITIAL_QUALITY = new BetaDistribution(1,9).sample

    // [DTC] § Experimental Setup
    def estimate_artifact_utility(qlty: Double): Double = 1000 * (exp(qlty) - 1) / (exp(1) - 1)

    val IMPROVEMENT_COST    = .05
    val BALLOT_COST         = .01
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 3
    val NUM_QUESTIONS       = 10000
    val INITIAL_ALLOWANCE   = 400.0
    val NUM_PARTICLES       = 10000

    // of course it's situation-dependent, but put a Good # here
    val UTILITY_OF_$$$      = 5.0

    /* so that MANY Questions can be run Per Experiment
     * I'ma start with just one question though, and try to get that working first
     */
    val qstn = Question(trueAnswer=true)
}

object TestStuff extends App {
    // while(true) FirstExperiment.qstn.choose_action()
}

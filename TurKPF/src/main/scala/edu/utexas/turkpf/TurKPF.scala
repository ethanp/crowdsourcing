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
import org.apache.commons.math3.distribution.{RealDistribution, BetaDistribution, NormalDistribution}

/* this means I can choose all new parameters by replacing this line with
 *  import SecondExperiment._  and so on  */
import FirstExperiment._

/* notes:
 *  Type Info: cmd-T
 */

case class BallotJob()
{
    val ballotCost = .01

    /* TODO doesn't incorporate the observation value of having obtained the ballots */
    // [DTC] (eq. 9)
    def utility_of_stopping_voting: Double = {max(
        qstn.convolute_Utility_with_Particles(qstn.f_Q_of_q),  // [DTC] (eq. 10)

        // TODO this is a bit of a placeholder, the "PREDICT" /SHOULD/ have been done already by this point
        qstn.convolute_Utility_with_Particles(qstn.f_Q_of_q.predict)  // [DTC] (eq. 11)
    )}

    def utility_of_voting: Double = ???

    def need_another_vote: Boolean = utility_of_voting < utility_of_stopping_voting

    def get_addnl_ballot(): Boolean = {
        qstn.balance -= ballotCost  // pay for it
        qstn.WORKERS.generateVote(qstn.artifact_difficulty)
    }

    var votes = List[Boolean]()

    while (need_another_vote) {
        votes ::= get_addnl_ballot()
    }
    qstn.WORKERS.updateGX(votes)
}

/* prior quality estimate distribution f_Q (q) */
case class QualityDistribution(numParticles: Int,
                               dist: RealDistribution,
                               particles: Array[Double])
{
    def this(numParticles: Int, dist: RealDistribution) =
        this(numParticles, dist, dist.sample(numParticles))

    def find_improvementFunctionMean(qlty: Double): Double = { // [DTC] (eq. 13)
        val accuracy: Double = qstn.WORKERS.accuracy(qstn.artifact_difficulty)
        qlty + 0.5 * ((1 - qlty) * (accuracy - 0.5) + qlty * (accuracy - 1))
    }

    def improvementDistr(qlty: Double) = {  // [DTC] § Experimental Setup
        val mu = find_improvementFunctionMean(qlty)
        new BetaDistribution(10 * mu, 10 * (1 - mu))
    }

    // [DTC] (eq. 1), q => generate f_{ Q' | particle.q } (q') and sample from it
    def predict: QualityDistribution =
        QualityDistribution(numParticles, dist, particles map {improvementDistr(_).sample})

    // [DTC] (eqs. 4-6)
    def observe(vote: Boolean): QualityDistribution = { ??? }

    def re_estimate { ??? }

    def sample { ??? }

    // avg loc of particles in associated Particle Filter
    def meanQltyEst: Double = particles.sum / NUM_PARTICLES

    // TODO don't actually run 'predict' here, just 'get' the prediction from somewhere
    def meanQltyEstPrime: Double = predict.particles.sum / NUM_PARTICLES
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

    def prob_true_given_Qs: Double = {
        if (qstn.f_Q_of_q.meanQltyEst < qstn.f_Q_of_q.meanQltyEstPrime)
            accuracy(qstn.artifact_difficulty)
        else
            1 - accuracy(qstn.artifact_difficulty)
    }
}

case class Question(trueAnswer: Boolean)
{
    var balance = INITIAL_ALLOWANCE
    var qlty = INITIAL_QUALITY
    var qltyPrime = 0.0

    // [DTC] trueGX > 0; code is dist-agnostic
    var workerTrueGm = WORKER_DIST.sample
    while (workerTrueGm < 0) workerTrueGm = WORKER_DIST.sample

    val WORKERS = Workers(workerTrueGm)

    // [DTC] § Experimental Setup
    val f_Q_of_q =
        new QualityDistribution(NUM_PARTICLES, new BetaDistribution(1, 9))


    def artifact_difficulty: Double = difficulty(qlty, qltyPrime)

    // [DTC] (eq. 2)
    def difficulty(qlty: Double, qltyPrime: Double): Double = {
        1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)
    }

    // was including $$ correct?
    def artifact_utility: Double = estimate_artifact_utility(qlty) + balance * UTILITY_OF_$$$

    def convolute_Utility_with_Particles(dist: QualityDistribution): Double = {
        dist.particles.foldLeft(0.0)(
            (sum, particle) =>
                sum + (estimate_artifact_utility(particle)/NUM_PARTICLES))
    }

    // [DTC] (eq. 12)
    // this is O(numParticles^2)...they also note that this equation takes a while
    def dStar: Double = {
        val f_qPrime = f_Q_of_q.predict
        f_Q_of_q.particles.foldLeft(0.0)((sum, q) =>
            sum + f_qPrime.particles.foldLeft(0.0)((sum2, qPrime) =>
                sum2 + q * qPrime * difficulty(q, qPrime) / (NUM_PARTICLES * NUM_PARTICLES)))
    }

    def submit_final() = {
        println("Final Utility: " + artifact_utility)
        sys.exit(0)
    }

    def choose_action() {
        if (artifact_utility > utility_of_ballot_job
         && artifact_utility > utility_of_improvement_job)
            submit_final()
        else if (utility_of_ballot_job > utility_of_improvement_job)
            BallotJob
        else
            improvement_job()
    }

    // note this is a copy-paste of utility_of_stopping_voting (besides the $$ part),
    // that IS what the paper says to do though.
    // [DTC] (top-right of page 4)
    def utility_of_improvement_job: Double = {
        max(
            convolute_Utility_with_Particles(qstn.f_Q_of_q),
            convolute_Utility_with_Particles(qstn.f_Q_of_q.predict)
        ) - IMPROVEMENT_COST * UTILITY_OF_$$$
    }

    /******* UNIMPLEMENTED **********/
    def estimate_prior_for_alphaPrime() = ???

    def update_posteriors_for_alphas() = ???

    def improvement_job(): QualityDistribution = { ??? }

    def utility_of_ballot_job: Double = ???

    def re_estimate_worker_accuracy(workerIndex: Int) { ??? }

    def update_belief_state() { ??? }
}

object FirstExperiment
{
    /* [DTC] gmX "follow a bell shaped distribution" "average error coefficient gm=1",
     *      although note that gmX > 0 */
    val WORKER_DIST = new NormalDistribution(1,1)

    /* this is a method so that it generates a new initial quality every time a question starts */
    def INITIAL_QUALITY = new BetaDistribution(1,9).sample

    // [DTC] § Experimental Setup
    def estimate_artifact_utility(qlty: Double): Double = 1000 * (exp(qlty) - 1) / (exp(1) - 1)

    val IMPROVEMENT_COST    = .05
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 3
    val NUM_QUESTIONS       = 10000
    val INITIAL_ALLOWANCE   = 400.0
    val NUM_PARTICLES       = 10000
    val UTILITY_OF_$$$      = 5.0   // of course it's situation-dependent, but put a Good # here

    /*
     * I suppose one reason to make question a class, and not just bring it all
     * into this Experiment class, is so that MANY Questions can be run Per Experiment
     *  I'ma start with just one question though, and try and get that working first
     */
    val qstn = Question(trueAnswer=true)
}

object TestStuff extends App {}

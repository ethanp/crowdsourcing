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

case class BallotJob(qstn: Question)
{
    val ballotCost = .01

    /* I don't think this is right because the "priorQualityDensityFctn"
     * TODO doesn't incorporate the observation value of having obtained the ballots */
    def utility_of_stopping_voting: Double = {  // [DTC] (eq. 9)
        max(
            qstn.priorQualityDensityFctn.priorDistr  // [DTC] (eq. 10)
              .foldLeft(0.0)((sum,particle) => sum + (qstn.estimated_artifact_utility(particle)/NUM_PARTICLES)),

            // TODO this is a bit of a placeholder, the "PREDICT" /SHOULD/ have been done already by this point
            qstn.priorQualityDensityFctn.predict.priorDistr  // [DTC] (eq. 11)
              .foldLeft(0.0)((sum,particle) => sum + (qstn.estimated_artifact_utility(particle)/NUM_PARTICLES))
        )
    }

    def utility_of_voting: Double = ???

    def need_another_vote: Boolean = utility_of_voting < utility_of_stopping_voting

    def get_addnl_ballot(): Boolean = {
        qstn.balance -= ballotCost  // pay for it
        qstn.WORKERS.generateVote(qstn.difficulty)
    }

    var votes = List[Boolean]()

    while (need_another_vote) {
        votes ::= get_addnl_ballot()
    }
    qstn.WORKERS.updateGX(votes)
}

abstract class ParticleFilter(numParticles: Int, dist: RealDistribution, priorDistribution: Array[Double])
{
    def predict: ParticleFilter
    def observe
    def re_estimate
    def sample
}

/* prior quality estimate distribution f_Q (q) */
case class QualityDistribution(numParticles: Int,
                               dist: RealDistribution,
                               priorDistr: Array[Double],
                               qstn: Question)

    extends ParticleFilter(numParticles, dist, priorDistr)
{
    def this(numParticles: Int, dist: RealDistribution, qstn: Question) =
        this(numParticles, dist, dist.sample(numParticles), qstn)

    def find_improvementFunctionMean(qlty: Double): Double = { // [DTC] (eq. 13)
        val accuracy: Double = qstn.WORKERS.accuracy(qstn.difficulty)
        qlty + 0.5 * ((1 - qlty) * (accuracy - 0.5) + qlty * (accuracy - 1))
    }

    def improvementDistr(qlty: Double) = {  // [DTC] § Experimental Setup
        val mu = find_improvementFunctionMean(qlty)
        new BetaDistribution(10 * mu, 10 * (1 - mu))
    }

    // [DTC] (eq. 1), q => generate f_{ Q' | particle.q } (q') and sample from it
    def predict: QualityDistribution =
        QualityDistribution(numParticles, dist, priorDistr map {improvementDistr(_).sample}, qstn)

    def observe { ??? }

    def re_estimate { ??? }

    def sample { ??? }
}

/* I am modelling all workers with just one instance
 *  instead of having one worker-independent model
 *  and multiple worker-dependent models
 * in order to simply my life a bit
 */
case class Workers(trueGX: Double, qstn: Question)
{
    val learningRate = 0.05
    var estGX: Double = 1    // set to the mean of the true distribution

    def generateVote(difficulty: Double): Boolean = random < accuracy(difficulty) // [DTC] (eq. 3)

    def accuracy(difficulty: Double) = 0.5 * (1 + pow(1-difficulty, estGX)) // [DTC] (above eq. 3)

    def updateGX(votes: List[Boolean]) {    // [DTC] (below eq. 12)
        val (correct, incorrect) = votes.partition(_ == qstn.trueAnswer)
        estGX -= correct.length * qstn.difficulty * learningRate  // higher GX means Worse worker
        estGX += incorrect.length * (1 - qstn.difficulty) * learningRate
    }

    def prob_true_given_qS(): Double = {
        if (mean_f_Q_giv_q < mean_f_QP_giv_qp)
            accuracy(qstn.difficulty)
        else
            1 - accuracy(qstn.difficulty)
    }

    def mean_f_Q_giv_q(): Double = ???   // find avg loc of particles in associated Particle Filter
    def mean_f_QP_giv_qp(): Double = ??? // find avg loc of particles in associated Particle Filter
}

case class Question(trueAnswer: Boolean)
{
    var balance = INITIAL_ALLOWANCE
    var qlty = INITIAL_QUALITY
    var qltyPrime = 0.0
    var workerTrueGm = WORKER_DIST.sample
    while (workerTrueGm < 0) workerTrueGm = WORKER_DIST.sample  // [DTC] trueGX > 0; code is dist-agnostic
    val WORKERS = Workers(workerTrueGm, QUESTION)
    val priorQualityDensityFctn =
        new QualityDistribution(NUM_PARTICLES, new BetaDistribution(1, 9), this) // [DTC] § Experimental Setup


    def difficulty = 1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)      // [DTC] (eq. 2)

    def estimated_artifact_utility(qlty: Double): Double = 1000 * (exp(qlty) - 1) / (exp(1) - 1)    // [DTC] § Experimental Setup

    def dStar = ???  // [DTC] (eq. 12)

    def submit_final() = {
        println("Final Utility: " + estimated_artifact_utility(qlty))
        sys.exit(0)
    }

    def choose_action() {
        if (estimated_artifact_utility(qlty) > utility_of_ballot_job
         && estimated_artifact_utility(qlty) > utility_of_improvement_job)
            submit_final()
        else if (utility_of_ballot_job > utility_of_improvement_job)
            BallotJob
        else
            improvement_job()
    }

    def improvement_job(): QualityDistribution = priorQualityDensityFctn.predict


    /******* UNIMPLEMENTED **********/
    def estimate_prior_for_alphaPrime() = ???

    def update_posteriors_for_alphas() = ???

    def utility_of_improvement_job: Double = ???

    def utility_of_ballot_job: Double = ???

    def re_estimate_worker_accuracy(workerIndex: Int) { ??? }

    def update_belief_state() { ??? }
}

object FirstExperiment
{
    /* [DTC] gmX "follow a bell shaped distribution" "average error coefficient gm=1", although gmX > 0 */
    val WORKER_DIST = new NormalDistribution(1,1)

    /* this is a method so that it generates a new initial quality every time a question starts */
    def INITIAL_QUALITY = new BetaDistribution(1,9).sample

    val IMPROVEMENT_COST = .05
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH = 3
    val NUM_QUESTIONS = 10000
    val INITIAL_ALLOWANCE = 400.0
    val NUM_PARTICLES = 10000

    /*
     * I suppose one reason to make question a class, and not just bring it all
     * into this Experiment class, is so that MANY Questions can be run Per Experiment
     *  I'ma start with just one question though, and try and get that working first
     */
    val QUESTION = Question(trueAnswer=true)
}

object TestStuff extends App {}

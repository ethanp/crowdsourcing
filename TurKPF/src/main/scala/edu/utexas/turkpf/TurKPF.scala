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

/* notes:
 *  Type Info: cmd-T
 */

case class BallotJob(qstn: Question)
{
    val ballotCost = .01

    def utility_of_stopping_voting: Double = ???

    def utility_of_voting: Double = ???

    def need_another_vote: Boolean = utility_of_voting < utility_of_stopping_voting

    def get_addnl_ballot(): Boolean = {
        qstn.allowanceBalance -= ballotCost  // pay for it
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
                               priorDistribution: Array[Double],
                               qstn: Question)

    extends ParticleFilter(numParticles, dist, priorDistribution)
{
    def this(numParticles: Int, dist: RealDistribution, qstn: Question) =
        this(numParticles, dist, dist.sample(numParticles), qstn)

    def find_improvementFunctionMean(q: Double): Double = { // [DTC] (eq. 13)
        val accuracy: Double = qstn.WORKERS.accuracy(qstn.difficulty)
        q + 0.5 * ((1 - q) * (accuracy - 0.5) + q * (accuracy - 1))
    }

    def workerFctn(q: Double) = {  // [DTC] § Experimental Setup
        val mu = find_improvementFunctionMean(q)
        new BetaDistribution(10 * mu, 10 * (1 - mu))
    }

    // [DTC] (eq. 1), => generate f_{ Q' | particle.q } (q') and sample from it
    def predict: QualityDistribution =
        QualityDistribution(numParticles, dist, priorDistribution.map(workerFctn(_).sample), qstn)

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

    def generateVote(d: Double): Boolean = random < accuracy(d) // [DTC] (eq. 3)

    def accuracy(d: Double) = 0.5 * (1 + pow(1-d, estGX))

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
    /* this means I can choose all new parameters by replacing this line with
     *  import SecondExperiment._  and so on  */
    import FirstExperiment._

    var allowanceBalance = INITIAL_ALLOWANCE
    var q = INITIAL_QUALITY
    var qPrime = 0.0
    val WORKERS = Workers(WORKER_DIST.sample, QUESTION)
    val priorQualityDensityFctn =
        new QualityDistribution(100, new BetaDistribution(1, 9), this) // [DTC] § Experimental Setup


    def difficulty = 1 - pow((q - qPrime).abs, DIFFICULTY_CONSTANT)      // [DTC] (eq. 2)

    def estimated_artifact_utility = 1000 * (exp(q) - 1) / (exp(1) - 1)    // [DTC] § Experimental Setup

    def dStar = ???  // [DTC] (eq. 12)

    def submit_final() = {
        println("Final Utility: " + estimated_artifact_utility)
        sys.exit(0)
    }

    def choose_action() {
        if (estimated_artifact_utility > utility_of_ballot_job
         && estimated_artifact_utility > utility_of_improvement_job)
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
    /* [DTC] gmX "follow a bell shaped distribution" "average error coefficient gm=1" */
    val WORKER_DIST = new NormalDistribution(1,1)

    /* this is a method so that it generates a new initial quality every time a question starts */
    def INITIAL_QUALITY = new BetaDistribution(1,9).sample

    val IMPROVEMENT_COST = .05
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH = 3
    val NUM_QUESTIONS = 10000
    val INITIAL_ALLOWANCE = 400.0

    /* I suppose one reason to make question a class, and not just bring it all
     * into this Experiment class, is so that MANY Questions can be run Per Experiment
     *  I'ma start with just one question though, and try and get that working first
     */
    val QUESTION = Question(trueAnswer=true)
}

object TestStuff extends App {}

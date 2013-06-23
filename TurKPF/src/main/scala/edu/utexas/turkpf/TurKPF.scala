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
import scala.util.Random


/* notes:
 *  Type Info: cmd-T
 *
 *  I could potentially make a general implementation just using the Normal Distribution,
 *  and then turned it into a particle filter after getting that working?
 */

case class BallotJob(qstn: Question)
{
    val ballotCost = .01
    def utility_of_stopping_voting(): Double = ???
    def utility_of_voting(): Double = ???
    def decide_whether_to_vote(): Boolean = utility_of_voting < utility_of_stopping_voting
    def get_addnl_ballot(): Boolean = {
        /* pay for it */
        qstn.allowanceBalance -= ballotCost

        /* random worker gets chosen to vote */
        val worker = Random.shuffle(qstn.workers.toList).head
        worker.generateVote(qstn.difficulty)  // currently just updates 'vote' field
        worker.vote
    }

    var votes = List[Boolean]()

    while (decide_whether_to_vote) {
        votes ::= get_addnl_ballot()
    }
}

trait ParticleFilter
{
    val priorDistribution: Array[Double]
    def updatePrior {propagate; observe; sample; re_estimate}
    def propagate
    def observe
    def re_estimate
    def sample
}

case class QualityDistribution(numParticles: Int, dist: RealDistribution)
    extends ParticleFilter
{
    val priorDistribution = dist.sample(numParticles)

    def propagate {} // [DTC] (eq. 1)

    def observe {}

    def re_estimate {}

    def sample {}
}

case class ConditionalImprovementGivenTurker(numParticles: Int, dist: RealDistribution)
    extends ParticleFilter
{
    val priorDistribution = dist.sample(numParticles)

    def propagate   {}

    def observe     {}

    def re_estimate {}

    def sample      {}
}

case class Turker(trueGX: Double, qstn: Question)
{
    val learningRate = 0.05  // perhaps there is a better number to use here
    var estGX: Double = 1    // was simply set to the mean of the true distribution

    def find_improvementFunctionMean: Double = // [DTC] (eq. 13)
        qstn.q +
          0.5 * ((1 - qstn.q) * (accuracy(qstn.difficulty) - 0.5) +
          qstn.q * (accuracy(qstn.difficulty) - 1))

    val turkerFctn =  // [DTC] § Experiments
        ConditionalImprovementGivenTurker(100,
            new BetaDistribution(
                10*find_improvementFunctionMean,
                10*(1-find_improvementFunctionMean)))

    var vote = false

    def generateVote(d: Double) { vote = random < accuracy(d) } // [DTC] (eq. 3)

    def accuracy(d: Double) = (1/2)*(1+pow(1-d, estGX))

    def updateGX(qstn: Question) = {    // [DTC] (below eq. 12)
        if(vote == qstn.trueAnswer)
            estGX -= qstn.difficulty * learningRate
        else
            estGX += (1-qstn.difficulty) * learningRate
    }
}

case class Question(trueAnswer: Boolean, q: Double, dC: Double)
{
    val allowance: Int = 400  // the amount of $$ we start out with
    var allowanceBalance: Double = allowance

    val workerDist = new NormalDistribution(1,1)

    var qPrime = 0.0
    def difficulty = 1 - pow((q - qPrime).abs, dC)      // [DTC] (eq. 2)
    def utility = 1000 * (exp(q) - 1) / (exp(1) - 1)    // [DTC] § Experimental Setup
    def dStar = ???                                     // [DTC] (eq. 12)
    val priorQualityDensityFctn =
        QualityDistribution(100, new BetaDistribution(1, 9)) // [DTC] § Experimental Setup

    /* the 'belief state' */
    val joint_prob_dens_of_q_and_qPrime = ParticleFilter(100, new BetaDistribution(1,5)) // random parameters)
    val numTurkers = 1000
    val workers = Array.fill(numTurkers)(Turker(workerDist.sample, this))
}

object FirstExperiment
{
    // what is this?? shouldn't it be a function of q?
    def initialArtifactQuality = new BetaDistribution(1,9).sample

    val improvementCost = .05
    val difficultyConstant = 0.5
    val lookaheadDepth = 3
    val numTrials = 10000

    val qstn = Question(trueAnswer=true, initialArtifactQuality, difficultyConstant)

    /* methods I'll probably want */
    def choose_action() {  // I don't think this is correctly implemented
        if (utility_of_current_artifact > utility_of_ballot_job &&
          utility_of_current_artifact > utility_of_improvement_step) {
            submit_final()
        } else if (utility_of_ballot_job > utility_of_improvement_step) {
            BallotJob
        } else {
            improvement_job()
        }
    }

    def improvement_job() = ???

    def estimate_prior_for_alphaPrime() = ???

    def update_posteriors_for_alphas() = ???

    def utility_of_improvement_step(): Double = ???

    def utility_of_ballot_job(): Double = ???

    def re_estimate_worker_accuracy(workerIndex: Int) { ??? }

    def update_belief_state() { ??? }

    def submit_final() = sys.exit(0)

    /* methods I have written */
    def utility_of_current_artifact(): Double = 25*qstn.q  // AI-AAI
}

object TestStuff extends App
{

}

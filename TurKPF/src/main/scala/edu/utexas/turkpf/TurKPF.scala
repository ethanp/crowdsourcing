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
    def get_addnl_ballot(): Boolean = {
        /* pay for it */
        qstn.allowanceBalance -= ballotCost

        /* random worker gets chosen to vote */
        val worker = Random.shuffle(qstn.workers.toList).head
        worker.generateVote(qstn.difficulty)  // currently just updates 'vote' field
        worker.vote
    }
}

case class ParticleFilter(numParticles: Int, dist: RealDistribution)
{
    val priorDistribution = dist.sample(numParticles)
    def updatePrior = ???  // propagate, observe, re-estimate
    def propagate = ???
    def observe = ???
    def re_estimate = ???
}

case class Turker(trueGX: Double)
{
    val learningRate = 0.05  // maybe there's a better number to use here
    var estGX: Double = 1  // mean of true distribution (not sure what else to use)
    val conditionalImprovementGivenTurkerFunction = ParticleFilter(100)
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
    val allowance = 400  // the amount of $$ we start out with
    var allowanceBalance = allowance

    // TODO: figure out a reasonable stdev
    val workerDist = new NormalDistribution(1,1)

    var qPrime = 0.0
    def difficulty = 1 - pow((q - qPrime).abs, dC)
    def dStar = ???  // [DTC] (below eq. 12)
    val priorDensityFctn = ParticleFilter(100, new BetaDistribution(1,5)) // random parameters

    /* the 'belief state' */
    val joint_prob_dens_of_q_and_qPrime = ParticleFilter(100, new BetaDistribution(1,5)) // random parameters)
    val numTurkers = 1000
    val workers = Array.fill(numTurkers)(Turker(workerDist.sample))
}

object FirstExperiment
{
    // what is this?? shouldn't it be a function of q?
    val utilityFunction = 1000 * (exp(3) - 1) / (exp(1) - 1)
    def initialArtifactQuality = new BetaDistribution(1,9).sample


    val improvementCost = .05
    val difficultyConstant = 0.5
    val lookaheadDepth = 3
    val numTrials = 10000

    val question = Question(trueAnswer=true, initialArtifactQuality, difficultyConstant)


    /* methods I'll probably want */
    def choose_action() = ???

    def improvement_job() = ???

    def estimate_prior_for_alphaPrime() = ???

    def update_posteriors_for_alphas() = ???

    def utility_of_stopping_voting(): Double = ???

    def utility_of_voting(): Double = ???

    def utility_of_improvement_step(): Double = ???

    def re_estimate_worker_accuracy(workerIndex: Int) { ??? }

    def update_belief_state() { ??? }

    /* methods I have written */
    def utility_of_current_artifact(): Double = 25*question.q  // AI-AAI

    def decide_whether_to_vote(): Boolean = utility_of_voting < utility_of_stopping_voting
}

object TestStuff extends App
{

}


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
import org.apache.commons.math3.distribution.BetaDistribution
import org.apache.commons.math3.distribution.NormalDistribution
import scala.util.Random


/* notes:
 *  Type Info: cmd-T
 */

case class Turker(trueGX: Double)
{
    var estGX: Double = 1  // mean of true distribution (not sure what else to use)
    def generateVote(d: Double): Boolean = random < accuracy(d)
    def accuracy(d: Double) = (1/2)*(1+pow(1-d, estGX))
    def updateGX() = ???
}

case class Question(trueAnswer: Boolean, q: Double)
{
    var qPrime = 0.0
    def difficulty = 1 - pow((question.q - qPrime).abs, difficultyConstant)
    def priorDensityFctn = ???  // this will use a particle filter
}

object FirstExperiment
{
    // what is this?? shouldn't it be a function of q?
    val utilityFunction = 1000 * (exp(3) - 1) / (exp(1) - 1)
    def initialArtifactQuality = new BetaDistribution(1,9).sample

    // TODO: figure out a reasonable stdev
    val workerDist = new NormalDistribution(1,1)

    val improvementCost = .05
    val ballotCost = .01
    val difficultyConstant = 0.5
    val lookaheadDepth = 3
    val allowance = 400  // the amount of $$ we start out with
    var allowanceBalance = allowance
    val numTurkers = 1000
    val numTrials = 10000

    val question = Question(trueAnswer=true, initialArtifactQuality)

    val workers = Array.fill(numTurkers)(Turker(workerDist.sample))

    /* the flowchart in method-names */
    def choose_action() = ???

    def decide_whether_to_improve() = ???

    def improvement_job() = ???

    def estimate_prior_for_alphaPrime() = ???

    def update_posteriors_for_alphas() = ???

    /* other methods I'll probably want */
    def utility_of_stopping_voting(): Double = ???

    def utility_of_voting(): Double = ???

    def utility_of_improvement_step(): Double = ???

    def re_estimate_worker_accuracy(workerIndex: Int) { ??? }

    def update_belief_state() { ??? }

    /* methods I have written */
    def utility_submitting_final(): Double = 25*question.q  // AI-AAI

    def ballot_job(): Boolean = {
        /* pay for it */
        allowanceBalance -= ballotCost

        /* random worker gets chosen to vote */
        val worker = Random.shuffle(workers.toList).head
        worker.generateVote(question.difficulty)
    }
    def decide_whether_to_vote(): Boolean = utility_voting < utility_stop_voting
}

object TestStuff extends App
{

}


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


/* notes:
 *  Type Info: cmd-T
 *
 *  Doesn't use the Standard Directory Layout:
 *   http://maven.apache.org/guides/introduction/introduction-to-the-standard-directory-layout.html
 */

case class Turker(trueError: Double, var estimatedError: Double)
{
    def generateVote() = ???
}

object FirstExperiment extends App
{
    val utilityFunction = 1000 * (exp(3) - 1) / (exp(1) - 1)
    val initialArtifactQuality = new BetaDistribution(1,9).sample

    // TODO: figure out a reasonable stdev
    val workerDist = new NormalDistribution(1,1)

    val costRatioImprovementsVsBallots = 3  // "because ballots take less time"
    val ballotCost = 10
    val improvementCost = ballotCost * costRatioImprovementsVsBallots
    val difficultyConstant = 0.5
    val lookaheadDepth = 3
    val allowance = 400  // the amount of $$ we start out with
    var allowanceBalance = allowance
    val numTurkers = 1000
    val numTrials = 10000

    var q = initialArtifactQuality
    var qPrime = 0.0
    var difficulty = 0.0
    val workers = Array.fill(numTurkers)(Turker(workerDist.sample))

    /* the flowchart in method-names */
    def decide_whether_to_improve() = ???
    def improvement_job() = ???
    def estimate_prior_for_alphaPrime() = ???
    def decide_whether_to_vote() = ???
    def ballot_job() = ???
    def update_posteriors_for_alphas() = ???

    /* other methods I'll probably want */
    def submit_final() = ???
    def get_utility_of_stopping_voting() = ???
    def get_utility_of_one_more_ballot() = ???
    def get_utility_of_improvement_step() = ???
    def get_utility_of_submitting_final() = ???
    def update_difficulty() {difficulty = 1 - pow((q-qPrime).abs, difficultyConstant)}
    def update_worker_accuracy_estimate(workerIndex: Int) = ???
    def update_belief_state() = ???

}


/**
 * Adapts TurKontrol into a Particle Filter
 * TurKontrol was presented in "Decision-Theoretic Control of Crowd-Sourced Workflows"
 * by Peng Dai, Mausam, and Daniel S. Weld (2010)
 *
 * Author:  Ethan Petuchowski
 * Date:    6/18/13
 * License: Unknown
 */

import math._
import org.apache.commons.math3.distribution.BetaDistribution
import org.apache.commons.math3.distribution.NormalDistribution


/* notes:
 *  Type Info: cmd-T
 */

case class Turker(errorCoeff: Double)

object FirstExperiment extends App
{
    val utilityFunction = 1000 * (exp(3) - 1) / (exp(1) - 1)
    val initialArtifactQuality = new BetaDistribution(1,9).sample

    // TODO: figure out a reasonable stdev, I set it to 1 with 0 thought
    val workerDist = new NormalDistribution(1,1)

    val costRatioImprovementsVsBallots = 3  // "because ballots take less time" (pg. 1172)
    val ballotCost = 10
    val improvementCost = ballotCost * costRatioImprovementsVsBallots
    val difficultyConstant = 0.5
    val lookaheadDepth = 3
    val allowance = 400  // the amount of $$ we start out with
    var allowanceBalance = allowance
    val numTurkers = 1000
    val numTrials = 10000  // 10k

    var q = initialArtifactQuality
    val workers = (1 to 100).map(x => Turker(workerDist.sample)).toArray

    for (i <- numTrials) {
        // make a decision here.
    }
    def improvement_job() = ???
    def vote_job() = ???
    def submit_final() = ???
    def get_utility_of_stopping_voting() = ???
    def get_utility_of_one_more_ballot() = ???
    def get_utility_of_improvement_step() = ???
    def get_utility_of_submitting_final() = ???
    def update_difficulty() = ???
    def update_worker_accuracy_estimates() = ???
    def update_belief_state() = ???

}


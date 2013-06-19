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

class ExperimentalPplGenerator extends App
{
    val utilityFunction = 1000 * (exp(3) - 1) / (exp(1) - 1)
    val initialArtifactQuality = new BetaDistribution(1,9).sample
    val costRatioImprovementsVsBallots = 3  // "because ballots take less time" (pg. 1172)
    val difficultyConstant = 0.5
    val numWorkers = 1000  // these may need to become instances of a class?
    val workers = new Array[Turker](numWorkers)
    workers.foreach(println(_.errorCoefficient))
}

class Turker
{
    // TODO: figure out what the stdev of this is supposed to be
    // I set it to 2 with 0 thought
    val errorCoefficient = new NormalDistribution(1,2).sample
}


package edu.utexas.turkpf

import org.apache.commons.math3.distribution.NormalDistribution
import scala.math._

object FirstExperiment extends Exp {

    /* [DTC] gmX "follow a bell shaped distribution"
     *           "average error coefficient gm=1",
     *             where gmX > 0 */
    val WORKER_DIST = new NormalDistribution(1,0.2)

    /* this is a method so that it can be set to generate a random
     * initial quality every time a question starts */
    def INITIAL_QUALITY = .01 // new BetaDistribution(1,9).sample

    val IMPROVEMENT_COST    = 3.0
    val BALLOT_COST         = .75
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 2
    val NUM_QUESTIONS       = 10
    val INITIAL_ALLOWANCE   = 10.0
    val NUM_PARTICLES       = 100
    val UTILITY_OF_$$$      = 1.0  // let's just say it's "1.0" for simplicity
    val qstn = Question()
}

trait Exp {
    val WORKER_DIST:         NormalDistribution
    def INITIAL_QUALITY:     Double
    // [DTC] § Experimental Setup, i.e. U(q)
    def estimate_artifact_utility(qlty: Double): Double = 1000 * (exp(qlty) - 1) / (exp(1) - 1)
    val IMPROVEMENT_COST:    Double
    val BALLOT_COST:         Double
    val DIFFICULTY_CONSTANT: Double
    val LOOKAHEAD_DEPTH:     Int
    val NUM_QUESTIONS:       Int
    val INITIAL_ALLOWANCE:   Double
    val NUM_PARTICLES:       Int
    val UTILITY_OF_$$$:      Double


    def writeParameters(output: StringBuilder) = {
        output.append(
            WORKER_DIST.getMean + "\t" +
            WORKER_DIST.getStandardDeviation + "\t" +
            INITIAL_QUALITY     + "\t" +
            IMPROVEMENT_COST    + "\t" +
            BALLOT_COST         + "\t" +
            DIFFICULTY_CONSTANT + "\t" +
            LOOKAHEAD_DEPTH     + "\t" +
            NUM_QUESTIONS       + "\t" +
            INITIAL_ALLOWANCE   + "\t" +
            NUM_PARTICLES       + "\t" +
            UTILITY_OF_$$$      + "\t"
        )
    }
}

object Test_FirstExperiment extends App { println(FirstExperiment.writeParameters(new StringBuilder)) }

object Vary_Ballot_Cost extends App with Exp {

    /* [DTC] gmX "follow a bell shaped distribution"
     *           "average error coefficient gm=1",
     *             where gmX > 0 */
    val WORKER_DIST = new NormalDistribution(1,0.2)

    /* this is a method so that it can be set to generate a random
     * initial quality every time a question starts */
    def INITIAL_QUALITY = .01 // new BetaDistribution(1,9).sample

    val IMPROVEMENT_COST    = 3.0
    val BALLOT_COST         = .75
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 2
    val NUM_QUESTIONS       = 10
    val INITIAL_ALLOWANCE   = 10.0
    val NUM_PARTICLES       = 100
    val UTILITY_OF_$$$      = 1.0  // let's just say it's "1.0" for simplicity
    val qstn = Question()

    println(writeParameters(new StringBuilder))
    while(true) qstn.look_ahead()
}

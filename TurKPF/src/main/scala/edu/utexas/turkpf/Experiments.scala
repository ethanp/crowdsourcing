package edu.utexas.turkpf

import org.apache.commons.math3.distribution.NormalDistribution
import scala.math._

trait Exp {

    /* [DTC] gmX "follow a bell shaped distribution"
     *           "average error coefficient gm=1",
     *             where gmX > 0 */
    val WORKER_DIST         = new NormalDistribution(1,0.2)

    /* this is a method so that it can be set to generate a random
     * initial quality every time a question starts */
    def INITIAL_QUALITY     = .01 // new BetaDistribution(1,9).sample

    // [DTC] § Experimental Setup, i.e. U(q)
    def estimate_artifact_utility(qlty: Double): Double = 1000 * (exp(qlty) - 1) / (exp(1) - 1)

    val IMPROVEMENT_COST    = 3.0
    val BALLOT_COST         = .75
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 2
    val NUM_QUESTIONS       = 10
    val INITIAL_ALLOWANCE   = 10.0
    val NUM_PARTICLES       = 100
    val UTILITY_OF_$$$      = 1.0  // let's just say it's "1.0" for simplicity


    def writeParameters(output: StringBuilder): String = {
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
        ).toString()
    }
}

object FirstExperiment extends Exp { val qstn = Question(/* default args */) }

object Test_FirstExperiment extends App { println(FirstExperiment.writeParameters(new StringBuilder)) }

object Vary_Ballot_Cost extends App with Exp {
    val qstn = Question(args = Set("finalUtil"), outFile = "test.txt")
    qstn.state.output.write(writeParameters(new StringBuilder))
    while(true) qstn.look_ahead()
    qstn.state.output.close()  // first flushes, then closes.
}

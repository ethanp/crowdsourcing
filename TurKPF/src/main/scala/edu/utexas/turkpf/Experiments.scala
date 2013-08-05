package edu.utexas.turkpf

import org.apache.commons.math3.distribution.NormalDistribution
import scala.math._

case class Exp() {

    // enum
    val EXP_CHOOSEACTION = "0\t"
    val EXP_LOOKAHEAD    = "1\t"

    /* [DTC] gmX "follow a bell shaped distribution"
     *           "average error coefficient gm=1",
     *             where gmX > 0 */
    val WORKER_DIST         = new NormalDistribution(1,0.2)

    /* this is a method so that it can be set to generate a random
     * initial quality every time a question starts */
    def INITIAL_QUALITY     = .01 // new BetaDistribution(1,9).sample

    // [DTC] § Experimental Setup, i.e. U(q)
    def UTILITY_FUNCTION(qlty: Double): Double = 1000 * (exp(qlty) - 1) / (exp(1) - 1)

    val IMPROVEMENT_COST    = 3.0
    val BALLOT_COST         = .75
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 2
    val NUM_QUESTIONS       = 10
    val INITIAL_BALANCE     = 10.0
    val NUM_PARTICLES       = 100
    val LEARNING_RATE       = 0.05
    val UTILITY_OF_$$$      = 1.0  // let's just say it's "1.0" for simplicity

    def parametersAsString: String = {
        WORKER_DIST.getMean   + "\t" +
          WORKER_DIST.getStandardDeviation + "\t" +
          INITIAL_QUALITY     + "\t" +
          IMPROVEMENT_COST    + "\t" +
          BALLOT_COST         + "\t" +
          DIFFICULTY_CONSTANT + "\t" +
          LOOKAHEAD_DEPTH     + "\t" +
          NUM_QUESTIONS       + "\t" +
          INITIAL_BALANCE     + "\t" +
          NUM_PARTICLES       + "\t" +
          LEARNING_RATE       + "\t" +
          UTILITY_OF_$$$      + "\t"
    }
}

object FirstExperiment extends App { val qstn = Question(/* default args */) }

object Vary_Ballot_Cost extends App {
    val exper = Exp()
    val runner = Runnit(exper)
    runner.run("test.txt", exper.EXP_LOOKAHEAD)
}

case class Runnit(exper: Exp) {
    var n = 0
    var qstn = Question()
    def continue: Boolean = {
        n += 1
        n < exper.NUM_QUESTIONS
    }
    def run(outFile: String = "test.txt", mode: String = "1\t") {
        while (continue) {
            qstn = Question(outFile = outFile)
            qstn.state.output.write(exper.parametersAsString)
            qstn.state.output.write(mode)
            if (mode == exper.EXP_CHOOSEACTION)
                while(qstn.choose_action()){}
            else while(qstn.look_ahead()){}
        }
    }
}

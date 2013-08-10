package edu.utexas.turkpf

import org.apache.commons.math3.distribution.NormalDistribution
import scala.math._

case class CONSTANTS() {

    // enum
    val NO_LOOKAHEAD    = "0\t"
    val USE_LOOKAHEAD   = "1\t"

    /* [DTC] gmX "follow a bell shaped distribution"
     *           "average error coefficient gm=1",
     *             where gmX > 0 */
    var WORKER_DIST         = new NormalDistribution(1,0.2)

    /* this is a method so that it can be set to generate a random
     * initial quality every time a question starts */
    def INITIAL_QUALITY     = .01 // new BetaDistribution(1,9).sample

    // [DTC] § Experimental Setup, i.e. U(q)
    def UTILITY_FUNCTION(qlty: Double): Double = 1000 * (exp(qlty) - 1) / (exp(1) - 1)

    var IMPROVEMENT_COST    = 3.0
    var BALLOT_COST         = .75
    var DIFFICULTY_CONSTANT = 0.5
    var LOOKAHEAD_DEPTH     = 2
    var NUM_QUESTIONS       = 200
    var INITIAL_BALANCE     = 10.0
    var NUM_PARTICLES       = 500
    var LEARNING_RATE       = 0.05
    var UTILITY_OF_$$$      = 1.0  // let's just say it's "1.0" for simplicity

    def columnTitles: String = {
        "mode\t"                  +
        "worker dist mean\t"      +
        "worker dist stdev\t"     +
        "initial quality\t"       +
        "improvement cost\t"      +
        "ballot cost\t"           +
        "difficulty constant\t"   +
        "lookahead depth\t"       +
        "num questions\t"         +
        "initial balance\t"       +
        "num particles\t"         +
        "learning rate\t"         +
        "utility of $$$\t"        +
        "action list\t"           +
        "final utility\n"
    }

    def parameterValuesAsString: String = {
        s"${WORKER_DIST.getMean}\t" +
        s"${WORKER_DIST.getStandardDeviation}\t" +
        s"$INITIAL_QUALITY\t" +
        s"$IMPROVEMENT_COST\t" +
        s"$BALLOT_COST\t" +
        s"$DIFFICULTY_CONSTANT\t" +
        s"$LOOKAHEAD_DEPTH\t" +
        s"$NUM_QUESTIONS\t" +
        s"$INITIAL_BALANCE\t" +
        s"$NUM_PARTICLES\t" +
        s"$LEARNING_RATE\t" +
        s"${UTILITY_OF_$$$}\t"
    }
}

object TestThatItRuns extends App {
    val qstn = Question(/* default args */)
    while(qstn.dont_lookahead()){}
}

case class Runnit(exper: CONSTANTS) {
    var qstn = Question()
    def run(modifyConstants: () => Unit,
            outFile: String = "test.txt",
            mode: String = "1\t") {
        for (i <- 1 to exper.NUM_QUESTIONS) {
            qstn = Question(outFile = outFile)
            modifyConstants()
            if (i == 1) qstn.state.output.write(exper.columnTitles)
            qstn.state.output.write(mode)
            qstn.state.output.write(exper.parameterValuesAsString)
            if (mode == exper.NO_LOOKAHEAD)
                while(qstn.dont_lookahead()){}
            else while(qstn.look_ahead()){}
        }
    }
}

trait ExperimentRunner {
    val exper = CONSTANTS()
    val runner = Runnit(exper)
    val curTime = new java.text.SimpleDateFormat("MM-dd-hh-mm").format(new java.util.Date())
    var fileName = this.getClass.toString
    fileName.drop(fileName.lastIndexOf("."))
    var searchAlgorithm = exper.USE_LOOKAHEAD
    def modifyConstants(): Unit = {}
    def run() {
        runner.run(modifyConstants, s"${fileName}_$curTime.tsv", searchAlgorithm)
    }
}

/************* Experiments: **************/
/* every variable defined in each experiment overrides the default */
/* TODO: "this stub corresponds to 'this' experiment in the paper" */

object SweepNumParticles extends App with ExperimentRunner {
    fileName = "SweepNumParticles"
    override def modifyConstants(): Unit = {
        exper.NUM_PARTICLES += 100
    }
    run()
}

object SweepNumParticles2 extends App with ExperimentRunner {
    fileName = "SweepNumParticles2"
    exper.NUM_PARTICLES = 5
    exper.NUM_QUESTIONS = 500
    override def modifyConstants(): Unit = {
        exper.NUM_PARTICLES += 1
    }
    run()
}

object JustRun200Times extends App with ExperimentRunner { run() }

object NoLookahead200Times extends App with ExperimentRunner {
    searchAlgorithm = exper.NO_LOOKAHEAD
    run()
}

object SweepImpCost extends App with ExperimentRunner {
    exper.IMPROVEMENT_COST = .05
    exper.INITIAL_BALANCE  = 15.0
    exper.BALLOT_COST      = 1.0
    override def modifyConstants(): Unit = {
        exper.IMPROVEMENT_COST += .05
    }
    run()
}

object SweepImpCost2 extends App with ExperimentRunner {
    exper.IMPROVEMENT_COST = .05
    exper.INITIAL_BALANCE  = 10.0
    exper.BALLOT_COST      = 1.0
    override def modifyConstants(): Unit = {
        exper.IMPROVEMENT_COST += .05
    }
    run()
}

object SweepImpCost3 extends App with ExperimentRunner {
    exper.IMPROVEMENT_COST = .1
    exper.INITIAL_BALANCE  = 100.0
    exper.BALLOT_COST      = 3.0
    override def modifyConstants(): Unit = {
        exper.IMPROVEMENT_COST += .1
    }
    run()
}

object SweepGmX extends App with ExperimentRunner {
    var i = .1
    override def modifyConstants(): Unit = {
        i += .03
        exper.WORKER_DIST = new NormalDistribution(i, 0.2)
    }
    run()
}

object SweepLookaheadDepth extends App with ExperimentRunner {
    exper.LOOKAHEAD_DEPTH = 3
    var i = 1
    exper.WORKER_DIST = new NormalDistribution(5, 2)
    override def modifyConstants(): Unit = {
        i += 1
        if (i % 100 == 0)
            exper.LOOKAHEAD_DEPTH += 1
    }
    run()
}

/**
 * Adapts TurKontrol into a Particle Filter
 * TurKontrol was presented in "Decision-Theoretic Control of Crowd-Sourced Workflows"
 * by Peng Dai, Mausam, and Daniel S. Weld (2010)
 *
 * Author:          Ethan Petuchowski
 * Date Started:    6/18/13
 * License:         Unknown
 */

package edu.utexas.turkpf

// TODO read it into an Excel and graph it at various levels of stuff
// the first thing it prints each run should be a listing of the parameter values
// then the values can be used to "join" the data within Excel
// maybe the first value denotes which version of the program I'm running:
//   { 0: choose_action(), 1: look_ahead() }

import java.io.{File, PrintWriter}
import math._
import org.apache.commons.math3.distribution.BetaDistribution
import scala.annotation.tailrec
import scala.util.Random

/* this is so one can choose a set of parameters by replacing this line with
 *  import SecondExperiment.experiment_parameters._  and so on  */
import FirstExperiment._


// implicitly add "normalize" to Array[Dbl] to make ||Array|| = 1
abstract class addNorm(a: Array[Double]) { def normalize: Array[Double] }

/* Particle Filter representation of artifact-quality Probability Density Functions */
case class PF(numParticles: Int, particles: Array[Double]) {

    def this(n: Int) = this(n, new BetaDistribution(1,9).sample(n))

    def this(particles: Array[Double]) = this(NUM_PARTICLES, particles)

    def this() = this(NUM_PARTICLES, new BetaDistribution(1,9).sample(NUM_PARTICLES))

    // [DTC] (eq. 13)
    def find_improvementFunctionMean(qlty: Double): Double = {
        val accuracy: Double = qstn.wrkrs.accuracy(qlty)
        qlty + 0.5 * ((1 - qlty) * (accuracy - 0.5) + qlty * (accuracy - 1))
    }

    /** [DTC] (eq. 1), q => generate f_{ Q' | particle.q } (q') and sample from it
     * IN WORDS:
     * We go through each particle, and using its value (which is an estimate of the qlty),
     * we generate an "improvement distribution" describing how much we can expect [an avg.]
     * worker to improve the quality of the artifact. We take a "random" stab at the new
     * quality after the "improvement job" by sampling from the "improvement distribution".
     */
    def predict: PF = new PF(
        particles map { q =>  // [DTC] § Experimental Setup
            val mu = find_improvementFunctionMean(q)
            new BetaDistribution(10 * mu, 10 * (1 - mu)).sample
        }
    )

    /* fold ALL the partBs through prob_true() with EACH partA :  [DTC] (eq. 5-6) */
    def weight_and_sample(vote:  Boolean,
                          that:  PF,
                          prime: Boolean):
    PF = {
        // get P( b_{n+1} | q ) :  [DTC] (eq. 6) ; [TK=>PF] (eq. 8-9)
        val weights = {
            this.particles map { partA =>
                (0.0 /: that.particles)((sum, partB) => {
                    val (p1, p2) = if (prime) (partB, partA) else (partA, partB)
                    sum + qstn.invertIfFalse(vote, qstn.prob_true_given_Qs(p1, p2))
                })
            }}.normalize

        // get f_{ Q | b_{n+1} } (q) :  [DTC] (eq. 5)
        new PF( particles map { _ => repopulate(weights) } )
    }

    /* algorithm for sampling from given set of points with associated weights:
     * generate a random number in [0,1), use the CDF of the weights to use the
     * random number to figure out what the sampled value is
     */
    def repopulate(weights: Array[Double]): Double = {
        val rand = Random.nextDouble()
        var accrue = 0.0
        for ((weight, index) <- weights.zipWithIndex) {
            accrue += weight
            if (rand < accrue)
                return particles(index)
        }
        throw new RuntimeException // shouldn't ever get here
        particles(particles.length-1)
    }

    implicit def addNorm(a: Array[Double]):
        addNorm = new addNorm(a) {
            def normalize = {
                val norm = a.sum
                a map {_/norm}
            }
        }
}

/* model all workers with just one worker-independent model */
case class Workers(trueGX: Double) {
    val LEARNING_RATE = 0.05
    var estGX = 1.0  // set to the mean of the true distribution
                     // could be altered to test robustness

    // [DTC] (above eq. 3)
    def accuracy(difficulty: Double) = 0.5 * (1 + pow(1 - difficulty, estGX))

    // higher GX means Worse worker
    def updateGX(votes: List[Boolean]) {    // [DTC] (below eq. 12)
        val (trues, falses) = votes.partition(_ == true)
        val trueBigger = trues.length > falses.length
        val bigger  = if (trueBigger) trues.length  else falses.length
        val smaller = if (trueBigger) falses.length else trues.length
        val d = qstn.dStar
        estGX -= bigger * d * LEARNING_RATE
        estGX += smaller * (1 - d) * LEARNING_RATE
    }
}

case class QuestionState() {
    var balance = INITIAL_ALLOWANCE
    var qlty = INITIAL_QUALITY
    var qltyPrime = 0.0
    var votes = List[Boolean]()
    var workerTrueGm = WORKER_DIST.sample
    while (workerTrueGm < 0) workerTrueGm = WORKER_DIST.sample
    var f_Q = new PF  // defaults to BetaDist(1,9)

    // [DTC] § Experimental Setup
    // my goal here is to initialize the qPrime prior a little higher than the q prior
    // a better way to do this would be nice
    var f_QPrime = new PF(
        new BetaDistribution(2,9).sample(NUM_PARTICLES)
    )
    val output: PrintWriter = new PrintWriter(new File("test.txt"))
}

case class Question() {

    val state = QuestionState()

    // [DTC] trueGX > 0; code is worker_dist-agnostic
    val wrkrs = Workers(state.workerTrueGm)

    // [DTC] (eq. 2)
    def difficulty(qlty:      Double,
                   qltyPrime: Double):
    Double = 1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)

    def utility_of_submitting(f_Q:      PF = state.f_Q,
                              f_QPrime: PF = state.f_QPrime):
    Double = max(convolute_Utility_with_Particles(f_Q),
                 convolute_Utility_with_Particles(f_QPrime))

    // the math for this checks out
    def convolute_Utility_with_Particles(pf: PF):
    Double = (0.0 /: pf.particles)(_ + estimate_artifact_utility(_)) / NUM_PARTICLES

    // [DTC] (eq. 12)
    def dStar: Double = {
        (0.0 /: state.f_Q.particles)((sum, q) =>
            sum + (0.0 /: state.f_QPrime.particles)((sum2, qPrime) =>
                sum2 + difficulty(q, state.qltyPrime)
            )
        ) / (NUM_PARTICLES * NUM_PARTICLES)
    }

    def submit_final() = {
        println("Final Utility: " + (utility_of_submitting() + state.balance * UTILITY_OF_$$$))
        state.output.write("2\n")
        state.output.close()
        sys exit 0
    }

    // [DTC] (top-right of page 4)
    def utility_of_improvement_job(f_Q:      PF = state.f_Q,
                                   f_QPrime: PF = state.f_QPrime):
    Double = {
        val (orig_predicted, prime_predicted) = utility_of_stopping_voting(f_Q, f_QPrime)
        max(orig_predicted, prime_predicted) - IMPROVEMENT_COST * UTILITY_OF_$$$
    }

    // [DTC] (eq. 9)
    def utility_of_stopping_voting(f_Q:      PF = state.f_Q,
                                   f_QPrime: PF = state.f_QPrime):
    (Double, Double) = {
        val orig_predicted = convolute_Utility_with_Particles(f_Q.predict)
        val prime_predicted = convolute_Utility_with_Particles(f_QPrime.predict)
        if (f_Q == state.f_Q) {
            println("Predicted Original Utility:   " + orig_predicted)
            println("Predicted Prime Utility:      " + prime_predicted)
        }
        (orig_predicted, prime_predicted)
    }

    /***************************** BALLOT JOB STUFF *******************************/
    // [DTC] (bottom-left Pg. 4) ;  i.e.  E[ U( Q | b_{n} + 1 ) ]
    def utility_of_voting(f_Q:      PF = state.f_Q,
                          f_QPrime: PF = state.f_QPrime):
    Double = {
        val probYes = probability_of_yes_vote(f_Q, f_QPrime)
        println("probYes: " + probYes)
        max(
            expVal_after_a_vote(dist_Q_after_vote(f_Q, f_QPrime), probYes),
            expVal_after_a_vote(dist_QPrime_after_vote(f_Q, f_QPrime), probYes)
        ) - BALLOT_COST * UTILITY_OF_$$$
    }

    // [DTC] (bottom-left Pg. 4)
    /* PSEUDOCODE for calculating P(b_{n+1}):
     * For each particle in the "normal" set
     *  "Convolute" the [entire] "predicted" set of particles with the accuracy according to whether
     *  the particle in the predicted set has a higher value than the one in the normal set (eq. 3)
     *   This convolution will yield a scalar
     * This [outer] summation will yield another scalar (our result, P(b_{n+1}))
     */
    def probability_of_yes_vote(f_Q:      PF = state.f_Q,
                                f_QPrime: PF = state.f_QPrime):
    Double = {
        (0.0 /: f_Q.particles)((sumA, particleA) =>
            sumA + particleA * (0.0 /: f_QPrime.particles)((sumB, particleB) =>
                sumB + prob_true_given_Qs(particleA, particleB)
            ) / NUM_PARTICLES
        ) / NUM_PARTICLES

    }

    // I checked and this function works properly
    def prob_true_given_Qs(q: Double, qPrime: Double): Double =
        qstn.invertIfFalse(q < qPrime, wrkrs.accuracy(qstn.difficulty(q, qPrime)))

    def expVal_after_a_vote(f: Boolean => PF,
                            probYes: Double):
    Double = {
        if (probYes > 1 || probYes < 0) throw new RuntimeException
        convolute_Utility_with_Particles(f(true)) * probYes +
            convolute_Utility_with_Particles(f(false)) * (1 - probYes)
    }

    // [DTC] (eq. 5)
    def dist_Q_after_vote(f_Q:      PF = state.f_Q,
                          f_QPrime: PF = state.f_QPrime)
                         (vote:     Boolean):
    PF = f_Q.weight_and_sample(vote, f_QPrime, false)

    // [DTC] (eq. 7-8) the same as above
    def dist_QPrime_after_vote(f_Q:      PF = state.f_Q,
                               f_QPrime: PF = state.f_QPrime)
                              (vote:     Boolean):
    PF = f_QPrime.weight_and_sample(vote, f_Q, true)


    // [DTC] (eqs. 4,5,6,7,8)
    def ballot_job(f_Q:      PF,
                   f_QPrime: PF,
                   balance:  Double,
                   vote:     Any = None):
    (PF, PF, Double) = {
        val theVote: Boolean = vote match {
            case v if v == None => random < probability_of_yes_vote(f_Q, f_QPrime)
            case v => v.asInstanceOf[Boolean]
        }
        (dist_Q_after_vote(f_Q, f_QPrime)(theVote),
         dist_QPrime_after_vote(f_Q, f_QPrime)(theVote),
         balance - BALLOT_COST)
    }

    /**
     * 1. Pay for it
     * 2. Generate vote & append to list
     * 3. Update distributions
     * 4. Return vote
     */
    def ballot_job(): Boolean = {
        val vote = random < probability_of_yes_vote()
        state.votes ::= vote
        printf("vote :: %s #L293\n\n", vote.toString.toUpperCase)
        println(state.votes.mkString("(",", ",")"))
        val newState   = ballot_job(state.f_Q, state.f_QPrime, state.balance, vote)
        state.f_Q      = newState._1
        state.f_QPrime = newState._2
        state.balance  = newState._3
        state.output.write("1\t")
        vote
    }


    def invertIfFalse(t: Boolean, v: Double): Double = if (t) v else 1-v
    /****************************** END BALLOT JOB STUFF **************************/

    /*
     * 1. pay for it
     * 2. choose to continue with alpha or alphaPrime [DTC top-right pg. 4]
     * 3. create new prior for quality of improved artifact
     */
    def improvement_job(f_Q:      PF,
                        f_QPrime: PF,
                        balance:  Double):
    (PF, PF, Double) = {
        val imprvF_Q = f_Q.predict
        val imprvF_QPrime = f_QPrime.predict

        val newF_Q_of_q = {
            if (convolute_Utility_with_Particles(imprvF_Q)
              > convolute_Utility_with_Particles(imprvF_QPrime))
                imprvF_Q
            else
                imprvF_QPrime
        }
        (newF_Q_of_q, newF_Q_of_q.predict, balance - IMPROVEMENT_COST)
    }

    def improvement_job() {
        // clear votes out
        wrkrs.updateGX(state.votes)
        state.votes = List[Boolean]()
        val newState   = improvement_job(state.f_Q, state.f_QPrime, state.balance)
        state.f_Q      = newState._1
        state.f_QPrime = newState._2
        state.balance  = newState._3
        state.output.write("0\t")
    }

     /*   For Tuple in DataStruct:
      *     If Tuple doesn't end in a "submit":
      *       Replace that tuple with 3 new ones,
      *           1 for each action in [b,i,s],
      *           recalculating the utility
      * Sort the New Tuples by utility and perform actionList.head.
      */
    @tailrec
    final def look_ahead(lookaheadList: List[Lookahead] = List[Lookahead](),
                         currentDepth:  Int = 0) {
        // all done, perform the first action from the highest performing sequence of actions:
        if (currentDepth == LOOKAHEAD_DEPTH) {
            val bestPath: List[String] = lookaheadList.sortWith(_.utility > _.utility).head.actions
            println(bestPath.mkString("Best Path: ", ", ", ""))
            execute_action(bestPath.last)
        } else {
            // fill in the next layer of branches and recurse
            var newLookaheadList = List[Lookahead]()
            if (!lookaheadList.isEmpty) {
                for (route <- lookaheadList) {
                    newLookaheadList = go_deeper(route, newLookaheadList)
                }
            }
            else newLookaheadList =
                go_deeper(
                    new Lookahead(List[String](), state.f_Q, state.f_QPrime, 0, state.balance),
                    newLookaheadList
                )
            look_ahead(newLookaheadList, currentDepth + 1)
        }
    }

    def execute_action(action: String) { action match {
        case "improve" => { println("improvement"); improvement_job() }
        case "ballot" =>  { println("ballot");      ballot_job()      }
        case "submit" =>  { println("submit");      submit_final()    }
        case _ => throw new RuntimeException
    }}

    def go_deeper(route: Lookahead, newLookaheadList: List[Lookahead]): List[Lookahead] = {
        if (route.actions.isEmpty || route.actions.head != "submit") {
            val anotherLayer: List[Lookahead] = List("improve", "ballot", "submit") map { action =>
                val (f_qNew, f_QPrimeNew, curBalNew):
                (PF, PF, Double) = action match {

                    case "improve" =>
                        improvement_job(route.f_Q, route.f_QPrime, route.curBalance)

                    case "ballot" =>
                        ballot_job(route.f_Q, route.f_QPrime, route.curBalance)

                    case "submit" =>
                        (route.f_Q, route.f_QPrime, route.curBalance)

                    case _ => throw new RuntimeException

                }
                val utility: Double =
                    max(
                        convolute_Utility_with_Particles(f_qNew),
                        convolute_Utility_with_Particles(f_QPrimeNew)
                    ) - (route.curBalance - curBalNew * UTILITY_OF_$$$)

                new Lookahead(
                    action :: route.actions,
                    f_qNew,
                    f_QPrimeNew,
                    utility,
                    curBalNew)
            }
            anotherLayer ::: newLookaheadList
        }
        else newLookaheadList
    }

    def choose_action() {
        val artifactUtility    = utility_of_submitting()
        val voteUtility        = utility_of_voting()
        val improvementUtility = utility_of_improvement_job()

//        println(qstn.f_Q.particles.mkString("\nParticles:\n(", ", ", ")\n"))
        println("current balance     " + state.balance)
//        println("artifactUtility:    " + artifactUtility)
//        println("voteUtility:        " + voteUtility)
//        println("improvementUtility: " + improvementUtility)
        println("Predicted Original Utility:   " + convolute_Utility_with_Particles(state.f_Q))
        println("Predicted Prime Utility:      " + convolute_Utility_with_Particles(state.f_QPrime))

        if (improvementUtility > voteUtility
          && improvementUtility > artifactUtility
          && state.balance > IMPROVEMENT_COST)
        {
            println("\n=> | IMPROVEMENT job |")
            improvement_job()
        }
        else if (voteUtility > artifactUtility
               && state.balance > BALLOT_COST)
        {
            println("\n=> | BALLOT job |")
            ballot_job()
        }
        else submit_final()

        println("\n\n****************************************************")
    }
}

case class Lookahead(actions:    List[String],
                     f_Q:        PF,
                     f_QPrime:   PF,
                     utility:    Double,
                     curBalance: Double)

object Test_choose_action extends App { while(true) FirstExperiment.qstn.choose_action() }
object Test_look_ahead    extends App { while(true) FirstExperiment.qstn.look_ahead()    }

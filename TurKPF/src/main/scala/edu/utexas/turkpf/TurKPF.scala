/**
 * Adapts TurKontrol into a Particle Filter
 * TurKontrol was presented in "Decision-Theoretic Control of Crowd-Sourced Workflows"
 * by Peng Dai, Mausam, and Daniel S. Weld (2010)
 *
 * Author:          Ethan Petuchowski
 * Date Started:    6/18/13
 * License:         Unknown
 */

// TODO: Write the actions to a file, like { 0,0,0,1,0,0,1 } where 0 = improvement, 1 = ballot
// then read it into an Excel and graph it at various levels of stuff

import math._
import org.apache.commons.math3.distribution.{BetaDistribution, NormalDistribution}
import scala.util.Random

/* this is so one can choose a set of parameters by replacing this line with
 *  import SecondExperiment._  and so on  */
import FirstExperiment._


// implicitly add "normalize" to Array[Dbl] to make ||Array|| = 1
abstract class addNorm(a: Array[Double]) { def normalize: Array[Double] }

/* Particle Filter representation of artifact-quality Probability Density Functions */
case class QualityDistribution(numParticles: Int, particles: Array[Double]) {

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
    def predict: QualityDistribution = new QualityDistribution(
        particles map { q =>  // [DTC] § Experimental Setup
            val mu = find_improvementFunctionMean(q)
            new BetaDistribution(10 * mu, 10 * (1 - mu)).sample
    })

    /* fold ALL the partBs through prob_true() with EACH partA :  [DTC] (eq. 5-6) */
    def weight_and_sample(vote: Boolean, that: QualityDistribution, prime: Boolean):
    QualityDistribution = {
        // get P( b_{n+1} | q ) :  [DTC] (eq. 6) ; [TK=>PF] (eq. 8-9)
        val weights = {
            this.particles map { partA =>
                (0.0 /: that.particles)((sum, partB) => {
                    val (p1, p2) = if (prime) (partB, partA) else (partA, partB)
                    sum + qstn.invertIfFalse(vote, qstn.prob_true_given_Qs(p1, p2))
                })
            }}.normalize

        // get f_{ Q | b_{n+1} } (q) :  [DTC] (eq. 5)
        new QualityDistribution( particles map { _ => repopulate(weights) } )
    }

    /* algorithm for sampling from given set of points with associated weights:
     * generate a random number in [0,1), use the CDF of the weights to use the
     * random number to figure out what the sampled value is
     */
    def repopulate(weights: Array[Double]): Double = {
        val rand = Random.nextDouble
        var accrue = 0.0
        for ((weight, index) <- weights.zipWithIndex) {
            accrue += weight
            if (rand < accrue)
                return particles(index)
        }
        throw new RuntimeException // shouldn't ever get here
        particles(particles.length-1)
    }

    implicit def addNorm(a: Array[Double]): addNorm = new addNorm(a) {
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
    var f_Q = new QualityDistribution  // defaults to BetaDist(1,9)

    // [DTC] § Experimental Setup
    // my goal here is to initialize the qPrime prior a little higher than the q prior
    // a better way to do this would be nice
    var f_QPrime = new QualityDistribution(
        new BetaDistribution(2,9).sample(NUM_PARTICLES)
    )
}

case class Question() {

    val state = QuestionState()

    // [DTC] trueGX > 0; code is worker_dist-agnostic
    val wrkrs = Workers(state.workerTrueGm)



    def artifact_difficulty: Double = difficulty(state.qlty, state.qltyPrime)

    // [DTC] (eq. 2)
    def difficulty(qlty: Double, qltyPrime: Double): Double =
        1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)

    def artifact_utility(f_Q: QualityDistribution): Double =
        convolute_Utility_with_Particles(f_Q)

    def artifact_utility: Double = artifact_utility(state.f_Q)

    // the math for this checks out
    def convolute_Utility_with_Particles(dist: QualityDistribution): Double =
        (0.0 /: dist.particles)(_ + estimate_artifact_utility(_)) / NUM_PARTICLES

    // [DTC] (eq. 12)
    def dStar: Double = {
        (0.0 /: state.f_Q.particles)((sum, q) =>
            sum + (0.0 /: state.f_QPrime.particles)((sum2, qPrime) =>
                sum2 + difficulty(q, state.qltyPrime)
            )
        ) / (NUM_PARTICLES * NUM_PARTICLES)
    }

    // TODO: The way I treat utility throughout this file may be inconsistent!
    def final_utility(f_Q: QualityDistribution, f_QPrime: QualityDistribution, balance: Double)

    def submit_final(f_Q_of_q: QualityDistribution, f_Q_of_QPrime: QualityDistribution, balance: Double):
    (QualityDistribution, QualityDistribution, Double) = {

    }

    def submit_final() = {
        println("Final Utility: " + (artifact_utility + state.balance * UTILITY_OF_$$$))
        sys exit 0
    }

    // [DTC] (top-right of page 4)
    def utility_of_improvement_job: Double = {
        val (orig_predicted, prime_predicted) = utility_of_stopping_voting
        max(orig_predicted, prime_predicted) - IMPROVEMENT_COST * UTILITY_OF_$$$
    }
        max(utility_of_stopping_voting) - IMPROVEMENT_COST * UTILITY_OF_$$$

    def utility_of_improvement_job(
        f_Q: QualityDistribution, f_QPrime: QualityDistribution): Double = {
        val (orig_predicted, prime_predicted) = utility_of_stopping_voting(state.f_Q, state.f_QPrime)
        max(orig_predicted, prime_predicted) - IMPROVEMENT_COST * UTILITY_OF_$$$
    }

    // [DTC] (eq. 9)
    def utility_of_stopping_voting(f_Q, f_QPrime): (Double, Double) = {
        val orig_predicted = convolute_Utility_with_Particles(state.f_Q.predict)
        val prime_predicted = convolute_Utility_with_Particles(f_QPrime.predict)
        (orig_predicted, prime_predicted)
    }

    def utility_of_stopping_voting: (Double, Double) = {
        val (orig_predicted, prime_predicted) = utility_of_stopping_voting(state.f_Q, state.f_QPrime)
        println("Predicted Original Utility:   " + orig_predicted)
        println("Predicted Prime Utility:      " + prime_predicted)
        (orig_predicted, prime_predicted)
    }

    /***************************** BALLOT JOB STUFF *******************************/
    /* [DTC] (bottom-left Pg. 4)
     * PSEUDOCODE for calculating E[ U( Q | b_{n} + 1 ) ]:
     * Then, For each particle in the result of performing (eq. 5)
     *   For each vote outcome \in { 0, 1 }
     *     Multiply U(q) * particle.q * P(b_{n+1} = {0,1})
     */
    def utility_of_voting: Double = {
        val probYes = probability_of_yes_vote
        println("probYes: " + probYes)
        max(
            expVal_after_a_vote(dist_Q_after_vote, probYes),
            expVal_after_a_vote(dist_QPrime_after_vote, probYes)
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
    def probability_of_yes_vote(f_Q: QualityDistribution, f_QPrime: QualityDistribution) = {
        (0.0 /: f_Q.particles)((sumA, particleA) =>
            sumA + particleA * (0.0 /: f_QPrime.particles)((sumB, particleB) =>
                sumB + prob_true_given_Qs(particleA, particleB)
            ) / NUM_PARTICLES
        ) / NUM_PARTICLES

    }
    def probability_of_yes_vote = probability_of_yes_vote(state.f_Q, state.f_QPrime)

    // I checked and this function works properly
    def prob_true_given_Qs(q: Double, qPrime: Double): Double =
        qstn.invertIfFalse(q < qPrime, wrkrs.accuracy(qstn.difficulty(q, qPrime)))

    def expVal_after_a_vote(f: Boolean => QualityDistribution, probYes: Double): Double = {
        if (probYes > 1 || probYes < 0) throw new RuntimeException
        convolute_Utility_with_Particles(f(true)) * probYes +
        convolute_Utility_with_Particles(f(false)) * (1 - probYes)
    }

    // [DTC] (eq. 5)
    def dist_Q_after_vote(vote: Boolean): QualityDistribution =
        state.f_Q.weight_and_sample(vote, state.f_QPrime, false)

    // [DTC] (eq. 7-8) the same as above
    def dist_QPrime_after_vote(vote: Boolean): QualityDistribution =
        state.f_QPrime.weight_and_sample(vote, state.f_Q, true)


    // [DTC] (eqs. 4,5,6,7,8)
    def ballot_job(f_Q_of_q: QualityDistribution, f_Q_of_QPrime: QualityDistribution, balance: Double,
                   vote: Boolean = None):
    (QualityDistribution, QualityDistribution, Double) = {
        val theVote = vote match {
            case v if v == None => random < probability_of_yes_vote(f_Q_of_q, f_Q_of_QPrime)
            case v => v
        }
        (dist_Q_after_vote(theVote), dist_QPrime_after_vote(theVote), balance - BALLOT_COST)
    }

    /**
     * 1. Pay for it
     * 2. Generate vote & append to list
     * 3. Update distributions
     * 4. Return vote
     */
    def ballot_job(): Boolean = {

        val vote = random < probability_of_yes_vote
        printf("vote :: %s #L293\n\n", vote.toString.toUpperCase)
        state.votes ::= vote
        println(state.votes.mkString("(",", ",")"))
        (state.f_Q, state.f_QPrime, state.balance) =
          ballot_job(state.f_Q, state.f_QPrime, state.balance, vote)
        vote
    }


    def invertIfFalse(t: Boolean, v: Double): Double = if (t) v else 1-v
    /****************************** END BALLOT JOB STUFF **************************/

    /*
     * 1. pay for it
     * 2. choose to continue with alpha or alphaPrime [DTC top-right pg. 4]
     * 3. create new prior for quality of improved artifact
     */
    def improvement_job(f_Q_of_q: QualityDistribution, f_Q_of_QPrime: QualityDistribution, balance: Double):
    (QualityDistribution, QualityDistribution, Double) = {
        val newF_Q_of_q = {
            if (convolute_Utility_with_Particles(f_Q_of_q)
              < convolute_Utility_with_Particles(f_Q_of_QPrime))
                f_Q_of_QPrime
            else
                f_Q_of_q
        }
        (newF_Q_of_q, newF_Q_of_q.predict, balance - IMPROVEMENT_COST)
    }

    def improvement_job() {
        // clear votes out
        wrkrs.updateGX(state.votes)
        state.votes = List[Boolean]()
        (state.f_Q, state.f_QPrime, state.balance) =
          improvement_job(state.f_Q, state.f_QPrime, state.balance)
    }

    /** TODO: Here's how the LookAhead might be implemented:
      * GoDeeper( DataStruct, maxDepth, currentDepth ):
      *   For Tuple in DataStruct:
      *     If Tuple doesn't end in a "submit":
      *       Replace that tuple with 3 new ones, 1 for each action in [b,i,s], calculating the score
      * Sort the Tuples by score and perform actionList.head.
      */
    def look_ahead(lookaheadList: List[Lookahead], currentDepth: Int) {

        // all done, return the first action from the highest performing sequence of actions:
        if (currentDepth == LOOKAHEAD_DEPTH) {
            lookaheadList.sortWith(_.utility > _.utility).head.actions.head match {
                case "improve" => improvement_job()
                case "ballot" => ballot_job()
                case "submit" => submit_final()
                case _ => throw new RuntimeException
            }
        }

        var newLookaheadList = List[Lookahead]
        for (look <- lookaheadList) {
            if (look.actions.head != "submit") {
                for (action <- List("improve", "ballot", "submit")) {
                    val (f_qNew, f_QPrimeNew, curBalNew):
                    (QualityDistribution, QualityDistribution, Double, Double) =  action match {
                        case "improve" => improvement_job(look.f_q, look.f_QPrime, look.curBalance)
                        case "ballot" => ballot_job(look.f_q, look.f_QPrime, look.curBalance)
                        case "submit" => submit_final(look.f_q, look.f_QPrime, look.curBalance)
                        case _ => throw new RuntimeException
                    }
                    val utility: Double = action match {
                        case "improve" => utility_of_improvement_job(look.f_Q, look.f_QPrime)
                        case "ballot" => utility_of_voting(look.f_Q, look.f_QPrime)
                        case "submit" => artifact_utility(look.f_Q, look.f_QPrime)
                    }
                    newLookaheadList ::= new Lookahead(action :: look.actions, f_qNew, f_QPrimeNew, utility, curBalNew)
                }
            }
        }
    }

    def choose_action() {
        val artifactUtility    = artifact_utility
        val voteUtility        = utility_of_voting
        val improvementUtility = utility_of_improvement_job

//        println(qstn.f_Q_of_q.particles.mkString("\nParticles:\n(", ", ", ")\n"))
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

case class Lookahead(actions: List[String],
                     f_q: QualityDistribution, f_QPrime: QualityDistribution,
                     utility: Double,
                     curBalance: Double)

object FirstExperiment {

    /* [DTC] gmX "follow a bell shaped distribution"
     *           "average error coefficient gm=1",
     *             where gmX > 0 */
    val WORKER_DIST = new NormalDistribution(1,0.2)

    /* this is a method so that it can be set to generate a random
     * initial quality every time a question starts */
    def INITIAL_QUALITY = .01 // new BetaDistribution(1,9).sample

    // [DTC] § Experimental Setup
    def estimate_artifact_utility(qlty: Double): Double =
        1000 * (exp(qlty) - 1) / (exp(1) - 1)

    val IMPROVEMENT_COST    = 3
    val BALLOT_COST         = .75
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 3  /* TODO: THE LookAhead */
    val NUM_QUESTIONS       = 10000
    val INITIAL_ALLOWANCE   = 10.0
    val NUM_PARTICLES       = 1000
    val UTILITY_OF_$$$      = 1.0  // let's just say it's "1" for simplicity

    val qstn = Question()
}

object TestStuff extends App { while(true) FirstExperiment.qstn.choose_action() }

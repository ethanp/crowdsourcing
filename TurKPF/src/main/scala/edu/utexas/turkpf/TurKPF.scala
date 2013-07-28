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
import org.apache.commons.math3.distribution.{BetaDistribution, NormalDistribution}
import scala.util.Random

/* this means one can choose a set of parameters by replacing this line with
 *  import SecondExperiment._  and so on  */
import FirstExperiment._

/* TODO: the LookAhead */
/* TODO: it shouldn't let you spend more than your allowance */

/* Particle Filter representation of artifact quality probability density functions */
case class QualityDistribution(numParticles: Int, particles: Array[Double])
{
    def this(n: Int) = this(n, new BetaDistribution(1,9).sample(n))

    def this(particles: Array[Double]) = this(NUM_PARTICLES, particles)

    def this() = this(NUM_PARTICLES, new BetaDistribution(1,9).sample(NUM_PARTICLES))

    // [DTC] (eq. 13)
    def find_improvementFunctionMean(qlty: Double): Double = {
        val accuracy: Double = qstn.wrkrs.accuracy(qlty)
        qlty + 0.5 * ((1 - qlty) * (accuracy - 0.5) + qlty * (accuracy - 1))
    }

    // [DTC] § Experimental Setup
    def improvementDistr(qlty: Double) = {
        val mu = find_improvementFunctionMean(qlty)
        new BetaDistribution(10 * mu, 10 * (1 - mu))
    }

    // [DTC] (eq. 1), q => generate f_{ Q' | particle.q } (q') and sample from it
    /*
     * THE WAY THIS WORKS:
     * We go through each particle, and using its value (which is an estimate of the qlty),
     * we generate an "improvement distribution" describing how much we can expect [an avg.]
     * worker to improve the quality of the artifact. We take a "random" stab at the new
     * quality after the "improvement job" by sampling from the "improvement distribution".
     */
    // TODO: This might be a faster way to implement this: (generating directly, not sampling)
    // http://doodleproject.sourceforge.net/numerics/numerics4j/1.3/api/net/sf/doodleproject/numerics4j/random/BetaRandomVariable.html
    def predict: QualityDistribution =
        new QualityDistribution(particles map {improvementDistr(_).sample})

    /* fold ALL the partBs through prob_true() with EACH partA
     * [DTC] (eq. 5-6)
     */
    def weight_and_sample(vote: Boolean, that: QualityDistribution):
    QualityDistribution = {

        // get P( b_{n+1} | q ) :  [DTC] (eq. 6)
        val rawWeights = this.particles map { partA =>
            (0.0 /: that.particles)((sum, partB) =>
                sum + qstn.invertIfFalse(vote, qstn.prob_true_given_Qs(partA, partB)))
        }

        // get f_{ Q | b_{n+1} } (q) :  [DTC] (eq. 5)
        val weightNorm = rawWeights.sum
        val weights = rawWeights map { _ / weightNorm } // normalize weights, works
        QualityDistribution(NUM_PARTICLES,
            (1 to NUM_PARTICLES).toArray map {
                _ => random_sample_given_weights(weights)
            }
        )
    }

    /* algorithm for sampling from given set of points with associated weights:
     * generate a random number in [0,1), use the CDF of the weights to use the
     * random number to figure out what the sampled value is
     *
     * I debugged this function and it works as expected
     */
    def random_sample_given_weights(weights: Array[Double]): Double = {
        val rand = Random.nextDouble
        var accrue = 0.0
        for ((weight, index) <- weights.zipWithIndex) {
            accrue += weight
            if (rand < accrue)
                return this.particles(index)
        }
        throw new RuntimeException // shouldn't ever get here
        particles(particles.length-1)
    }

    // avg loc of particles in associated Particle Filter
    // this doesn't actually make an appearance in the algorithm,
    // it's just for debugging
    def meanQltyEst: Double = particles.sum / NUM_PARTICLES
}

/* model all workers with just one worker-independent model */
case class Workers(trueGX: Double)
{
    val LEARNING_RATE = 0.05
    var estGX: Double = 1    // set to the mean of the true distribution
                             // could be altered to test robustness

    // [DTC] (eq. 3)
    def generateVote(difficulty: Double): Boolean = random < accuracy(difficulty)

    // [DTC] (above eq. 3)
    def accuracy(difficulty: Double) = 0.5 * (1 + pow(1 - difficulty, estGX))

    /* I'm not sure about my whole "bigger/smaller" logic it was a guess...
     *  plus there HAS to be a less unsightly way of accomplishing the same thing */
    // higher GX means Worse worker
    def updateGX(votes: List[Boolean]) {    // [DTC] (below eq. 12)
        val (trues, falses) = votes.partition(_ == true)
        val trueBigger = trues.length > falses.length
        val bigger  = if (trueBigger) trues.length  else falses.length
        val smaller = if (trueBigger) falses.length else trues.length
        val d = qstn.artifact_difficulty
        estGX -= bigger * d * LEARNING_RATE
        estGX += smaller * (1 - d) * LEARNING_RATE
    }
}

case class Question(trueAnswer: Boolean)
{
    var balance = INITIAL_ALLOWANCE
    var qlty = INITIAL_QUALITY
    var qltyPrime = 0.0

    // [DTC] trueGX > 0; code is worker_dist-agnostic
    var workerTrueGm = WORKER_DIST.sample
    while (workerTrueGm < 0) workerTrueGm = WORKER_DIST.sample

    val wrkrs = Workers(workerTrueGm)

    // [DTC] § Experimental Setup
    var f_Q_of_q = new QualityDistribution  // defaults to BetaDist(1,9)

    // my goal here is to initialize the qPrime prior a little higher than the q prior
    // a better way to do this would be nice
    var f_Q_of_qPrime = new QualityDistribution(
        new BetaDistribution(2,9).sample(NUM_PARTICLES)
    )

    def artifact_difficulty: Double = difficulty(qlty, qltyPrime)

    // [DTC] (eq. 2)
    def difficulty(qlty: Double, qltyPrime: Double): Double =
        1 - pow((qlty - qltyPrime).abs, DIFFICULTY_CONSTANT)

    def artifact_utility: Double =
        convolute_Utility_with_Particles(f_Q_of_q)

    // the math for this checks out
    def convolute_Utility_with_Particles(dist: QualityDistribution): Double =
        (0.0 /: dist.particles)(_ + estimate_artifact_utility(_)) / NUM_PARTICLES

    // [DTC] (eq. 12)
    // this is O(numParticles^2)...they also note that this equation takes a while
    // TODO: Why is this never used? What is this thing for anyway?
    def dStar: Double = {
        val normQ = f_Q_of_q.particles.sum
        val normQP = f_Q_of_qPrime.particles.sum
        (0.0 /: f_Q_of_q.particles)((sum, q) =>
            sum + (0.0 /: f_Q_of_qPrime.particles)((sum2, qPrime) =>
                sum2 + q * qPrime * difficulty(q, qPrime) / normQ
            ) / normQP
        )
    }

    def submit_final() = {
        println("Final Utility: " + artifact_utility)
        sys.exit(0)
    }

    // [DTC] (top-right of page 4)
    def utility_of_improvement_job: Double =
        utility_of_stopping_voting - IMPROVEMENT_COST * UTILITY_OF_$$$

    // [DTC] (eq. 9)
    def utility_of_stopping_voting: Double = { max(
        convolute_Utility_with_Particles(f_Q_of_q),  // [DTC] (eq. 10)
        convolute_Utility_with_Particles(f_Q_of_qPrime)  // [DTC] (eq. 11)
    )}

    /***************************** BALLOT JOB STUFF *******************************/
    // [DTC] (bottom-left Pg. 4)
    def utility_of_voting: Double = {
        val probYes = probability_of_yes_vote
        println("probYes: " + probYes)
        max(
            expVal_OLD_artifact_with_addnl_vote(probYes),
            expVal_NEW_artifact_with_addnl_vote(probYes)
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
    def probability_of_yes_vote = {
        (0.0 /: f_Q_of_q.particles)((sumA, particleA) =>
            sumA + particleA * (0.0 /: f_Q_of_qPrime.particles)((sumB, particleB) =>

                // TODO check that multiplying particleB in makes sense
                sumB + prob_true_given_Qs(particleA, particleB) * particleB
            ) / f_Q_of_qPrime.particles.sum
        ) / f_Q_of_q.particles.sum
    }

    // I checked and this function works properly
    def prob_true_given_Qs(q: Double, qPrime: Double): Double =
        qstn.invertIfFalse(q < qPrime, wrkrs.accuracy(qstn.difficulty(q, qPrime)))

    /* [DTC] (bottom-left Pg. 4)
     * PSEUDOCODE for calculating E[ U( Q | b_{n} + 1 ) ]:
     * Then, For each particle in the result of performing (eq. 5)
     *   For each vote outcome \in { 0, 1 }
     *     Multiply U(q) * particle.q * P(b_{n+1} = {0,1})
     */
    def expVal_OLD_artifact_with_addnl_vote(probYes: Double): Double =
        expVal_after_a_vote(dist_Q_after_vote, probYes)

    // E[ U( Q' | b_{n} + 1 ) ]; the same thing as above
    def expVal_NEW_artifact_with_addnl_vote(probYes: Double) =
        expVal_after_a_vote(dist_QPrime_after_vote, probYes)

    def expVal_after_a_vote(f: Boolean => QualityDistribution, probYes: Double): Double = {
        if (probYes > 1 || probYes < 0) throw new RuntimeException
        convolute_Utility_with_Particles(f(true)) * probYes +
        convolute_Utility_with_Particles(f(false)) * (1 - probYes)
    }

    // [DTC] (eq. 5)
    /* What this Does:
     * Creates a posterior distribution (estimate) of the quality of the artifact
     *  given one more ballot
     */
    def dist_Q_after_vote(vote: Boolean): QualityDistribution =
        f_Q_of_q.weight_and_sample(vote, f_Q_of_qPrime)

    /* [DTC] (eq. 7-8) the same as above, but the order in
     *   which the distributions are used is switched
     */
    def dist_QPrime_after_vote(vote: Boolean): QualityDistribution =
        f_Q_of_qPrime.weight_and_sample(vote, f_Q_of_q)

    def get_addnl_ballot_and_update_dists(): Boolean = {
        balance -= BALLOT_COST  // pay for it
        val vote: Boolean = wrkrs.generateVote(artifact_difficulty)
        printf("vote :: %s #L293\n\n", vote.toString.map(_.toUpper))
        f_Q_of_q      = dist_Q_after_vote(vote)  // [DTC] (eqs. 4,5,6,7,8)
        f_Q_of_qPrime = dist_QPrime_after_vote(vote)
        votes ::= vote
        print("(" + votes.mkString(", ") + ")\n")
        vote
    }

    var votes = List[Boolean]()

    def invertIfFalse(t: Boolean, v: Double): Double = if (t) v else 1-v
    /****************************** END BALLOT JOB STUFF **************************/

    def improvement_job() {
        // pay for it
        balance -= IMPROVEMENT_COST

        // choose to continue with alpha or alphaPrime [DTC top-right pg. 4]
        if (convolute_Utility_with_Particles(f_Q_of_q)
          < convolute_Utility_with_Particles(f_Q_of_qPrime))
            f_Q_of_q = f_Q_of_qPrime

        wrkrs.updateGX(votes)

        // clear votes out
        votes = List[Boolean]()

        // create new prior for quality of improved artifact
        f_Q_of_qPrime = f_Q_of_q.predict
    }

    def choose_action() {
        val artifactUtility    = artifact_utility
        val voteUtility        = utility_of_voting
        val improvementUtility = utility_of_improvement_job

//        println("\nParticles:\n(" + qstn.f_Q_of_q.particles.mkString(", ") + ")\n")
        println("current balance     " + balance)
        println("artifactUtility:    " + artifactUtility)
        println("voteUtility:        " + voteUtility)
        println("improvementUtility: " + improvementUtility)
        println("meanQltyEst:        " + qstn.f_Q_of_q.meanQltyEst)
        println("PrimeMeanQltyEst:   " + qstn.f_Q_of_qPrime.meanQltyEst)

        if (improvementUtility > voteUtility
        && improvementUtility > artifactUtility
        && balance > IMPROVEMENT_COST)
        {
            println("\n=> | IMPROVEMENT job |")
            println("\n\n****************************************************")
            improvement_job()
        }
        else if (voteUtility > artifactUtility
                && balance > BALLOT_COST)
        {
            println("\n=> | BALLOT job |")
            get_addnl_ballot_and_update_dists()
            println("\n\n****************************************************")
        }
        else submit_final()
    }
}

object FirstExperiment
{
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

    val IMPROVEMENT_COST    = .05
    val BALLOT_COST         = .01
    val DIFFICULTY_CONSTANT = 0.5
    val LOOKAHEAD_DEPTH     = 3  // not using this at this point
    val NUM_QUESTIONS       = 10000
    val INITIAL_ALLOWANCE   = 10.0
    val NUM_PARTICLES       = 1000
    val UTILITY_OF_$$$      = .05  // let's just say it's "1" for simplicity

    /* so that MANY Questions can be run Per Experiment
     * I'ma try to get just one Question working first though */
    val qstn = Question(trueAnswer=true)
}

object TestStuff extends App { while(true) FirstExperiment.qstn.choose_action() }

// TODO Judging by this output it seems like I got the effect of a vote job wrong
/*
/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/bin/java -Didea.launcher.port=7536 "-Didea.launcher.bin.path=/Applications/IntelliJ IDEA 12 CE.app/bin" -Dfile.encoding=UTF-8 -classpath "/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/lib/ant-javafx.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/lib/dt.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/lib/javafx-doclet.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/lib/javafx-mx.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/lib/jconsole.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/lib/sa-jdi.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/lib/tools.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/charsets.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/jce.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/jfr.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/jfxrt.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/JObjC.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/jsse.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/management-agent.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/resources.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/rt.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/ext/dnsns.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/ext/localedata.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/ext/sunec.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/ext/sunjce_provider.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/ext/sunpkcs11.jar:/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home/jre/lib/ext/zipfs.jar:/Users/Ethan/Dropbox/MLease/git/TurKPF/target/scala-2.10/classes:/Users/Ethan/.sbt/boot/scala-2.10.2/lib/scala-library.jar:/Users/Ethan/.ivy2/cache/org.apache.commons/commons-math3/jars/commons-math3-3.2.jar:/Applications/IntelliJ IDEA 12 CE.app/lib/idea_rt.jar" com.intellij.rt.execution.application.AppMain TestStuff
probYes: 0.5652923690663553
current balance     10.0
Average:            0.10151655349996495
artifactUtility:    64.97640576587854
voteUtility:        113.74640375175498
improvementUtility: 119.0314423092954
meanQltyEst:        0.10151655349996495
PrimeMeanQltyEst:   0.18013564994996462

=> | IMPROVEMENT job |


********************************************
probYes: 0.6519327142998282
current balance     9.95
Average:            0.18013564994996462
artifactUtility:    119.0339423092954
voteUtility:        226.54502317148973
improvementUtility: 244.87009584697512
meanQltyEst:        0.18013564994996462
PrimeMeanQltyEst:   0.33941549019172357

=> | IMPROVEMENT job |


********************************************
probYes: 0.5699940078478649
current balance     9.899999999999999
Average:            0.33941549019172357
artifactUtility:    244.87259584697512
voteUtility:        306.2686164038555
improvementUtility: 317.4977762519782
meanQltyEst:        0.33941549019172357
PrimeMeanQltyEst:   0.4219825641731508

=> | IMPROVEMENT job |


********************************************
probYes: 0.5361471903273249
current balance     9.849999999999998
Average:            0.4219825641731508
artifactUtility:    317.5002762519782
voteUtility:        345.1513561515167
improvementUtility: 352.9802559953438
meanQltyEst:        0.4219825641731508
PrimeMeanQltyEst:   0.45897610071240313

=> | IMPROVEMENT job |


********************************************
probYes: 0.5157132396291504
current balance     9.799999999999997
Average:            0.45897610071240313
artifactUtility:    352.9827559953438
voteUtility:        374.0945590534782
improvementUtility: 374.47282612041465
meanQltyEst:        0.45897610071240313
PrimeMeanQltyEst:   0.48210174578291526

=> | IMPROVEMENT job |


********************************************
probYes: 0.5000715733514703
current balance     9.749999999999996
Average:            0.48210174578291526
artifactUtility:    374.47532612041465
voteUtility:        375.2511406270203
improvementUtility: 374.47282612041465
meanQltyEst:        0.48210174578291526
PrimeMeanQltyEst:   0.4810626544719478

=> | BALLOT job |
vote :: TRUE #L293

(true)


********************************************
probYes: 0.4984127794342908
current balance     9.739999999999997
Average:            0.43150336671449346
artifactUtility:    325.9390272221734
voteUtility:        324.0672429134455
improvementUtility: 325.9365272221734
meanQltyEst:        0.43150336671449346
PrimeMeanQltyEst:   0.42730866809138157
Final Utility: 325.9390272221734

Process finished with exit code 0

*/

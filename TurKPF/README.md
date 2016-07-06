## TURKPF: TURKONTROL as a Particle Filter
##### by Ethan Petuchowski, supervised by Matthew Lease and Aashish Sheshadri

### [Find the Paper about TurKPF on arxiv](http://arxiv.org/abs/1404.5078)

```
"Decision-Theoretic Control of Crowd-Sourced Workflows"
by Peng Dai, Mausam, and Daniel S. Weld (2010).
```
###Abstract

TURKONTROL, an algorithm presented in (Dai et al. 2010), uses a POMDP to model
and control an iterative workflow for crowdsourced work. Here, TURKONTROL is
implemented  using a Particle Filter to reduce the computation time & memory
necessary to run it.

###Conclusion

**TURKPF is not fit for use in the real world.** One would have to use Machine
Learning on *real data* to determine appropriate values and functions for all
of the default parameters and equations. Methods for doing this were described
in [`(Dai et al. 2013)`](http://www.sciencedirect.com/science/article/pii/S000437021300057X).
One would also have to connect to the [`Amazon Mechanical Turk
API`](http://aws.amazon.com/documentation/mturk/). Other than that, TURKPF is a
functioning autonomous agent for allocating tasks to crowdworkers in an
iterative improvement workflow. Iterative improvement is very flexible, for
example it can be used in a Map-Reduce job-flow in either a Map or a Reduce
task.

The reason to use a particle filter in this situation is for its speed
advantages. Being able to choose the number of particles to use is a way to
name your desired speed. With the model specifics we used, as described in the
tables, an action is always chosen in much less than one second, which is
insignificant compared to the time it takes for a crowdworker to come across,
accept, and complete a task.

**[Link to the paper about TurKontrol on which this work is based](https://homes.cs.washington.edu/~mausam/papers/aaai10b.pdf)**

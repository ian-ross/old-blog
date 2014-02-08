---
author: Ian
tags: AI
title: The Storage Capacity of Neural Systems
published: 2014-02-08 22:17:02
---

I recently read *The Quest for Artificial Intelligence* by Nils
Nilsson.  Interrupting a fairly linear history of the AI field is an
interlude on some more philosophical questions about artificial
intelligence, minds and thought.  Searle's Chinese Room, things like
that.

I got to thinking about some of these questions on my daily
peregrinations with Winnie, and I started wondering about the scales
that are involved in most discussions of minds and AI.  Searle talks
about an individual person in his Chinese Room, which makes the idea
of some sort of disembodied intelligence actually understanding the
Chinese sentences it's responding to seem pretty absurd.  But is that
in any sense a realistic representation of a brain or a human mind?

In keeping with my upbringing as a baby physicist, I'm going to take a
very reductionist approach to this question.  I want to get some idea
of exactly what the information storage capacity of a human brain is.
I'm deliberately not going to think about information processing
speeds (because that would involve too much thinking about switching
rates, communication between different parts of brains, and other
biological things about which I know very little).  I'll treat this in
the spirit of a Fermi problem, which means I'll be horrifyingly
slapdash with anything other than powers of ten.  There will be
[big numbers][big].

<!--MORE-->

Where to start?  Brains are made of neurons.  In humans, the cerebral
cortex (the part of the brain involved of what we normally consider
"thinking" rather than "substrate maintenance tasks" -- keeping your
heart beating, and so on) has something like 15-30 billion neurons.
Let's call it 10 billion, for the sake of simplicity.  Using the
"cubes of water" visualisation technique from the
[earlier article](/blog/posts/2013/11/11/visualising-large-numbers/index.html),
this is a quite accessible number.  Think of ten one metre cubes of
water laid out in a line.  Each edge of each cube is graduated in
millimetres.  There are one billion one millimetre cubes in each one
metre cube of water, so there are ten billion one millimetre cubes in
all.

That's a lot of little cubes, and a lot of neurons, but it doesn't
seem so many when you think about it.  We have this idea that human
intellect is a nearly infinite ocean of possibility[^1], but a current
desktop system has far more than 10 gigabytes of memory, i.e. 10
billion 8-bit bytes.  However, we don't expect a computer to be able
to simulate a whole brain.  This comes down, of course, first, to
neurons being "squishy" and second, to the fact that the state of the
brain may be encoded more in the *connections* between neurons than in
the neurons themselves.  The "squishiness" of neurons means that the
state of an individual neuron requires much more than one bit (or
eight bits) to describe.  How many bits?  Let's think about that in a
minute.  Connections between neurons are called synapses, and there
are (to an order of magnitude) about a thousand times as many synapses
as neurons, so about 10 trillion, 10<sup>13</sup>.  That's quite a big
number.

What about individual neurons?  It's hard to say what aspects of the
state of individual neurons are important for information storage, but
if neural state *is* encoded in neurons, and we could encode the state
of an individual neuron in perhaps 1000 bits, then we'd need about
10<sup>13</sup> bits to encode a full brain state.  If synapses are
important and we need 1000 bits to encode the state of each synapse,
then we're looking at perhaps 10<sup>16</sup> bits per brain state.
These numbers are almost certainly very loose lower bounds!

Let's come at things from the other direction for a moment.  There is
(as far as we know) a fundamental limit of the amount of information
you can squeeze into a volume of space-time, given by something called
the Bekenstein bound.  The possible number of quantum states is a
region of space-time, $I$, measured in bits, is bounded by

$$I \leq \frac{2 \pi R E}{\hbar c \log 2}$$

where $E$ is the energy contained in the volume (including rest mass)
and $R$ is the radius of a sphere bounding the region.  Let's
approximate a human brain by a sphere of water of mass 1.5 kg.
Writing the volume and density of this sphere as $V$ and $\rho$
respectively, this gives us:

$$I \leq \frac{2 \pi M c}{\hbar \log 2}
    \left(\frac{3 M}{4 \pi rho}\right)^{1/3}$$

Putting $M = 1.5\,\mathrm{kg}$, $\rho =
1000\,\mathrm{kg}\,\mathrm{m}^{-3}$, we find $I \leq 2.75 \times
10^{42}$.  So *that*'s a *big* number.  Much bigger than any numbers
we tried to visualise in that earlier article.  Now, we have no reason
to expect that the information encoding capacity of biological systems
even begins to approach these limits.  The Bekenstein bound tells you
(more or less) how much information you can fit into a region of
space-time before you start spawning baby black holes, and no-one
suggests that biological systems get close to this.  It's an upper
bound for some theoretical highly advanced civilisation that can
manipulate quantum states directly, which we can't really do yet.

Here are some more down to earth questions.  How many atoms are there
in a brain?  How many molecules?  How many functionally meaningful
protein molecules?  Atoms are probably not all that relevant, but we
can get an idea of how many molecules are in a brain by taking a
weight of 1.5 kg again and assuming it's all water (slapdash,
remember?): 1.5 kg of water is about 100 moles of water (the molecular
mass of water is 14), so we have about 100 Avogadros of molecules or
around 10<sup>26</sup> molecules, give or take a couple of factors of
10.  This is also a big number, but is visualisable with a bit of work
(go and look at the picture of a sports stadium with a big cube of
water in [this article][big] and think about it a bit).

So, we have (more or less):

 * 10<sup>10</sup> neurons
 * 10<sup>13</sup> synapses
 * 10<sup>26</sup> molecules
 * 10<sup>42</sup> quantum states

I think it's fair to say that the storage capacity of a brain lies
somewhere in the middle of this.  In terms of bits, it's probably
somewhere between the number of synapses and the number of molecules.
If we split the difference, it seems reasonable to say that the
storage capacity of a human brain is unlikely to be *much* less or
*much* more (in terms of powers of 10) than 10<sup>20</sup> bits.

What does this tell us about my original musings about the scale
issues in AI?  If you're the poor person locked away in Searle's
Chinese room, following the rules to process Chinese messages, how
many bits can you hold in your head at a time?  Maybe a few hundred in
short term memory if you concentrate.  Some thousands if you're
allowed to write things down.  That doesn't compare too favourably
with the intrinsic storage capacity of a brain -- it's off by a factor
of something like 10<sup>15</sup>.

A glass of water is not the Pacific Ocean.

And now I'm going to head off to read Seth Lloyd's paper
"[Computational capacity of the universe](http://arxiv.org/abs/quant-ph/0110141)",
which has some really really REALLY big numbers in it...

[big]: /blog/posts/2013/11/11/visualising-large-numbers/index.html

[^1]: Of course, the word "nearly" is doing rather a lot of work in
      that sentence...

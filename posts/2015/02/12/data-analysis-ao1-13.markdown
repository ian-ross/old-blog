---
author: Ian
title: Non-diffusive atmospheric flow #13: Markov matrix examples
tags: data-analysis
published: 2015-02-12 14:57:45
---

(There's no code in this post, just some examples to explain what
we're going to do next.)

Suppose we define the state of the system whose evolution we want to
study by a probability vector $\mathbf{p}(t)$ -- at any moment in
time, we have a probability distribution over a finite partition of
the state space of the system (so that if we partition the state space
into $N$ components, then $\mathbf{p}(t) \in \mathbb{R}^N$).
Evolution of the system as a Markov chain is then defined by the
evolution rule

$$ \mathbf{p}(t + \Delta{}t) = \mathbf{M} \mathbf{p}(t), \qquad (1) $$

where $\mathbf{M} \in \mathbb{R}^{N \times N}$ is a *Markov matrix*.
This approach to modelling the evolution of probability densities has
the benefit both of being simple to understand and to implement (in
terms of estimating the matrix $\mathbf{M}$ from data) and, as we'll
see, of allowing us to distinguish between random "diffusive"
evolution and conservative "non-diffusive" dynamics.

We'll see how this works by examining a very simple example.

<!--MORE-->

First, there are a couple of properties that the matrix $\mathbf{M}$
has to satisfy in order to ensure that the evolution of
$\mathbf{p}(t)$ according to $(1)$ is consistent with $\mathbf{p}(t)$
being a probability distribution[^1].  If we rewrite $(1)$ as

$$ p_i(t + \Delta{}t) = \sum_j M_{ij} p_j(t), $$

then, writing the property that a vector $\mathbf{x}$ represents a
probability distribution as $\mathcal{P}[\mathbf{x}]$ (i.e. $x_i \geq
0$ and $\sum_i x_i = 1$), the condition that
$\mathcal{P}[\mathbf{p}(t)] \Rightarrow
\mathcal{P}[\mathbf{p}(t + \Delta{}t)]$ requires that

$$ M_{ij} \geq 0 \text{ for all $i$, $j$} \qquad \sum_i M_{ij} = 1
  \text{ for all $j$} \qquad \sum_j M_{ij} = 1 \text{ for all $i$}. $$

A matrix satisfying these conditions is called a *doubly stochastic
matrix*.

We'll see later how we can estimate Markov matrices from data, but for
now we're going to look at a very simple but interesting decomposition
of Markov matrix that will give us some insight into the type of
dynamics that our system exhibits.  All we do is split our Markov
matrix $\mathbf{M}$ into its symmetric and antisymmetric parts:

$$ \mathbf{M}^S = \frac12 (\mathbf{M} + \mathbf{M}^T) \qquad
  \mathbf{M}^A = \frac12 (\mathbf{M} - \mathbf{M}^T) $$

We'll use some simple examples to see how the symmetric component
picks out diffusive dynamics and the antisymmetric component
conservative dynamics.

For a first example, let's suppose we have partitioned the state space
of our system into four cells, and we have purely deterministic
dynamics, with a cycle of transitions between the cells in the
partition that goes as $1 \to 2 \to 3 \to 4 \to 1$.  This is
represented by a Markov matrix $\mathbf{M}_1$:

$$ \mathbf{M}_1 =
  \begin{pmatrix}
    0 & 0 & 0 & 1 \\
    1 & 0 & 0 & 0 \\
    0 & 1 & 0 & 0 \\
    0 & 0 & 1 & 0
  \end{pmatrix} $$

which has symmetric and asymmetric parts $\mathbf{M}_1^S$ and
$\mathbf{M}_1^A$:

$$ \mathbf{M}_1^S = \frac12
  \begin{pmatrix}
    0 & 1 & 0 & 1 \\
    1 & 0 & 1 & 0 \\
    0 & 1 & 0 & 1 \\
    1 & 0 & 1 & 0
  \end{pmatrix} \qquad
  \mathbf{M}_1^A = \frac12
  \begin{pmatrix}
    0 & -1 & 0 & 1 \\
    1 & 0 & -1 & 0 \\
    0 & 1 & 0 & -1 \\
    -1 & 0 & 1 & 0
  \end{pmatrix} $$

The matrix $\mathbf{M}_1^A + |\mathbf{M}_1^A|$ recovers the original
periodic motion:

$$ \mathbf{M}_1^A + |\mathbf{M}_1^A| =
  \begin{pmatrix}
    0 & 0 & 0 & 1 \\
    1 & 0 & 0 & 0 \\
    0 & 1 & 0 & 0 \\
    0 & 0 & 1 & 0
  \end{pmatrix}. $$

For the next example, let's use the same state space partitioning and
cycle of transitions, but let's say that there is a probability of 0.7
that the system follows the cycle at each time step, with equal
probabilities that it instead steps into any of the other "off-cycle"
states:

$$ \mathbf{M}_2 =
  \begin{pmatrix}
    0.1 & 0.1 & 0.1 & 0.7 \\
    0.7 & 0.1 & 0.1 & 0.1 \\
    0.1 & 0.7 & 0.1 & 0.1 \\
    0.1 & 0.1 & 0.7 & 0.1
  \end{pmatrix} $$

The symmetric and asymmetric parts of the Markov matrix are:

$$ \mathbf{M}_2^S = \frac12
  \begin{pmatrix}
    0.1 & 0.4 & 0.1 & 0.4 \\
    0.4 & 0.1 & 0.4 & 0.1 \\
    0.1 & 0.4 & 0.1 & 0.4 \\
    0.4 & 0.1 & 0.4 & 0.1
  \end{pmatrix} \qquad
  \mathbf{M}_2^A =
  \begin{pmatrix}
    0    & -0.3 & 0    & 0.3  \\
    0.3  & 0    & -0.3 & 0    \\
    0    & 0.3  & 0    & -0.3 \\
    -0.3 & 0    & 0.3  & 0
  \end{pmatrix} $$

and we find that

$$ \mathbf{M}_2^A + |\mathbf{M}_2^A| =
  \begin{pmatrix}
    0   & 0   & 0   & 0.6 \\
    0.6 & 0   & 0   & 0   \\
    0   & 0.6 & 0   & 0   \\
    0   & 0   & 0.6 & 0
  \end{pmatrix} $$

again recovering the non-diffusive part of the dynamics.

And for a third example, let's use the same state space partitioning
but purely random dynamics:

$$ \mathbf{M}_2 = \frac{1}{4}
  \begin{pmatrix}
    1 & 1 & 1 & 1 \\
    1 & 1 & 1 & 1 \\
    1 & 1 & 1 & 1 \\
    1 & 1 & 1 & 1
  \end{pmatrix} \qquad
  \mathbf{M}_3^S = \frac{1}{4}
  \begin{pmatrix}
    1 & 1 & 1 & 1 \\
    1 & 1 & 1 & 1 \\
    1 & 1 & 1 & 1 \\
    1 & 1 & 1 & 1
  \end{pmatrix} \qquad
  \mathbf{M}_3^A =
  \begin{pmatrix}
    0 & 0 & 0 & 0 \\
    0 & 0 & 0 & 0 \\
    0 & 0 & 0 & 0 \\
    0 & 0 & 0 & 0
  \end{pmatrix} $$

Here $\mathbf{M}_3^A + |\mathbf{M}_3^A| = 0$.

Comparing the three cases, we see that $\mathbf{M}_3 =
\mathbf{M}_3^S$, i.e. there is no antisymmetric component at all,
while $\mathbf{M}_1^A$ and $\mathbf{M}_2^A$ are both non-zero and seem
to represent the predictable cyclic dynamics present in the system:
the magnitude of the elements in $\mathbf{M}_2^A$ are smaller than
those in $\mathbf{M}_1^A$, reflecting the reduced predictability in
the second case because of the random diffusive element to the
dynamics.  In each case, the matrix $\mathbf{M}^A + |\mathbf{M}^A|$
extracts the non-diffusive part of the system dynamics.

Next, we're going to try to calculate Markov matrices to describe the
transitions between different regimes of atmospheric flow and use this
symmetric/antisymmetric decomposition (in particular the $\mathbf{M}^A
+ |\mathbf{M}^A|$ matrix) to detect predictable cycles of transitions.


[^1]: I'm not going to describe this stuff with any level of
      formality.  If you want the real skinny, read Crommelin's paper
      and the references within, R. A. Pasmanter & A. Timmermann
      (2003). Cyclic Markov chains with an application to an
      intermediate ENSO model. Nonlin. Processes Geophys. 10,
      197-210 &nbsp; [[PDF]](http://www.nonlin-processes-geophys.net/10/197/2003/npg-10-197-2003.pdf).

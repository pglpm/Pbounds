# Pinference

## An explanation of what this package is about

*[This explanation is taken from the [vignette](https://pglpm.github.io/Pinference/articles/inferP.html) accompanying the package.]*

The probability calculus is an extension of the calculus of propositional logic. Besides the *truth* or *falsity* of propositions, the probability calculus also considers a continuum of degrees of credibility -- probabilities -- between those two extremes. Just as propositional logic gives a set of rules to derive the truth or falsity of some propositions from the truth or falsity of others, so the probability calculus gives a set of rules to derive the probability for some propositions from the probability for others; these rules include the logical ones as a special case. (See [vignette](https://pglpm.github.io/Pinference/articles/inferP.html) for references.)

There are different [logical deduction systems](https://plato.stanford.edu/archives/spr2023/entries/natural-deduction/), each with many dialects, to express the rules of propositional logic. One of them is the [*sequent calculus*](https://encyclopediaofmath.org/wiki/Sequent_calculus). The probability calculus has many analogies with the sequent calculus.

In sequent calculus we express that proposition $a$ is true within a set of axioms $I$ by the notation

$$
I \vdash a
$$

The same is expressed in the probability calculus by the notation (note the reflection)

$$
\mathrm{P}(a \ \vert\  I) = 1
$$

but in this case we can also consider degrees of credibility different from $1$.

    

What about inferences?

In propositional logic, suppose we assert that:

- proposition $a$ is true given a set of axioms $I$,
- proposition $b \lor \lnot a$ is true given the set of axioms $I$ augmented with the proposition $c$.

Then we can also assert that $b$ is true given the set of axioms $c \land I$. This is a logical inference. In the notation of sequent calculus (Takeuti 1987) it is written as follows:

$$
\frac{
I \vdash a \quad\quad c \land I \vdash b \lor \lnot a
}{
c \land I \vdash b
}
$$

The conclusion follows from the initial assertions by the application of a set of inference rules.

In the probability calculus the same inference can be expressed as follows:

$$
\frac{
\mathrm{P}(a \ \vert\  I) = 1\quad\quad \mathrm{P}(b \lor \lnot a \ \vert\  c \land I) = 1
}{
\mathrm{P}(b \ \vert\  c \land I) = 1
}
$$

This final probability can be shown to follow from the initial ones by the well-known probability rules. Another example of a simple probability inference, which immediately follows by the probability rules, is

$$
\frac{
\mathrm{P}(a \ \vert\  I) = 0.3\quad\quad \mathrm{P}(b \ \vert\  a \land I) = 0.2
}{
\mathrm{P}(a \land b \ \vert\  I) = 0.06
}
$$


The probability rules effectively imply the rules of propositional logic as special cases.

What's remarkable is that **the probability rules allow us to algorithmically determine the *lower* and *upper* values that a probability can have, under the assertion of the values of other probabilities**. This algorithm is equivalent to solving a (fractional)-linear optimization problem. It is well-known, for instance, that if

$$
\mathrm{P}(a \ \vert\  I) = 0.2 \qquad
\mathrm{P}(b \ \vert\  I) = 0.7
$$

then the probability $\mathrm{P}(a \land b \ \vert\  I)$ cannot be larger than the minimum of the two above, that is,

$$
\mathrm{P}(a \land b \ \vert\  I) \in [0, 0.2]
$$

The present package offers the function [`inferP()`](https://pglpm.github.io/Pinference/reference/inferP.html), which implements this algorithm.

## Examples

Let's use `inferP()` for the simple example just discussed:

```
inferP(
  target = P(a & b  |  c),
  P(a  |  c) == 0.2,
  P(b  |  c) == 0.7
)
```
the answer we obtain is
```
min  max
0.0  0.2
```

Now let's check a classical logical inference, *modus ponens*. The symbol ` > ` stands for if-then $\Rightarrow$:
```
inferP(
  target = P(b | I),
  P(a > b | I) == 1,
  P(a | I) == 1
)

min max 
  1   1 
```


Other example: the probability of a proposition based on contradictory premises ($b \land \lnot b$) is undefined:
```
inferP(
  target = P(a  |  b & !b)
)

min max
 NA  NA
```


Let's check a variant of the [*cut rule*](https://ncatlab.org/nlab/show/cut+rule/) of sequent calculus:
```
inferP(
  target = P(X + Y | I & J),
  P(A & X | I) == 1,
  P(Y | A & J) == 1
)

min max 
  1   1 
```


More information about notation and constraints is available in the help function `help('inferP')`. More interesting examples, such as the *Monty Hall* problem, are given in the package's vignette.


## Installation
The package can be installed with
```
remotes::install_github('pglpm/Pinference')
```

It requires the [**lpSolve**](https://cran.r-project.org/package=lpSolve) package.



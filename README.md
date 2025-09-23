# Pinference

The package can be installed with
```
remotes::install_github('pglpm/Pinference')
```

It requires the [**lpSolve**](https://cran.r-project.org/package=lpSolve) package.

    

This R package offers a function, `inferP()`, that calculates the lower and upper bounds of the probability for a proposition conditional on another, given constraints on the conditional probabilities for other propositions.

As a trivial example, suppose we want to know the bounds of the probability

$$
\mathrm{P}(a \land b\  \vert\  c)
$$

under the assumption that

$$
\mathrm{P}(a\  \vert\  c) = 0.2  ,\enspace
\mathrm{P}(b\  \vert\  c) = 0.7  .
$$

The result, easy to find, is that the desired probability must be less than $0.2$:

$$
\mathrm{P}(a \land b\  \vert\  c) \in [0, 0,2]  .
$$

With the `inferP()` function this probabilistic inference problem is posed as follows:
```
inferP(
  target = P(a & b  |  c),
  P(a  |  c) == 0.2,
  P(b  |  c) == 0.7
)
```
and yields the answer
```
min  max
0.0  0.2
```

This function can also be used in propositional logic, to check whether a logical formula or sequent from [sequent calculus](https://encyclopediaofmath.org/wiki/Sequent_calculus) follows from other formulae or sequents. For instance *modus ponens* (` > ` stands for if-then $\Rightarrow$):
```
inferP(
  target = P(b | I),
  P(a > b | I) == 1,
  P(a | I) == 1
)

min max 
  1   1 
```
or the *cut rule* of sequent calculus:
```
inferP(
  target = P(X + Y | I & J),
  P(A & X | I) == 1,
  P(Y | A & J) == 1
)

min max 
  1   1 
```


More information about notation and constraints is available in the help function `help('inferP')`. More interesting examples are given in the package's vignette.


# Pinference

This R package offers a function, `inferP()`, that calculates the lower and upper bounds of the probability for a proposition conditional on another, given constraints on the conditional probabilities for other propositions.

As a trivial example, suppose we want to know the bounds of the probability

$$
\mathrm{P}(a \land b  \vert  c)
$$

under the assumption that

$$
\mathrm{P}(a  \vert  c) = 0.2  ,\enspace
\mathrm{P}(b  \vert  c) = 0.7  .
$$

The result, easy to find, is that the desired probability must be less than $0.2$:

$$
\mathrm{P}(a \land b  \vert  c) \in [0, 0,2]  .
$$

With the `infeP()` this probabilistic inference problem is posed as follows:
```
inferP(
    target = P(a & b  ~  c),
    P(a  ~  c) == 0.2,
    P(b  ~  c) == 0.7
)
```
and yields the answer
```
min  max
0.0  0.2
```

More information about notation and constraints is available in the help function `help('inferP')`. More interesting examples are given in the package's vignette.

The package can be installed with
```
remotes::install_github('pglpm/Pinference')
```

It requires the [**lpSolve**](https://cran.r-project.org/package=lpSolve) package.


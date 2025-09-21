#' Calculation of lower and upper probability bounds
#'
#' @description
#' Compute the minimum and maximum allowed values of the probability of a propositional-logic expression conditional on another one, given numerical or equality constraints for the conditional probabilities of other propositional-logic expressions.
#'
#' @details
#' The function takes as first argument the probability of a propositional-logic expression, conditional on another expression, and as subsequent arguments the constraints on the probabilities of other propositional-logic expressions.
#'
#' A propositional-logic expression is a combination of atomic propositions by means of logical connectives. Atomic propositions can have any name that satisfies [R syntax for *object names*](https://cran.r-project.org/doc/FAQ/R-FAQ.html#What-are-valid-names_003f), for example:
#' ```
#' a
#' A
#' hypothesis1
#' coin.lands.tails
#' coin_lands_heads
#' `tomorrow it rains` # note the backticks
#' ```
#'
#' Available logical connectives are "not" (negation), "and" (conjunction), "or" (disjunction), "if-then". The first three follow standard R syntax for [logical operators](logical):
#' - Not: `!`
#' - And: `&&` or `&`
#' - Or: `||` or `|`
#'
#' The "if-then" connective is represented by the infix operator ` %>% `; internally `a %>% b` is simply defined as `b || !a`.
#' 
#' Examples of propositional-logic expressions are therefore
#' ```
#' a
#' a & b
#' (a | hypothesis1) & !A
#' (a %>% b) | c
#' ```
#'
#' 
#'
#' Each probability constraint can have one of these four forms:
#' ```
#' P(X|Z) = [number between 0 and 1]
#'
#' P(X|Z) = P(Y|Z)
#'
#' P(X|Z) = P(Y|Z) * [positive number]
#'
#' P(X|Z) = P(Y|Z) / [positive number]
#' ```
#' where `X`, `Y`, `Z` are (possibly different) propositional-logic expressions. Inequalities `<=` `>=` are also allowed instead of equalities.
#'
#' All probability expressions must start with `P`, as above, or `p` or `Pr` followed by parentheses. The symbol for the conditional bar and the logical connectives are as follows:
#'
#' - Conditional bar: `~`
#' - If-then: `%>%` (internally `a %>% b` is defined as `b || !a`)
#'
#' The expressions otherwise follow the standard rules for R logical expressions.
#' For instance,\cr
#' "P(¬(a ∨ b) ⇒ c | c ∧ I)"\cr
#' is written (adding spaces for clarity)
#' ```
#' P( !(a | b) %>% c  ~  c & I )
#' ```
#'
#' Note that *all* probabilities appearing in the function are understood to have a common, and-ed sentence "... ∧ I" in their conditional, if such a sentence doesn't appear explicitly. Otherwise the probabilities would be unrelated a priori.
#'
#' The function uses the `lpSolve::lp()` function from the [**lpSolve**](https://cran.r-project.org/package=lpSolve) package.
#'
#' @param target The desired probability expression.
#' @param ... Probability constraints (see Details).
#'
#' @return A vector of `min` and `max` values of the target probability, or `NA` if the constraints are mutually contradictory. If `min` and `max` are `0` and `1` then the constraints do not restrict the target probability in any way.
#'
#' @import lpSolve
#'
#' @examples
#' inferP(
#'   target = P(a & b ~ h), # P(a ∧ b | h) in standard notation
#'   P(a ~ h) == 0.3,
#'   P(b ~ h) == 0.6
#' )
#' ## min max
#' ## 0.0 0.3
#'
#' inferP(
#'     target = P(a & b ~ h), # P(a ∧ b | h) in standard notation
#'     P(a ~ h) == 0.3,
#'     P(b ~ a & h) == 0.2
#' )
#' ##  min  max
#' ## 0.06 0.06
#'
#' ## Solution to the Monty Hall problem:
#' inferP(
#'   target = P(car1 ~ you1 & host2 & I),
#'   P(car1 | car2 | car3 ~ I) == 1,
#'   P(car1 & car2 ~ I) == 0,
#'   P(car1 & car3 ~ I) == 0,
#'   P(car2 & car3 ~ I) == 0,
#'   P(host1 & you1 ~ I) == 0,
#'   P(host2 & you2 ~ I) == 0,
#'   P(host3 & you3 ~ I) == 0,
#'   P(host1 & car1 ~ I) == 0,
#'   P(host2 & car2 ~ I) == 0,
#'   P(host3 & car3 ~ I) == 0,
#'   P(host1 & host2 ~ I) == 0,
#'   P(host1 & host3 ~ I) == 0,
#'   P(host2 & host3 ~ I) == 0,
#'   P(host1 | host2 | host3 ~ I) == 1,
#'   P(host2 ~ car1 & you1 & I) == P(host3 ~ car1 & you1 & I),
#'   P(car1 ~ I) == 1/3,
#'   P(car2 ~ I) == 1/3,
#'   P(car3 ~ I) == 1/3,
#'   P(car1 ~ you1 & I) == 1/3,
#'   P(car2 ~ you1 & I) == 1/3,
#'   P(car3 ~ you1 & I) == 1/3
#' )
#' ##      min      max
#' ## 0.333333 0.333333
#'
#' @export
inferP <- function(target, ...) {
    ## Define if-then logical connective
    `%>%` <- function(a, b){b || !a}

    ## number of constraints
    nc <- length(substitute(alist(...)))

    ## find atomic sentences and prepare truth table for DNF
    tvals <- c(FALSE, TRUE)
    atoms <- sort(all.vars(as.formula(substitute(~ alist(target, ...)))))
    ttable <- list()
    for(i in atoms) {
        ttable[[i]] <- tvals
    }
    combos <- expand.grid(ttable)

    ## total number of conjunctions
    na <- 2L^length(atoms)

    ## print(combos) # for debugging

    ## Accepted probability (or truth) symbols
    Psyms <- c('P', 'p', 'Pr', 'pr', 'T', 't')
    ## accepted relation symbols
    Esyms <- c('==', '<=', '>=', '<', '>')

### Target probability
    Tsupp <- substitute(target)

    ## Syntax check
    if(
        !(length(Tsupp) == 2) || !(deparse(Tsupp[[1]]) %in% Psyms)
    ) {
        stop('invalid first argument')
    }
    Tsupp <- Tsupp[[2]]

    ## Below:
    ## E: matrix of constraint coefficients
    ## F: vector of numerical values of constraints
    ## D: vector of equality/inequality constraint directions

    if(length(Tsupp) < 3 || !(deparse(Tsupp[[1]]) == '~')){
        ## it doesn't have a conditional
        extraE <- NULL
        Obj <- 1L * apply(combos, 1, function(zz){
            eval(Tsupp, as.list(zz))
        })
        E <- matrix(1, nc + na, na)
        F <- numeric(nc + na)
        F[1] <- 1
        E[nc + (1:na), ] <- diag(na)
        D <- c(rep('==', nc), rep('>=', na))

    } else {
        ## it does have a conditional
        extraE <- 0
        Tsupp[[1]] <- `&&`
        Tcond <- Tsupp[[3]]
        Obj <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tsupp, as.list(zz))
            }),
            0)
        E <- matrix(1, nc + na + 2, na + 1)
        F <- numeric(nc + na + 2)
        ## first constraint has the form sum_i x_i - t = 0
        E[1, na + 1] <- -1
        ## last constraint involves the conditional of target prob.
        E[nc + 1, ] <- c(
            1L * apply(combos, 1, function(zz){
                eval(Tcond, as.list(zz))
            }),
            0)
        F[nc + 1] <- 1
        E[nc + 1 + (1:(na+1)), ] <- diag(na + 1)
        D <- c(rep('==', nc + 1), rep('>=', na + 1))
    }

### Constraint probabilities, analysed one at a time
    Tp <- substitute(alist(...))

    for(i in seq_len(nc)[-1]) {

        ## Syntax check
        if(length(Tp[[i]]) < 3 || !(deparse(Tp[[i]][[1]]) %in% Esyms)){
            stop('argument ', i, ' is not an (in)equality')
        }

        D[i] <- deparse(Tp[[i]][[1]]) # sign of (in)equality
        left <- Tp[[i]][[2]]

        ## Syntax check
        if(
            !(length(left) == 2) || !(deparse(left[[1]]) %in% Psyms)
        ) {
            stop('invalid left side in argument ', i)
        }

        right <- Tp[[i]][[3]]

        if(!is.numeric(try(eval(right), silent = TRUE))) {
            ## constraint is equality between two probabilities
            if(
                length(right) < 2 || length(right) > 3
            ) {
                stop('invalid right side in argument ', i)
            }

            if(length(right) == 2) {
                ## right side is a probability
                if(
                    !(deparse(right[[1]]) %in% Psyms)
                ) {
                    stop('invalid right side in argument ', i)
                }
                temp <- right
                right <- substitute(a * 1)
                right[[2]] <- temp
            }

            if(
                !(deparse(right[[1]]) %in% c('*', '/')) ||
                    !(length(right[[2]]) == 2) ||
                     !(deparse(right[[2]][[1]]) %in% Psyms)
            ) {
                stop('invalid right side in argument ', i)
            }
            coeff <- eval(right[[1]])(1, right[[3]])
            right <- right[[2]]

            Esuppl <- left[[2]]
            Esuppr <- right[[2]]
            if(
                !(length(Esuppl) < 3 || !(deparse(Esuppl[[1]]) == '~')) ||
                !(length(Esuppr) < 3 || !(deparse(Esuppr[[1]]) == '~'))
            ) {
                ## both probabilities have conditional
                ## we use the rule of cond. prob.
                if(!(Esuppl[[3]] == Esuppr[[3]])) {
                    stop('invalid conditionals in argument ', i)
                }
                Esuppl[[1]] <- `&&`
                Esuppr[[1]] <- `&&`
            }

            E[i, ] <- c(
                1L * apply(combos, 1, function(zz){
                    eval(Esuppl, as.list(zz))
                }) - coeff * apply(combos, 1, function(zz){
                    eval(Esuppr, as.list(zz))
                }),
                extraE)
        } else {
            ## constraint is numeric
            coeff <- eval(right)
            if(coeff < 0) {
                stop('negative coefficient in argument ', i)
            }

            Esupp <- left[[2]]
            if(length(Esupp) < 3 || !(deparse(Esupp[[1]]) == '~')) {
                ## probability has no conditional
                E[i, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(Esupp, as.list(zz))
                    }) - coeff,
                    extraE)
            } else {
                ## probability has conditional
                Esupp[[1]] <- `&&`
                Econd <- Esupp[[3]]
                E[i, ] <- c(
                    1L * apply(combos, 1, function(zz){
                        eval(Esupp, as.list(zz))
                    }) -
                        coeff * apply(combos, 1, function(zz){
                            eval(Econd, as.list(zz))
                        }),
                    extraE)
            }
        }
    }

    ## print(list(E = E, F = F, EqA = Obj)) # for debugging
    ## Find minimum value
    minp <- lpSolve::lp(
        direction = 'min',
        objective.in = Obj,
        const.mat = E,
        const.dir = D,
        const.rhs = F
    )
    ## ## for debugging
    ## print(minp$constraints)
    ## str(minp$solution)
    ## str(minp$objval)
    ## str(minp$status)

    ## Find maximum value
    maxp <- lpSolve::lp(
        direction = 'max',
        objective.in = Obj,
        const.mat = E,
        const.dir = D,
        const.rhs = F
    )
    ## ## for debugging
    ## print(maxp$constraints)
    ## str(maxp$solution)
    ## str(maxp$objval)
    ## str(maxp$status)

    c(
        min = if(minp$status == 0){minp$objval} else {NA},
        max = if(maxp$status == 0){maxp$objval} else {NA}
    )
}

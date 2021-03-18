Problem Set 1
================
Alex, Micah, and Scott
12/01/2020

``` r
library(data.table)

knitr::opts_chunk$set(echo = TRUE)
```

# Potential Outcomes Notation

1.  Explain the notation \(Y_i(1)\).

\(Y_i(1)\) is the potential treated outcome for an event or individual,
i. Since it is a potential treated outcome, this notation does not
neccessarily imply that they actually received treatment. It could be
completely hypothetical.

2.  Explain the notation \(Y_1(1)\).

\(Y_1(1)\) is the potential treated outcome notation for individual or
event 1. Like question 1, this notation does not neccessarily imply that
the individual 1 received treatment in real life.

3.  Explain the notation \(E[Y_i(1)|d_i=0]\).

This denotes the espected potential treated outcome among the group that
DID not receive treatment. In other words, it is the average outcome for
the group that did not receive treatment IF they received treatment.
Clearly, we never actually get to see this because it is purely
hypothetical.

4.  Explain the difference between the notation \(E[Y_i(1)]\) and
    \(E[Y_i(1)|d_i=1]\)

\(E[Y_i(1)\) denotes the expected treated outcome. This encompasses
people/events who are and are not actually in the treated group. The
second expression, \(E[Y_i(1)|d_i =1]\) specifically denotes the
expected treated outcome in the group that DOES receive treatment. With
the guarantess of random assignment, both of these expressions are equal
to each other.

# Potential Outcomes and Treatment Effects

1.  Use the values in the table below to illustrate that
    \(E[Y_i(1)]-E[Y_i(0)] = E[Y_i(1)- [Y_i(0)]\).

This expression, \(E[Y_i(1)]-E[Y_i(0)]\) is simply the average of
\(Y_1\) - the average of \(Y_0\) which is equivalent to taking the
average of both groups subtracted from each other. I will show this in
code here.

``` r
#Left hand side
table[ , .(mean(y_1))] - table[ , .(mean(y_0))]
```

    ##    V1
    ## 1:  2

``` r
table[, .(mean(y_1-y_0))]
```

    ##    V1
    ## 1:  2

**2 = 2**

2.  Is it possible to collect all necessary values and construct a table
    like the one below in real life? Explain why or why not.

**No it is not**. It is not possible to have two events occur in the
same individual at the same time. For this data to occur in real life,
two treatment events for the same individual would have to occur at
exactly the same time. Clearly, this cannot happen.

# Visual Acuity

Suppose we are interested in the hypothesis that children playing
outside leads them to have better eyesight.

Consider the following population of ten children whose visual acuity we
can measure.

  - Visual acuity is the decimal version of the fraction given as output
    in standard eye exams.
  - Someone with 20/20 vision has acuity 1.0, while someone with 20/40
    vision has acuity 0.5.
  - Numbers greater than 1.0 are possible for people with better than
    “normal” visual acuity.

<!-- end list -->

``` r
d <- data.table(
  child = 1:10, 
  y_0 = c(1.2, 0.1, 0.5, 0.8, 1.5, 2.0, 1.3, 0.7, 1.1, 1.4), 
  y_1 = c(1.2, 0.7, 0.5, 0.8, 0.6, 2.0, 1.3, 0.7, 1.1, 1.4)
)
```

In this table:

  - `y_1` means means the measured *visual acuity* if the child were to
    play outside at least 10 hours per week from ages 3 to 6’  
  - `y_0` means the measured *visual acuity* if the child were to play
    outside fewer than 10 hours per week from age 3 to age 6;
  - Both of these potential outcomes *at the child level* would be
    measured at the same time, when the child is 6.

<!-- end list -->

1.  Compute the individual treatment effect for each of the ten
    children.

<!-- end list -->

``` r
d[, .(individual_treatment_effect = y_1-y_0)]
```

    ##     individual_treatment_effect
    ##  1:                         0.0
    ##  2:                         0.6
    ##  3:                         0.0
    ##  4:                         0.0
    ##  5:                        -0.9
    ##  6:                         0.0
    ##  7:                         0.0
    ##  8:                         0.0
    ##  9:                         0.0
    ## 10:                         0.0

2.  Tell a “story” that could explain this distribution of treatment
    effects. In particular, discuss what might cause some children to
    have different treatment effects than others.

Perhaps, a minority of the population have genetic characteristics that
make playing outside or not playing outside beneficial. For the rest of
the population playing outside or not playing outside has no effect on
vision.

3.  For this population, what is the true average treatment effect (ATE)
    of playing outside.

<!-- end list -->

``` r
d[, .(ate = mean(y_1-y_0))]
```

    ##      ate
    ## 1: -0.03

**-0.03**

4.  Suppose we are able to do an experiment in which we can control the
    amount of time that these children play outside for three years. We
    happen to randomly assign the odd-numbered children to treatment and
    the even-numbered children to control. What is the estimate of the
    ATE you would reach under this assignment? (Please describe your
    work.)

Do to the guarantees of random assignment, we can express express the
aveage treatment effect as
\(ATE = E[Y_i(1)|D_i =1] - E[Y_i(0)|D_i = 0]\). This formula holds
because under random assignment the treatment and control groups have
the same expected potential outcome. Using the sampling framework
established in the questiion, the estimate for the ATE would be:

``` r
#select all the odd numbered children
odds = d[child %% 2 !=0] 
#select all the even numbered children
evens = d[child %%2 == 0]
#subtract their means, this is directly pulled from the formula above
odds[, .(mean(y_1))] - evens[, .(mean(y_0))]
```

    ##       V1
    ## 1: -0.06

To reach this estimate of ATE, I first selected all the odd numbered
children and then selected all the even numbered children. I then took
the mean eyesight score for both groups and subtracted the control group
from the treatment group to get the ATE.

5.  How different is the estimate from the truth? Intuitively, why is
    there a difference?

The estimated ATE is off from the actual ATE by **0.3**. Although the
estimated ATE is an unbiased estimator for the actual ATE, the results
in any given experiment can vary from the true ATE. In this case, our
sample size is very small making errors much more likely. With a much
larger sample size, the estimate would likely approach the true value.

6.  We just considered one way (odd-even) an experiment might split the
    children. How many different ways (every possible ways) are there to
    split the children into a treatment versus a control group (assuming
    at least one person is always in the treatment group and at least
    one person is always in the control group)?

We can use the formula \(\frac{N!}{v!(N-v)!}\) and sum the total
combinations.

``` r
total = 0
for (i in c(1:9)) {
  total = total + (factorial(10)/(factorial(i)*factorial(10-i)))
}
total
```

    ## [1] 1022

**1022**

7.  Suppose that we decide it is too hard to control the behavior of the
    children, so we do an observational study instead. Children 1-5
    choose to play an average of more than 10 hours per week from age 3
    to age 6, while Children 6-10 play less than 10 hours per week.
    Compute the difference in means from the resulting observational
    data.

<!-- end list -->

``` r
#first half of kids
first_half = d[child <= 5]
#second half of kids
second_half = d[child > 5]
first_half[, .(mean(y_1))] - second_half[, .(mean(y_0))]
```

    ##       V1
    ## 1: -0.54

**-0.54**

8.  Compare your answer in (7) to the true ATE. Intuitively, what causes
    the difference?

There is a difference of **0.48** between the true ATE and the ATE
atained with observational data. The main issue with observational data
is that it allows for a plethora of other factors that influence eye
sight to enter the calculation, meaning we cannot assure that the ATE is
truly an unbiased estimator. In this case, the children numbered 6-10
had unusually good vision leading to the result of \(-0.54\). One
possible explanation is that some of these children would have had the
same vision if they were in the opposite group, leading to this ATE.

# Randomization and Experiments

1.  Assume that researcher takes a random sample of elementary school
    children and compare the grades of those who were previously
    enrolled in an early childhood education program with the grades of
    those who were not enrolled in such a program. Is this an
    experiment, an observational study, or something in between?
    Explain\!

This is an observational study because the treatment (early chidhood
education) is not assigned to the students. They simply looked at two
groups, one who received early education and the other that did not.

2.  Assume that the researcher works together with an organization that
    provides early childhood education and offer free programs to
    certain children. However, which children that received this offer
    was not randomly selected by the researcher but rather chosen by the
    local government. (Assume that the government did not use random
    assignment but instead gives the offer to students who are deemed to
    need it the most) The research follows up a couple of years later by
    comparing the elementary school grades of students offered free
    early childhood education to those who were not. Is this an
    experiment, an observational study, or something in between?
    Explain\!

I would say this is something in between an observational study and an
experimental study. The treatment, early childhood education, is
assigned but it is not assigned randomly. This means that some of the
benefits seen in a truly randomized experiment will not be available in
this study. In other words, the researchers would not be able to compare
apples to apples since early childhood education was assigned based on
financial need. Almost certainly this would introduce confounding
variables into the study.

3.  Does your answer to part (2) change if we instead assume that the
    government assigned students to treatment and control by “coin toss”
    for each student? Why or why not?

Yes completely. This makes the experiment a true randomized experiment.
This would mean that on average the treatment and control group would
not differ in substantial ways. This would give us all the positive
guarantees of a true randomized experiment.

# Moral Panic

Suppose that a researcher finds that high school students who listen to
death metal music at least once per week are more likely to perform
badly on standardized test. :metal: As a consequence, the researcher
writes an opinion piece in which she recommends parents to keep their
kids away from “dangerous, satanic music”.

  - Let the potential outcomes to control, \(Y_i(0)\), be each student’s
    test score when listening to death metal at least one time per week.
  - Let \(Y_i(1)\) be the test score when listening to death metal less
    than one time per week.

<!-- end list -->

1.  Explain the statement \(E[Y_i(0)|D_i=0] = E[Y_i(0)|D_i=1]\) in
    words. First, state the rote english language translation – i.e.
    “The expected value of …” – but then, second, tell us the
    *meaning* of this statement.

The English translation would be the expected untreated potential
outcome (test scores) in individuals who actually listened to death
metal music is equal to the expected untreated potential outcome in
individuals who did not listen to death metal music, had they listened
to death metal music. In other words, students who do not receive the
treatment, not listening to death metal, have the same expected
untreated potential outcome \(Y_i(0)\) that the treatment group,
\(D_i = 1\) would have if they were untreated. This underlies one of the
main guarantees of random assignment, that treatment and control groups
have the same expected potential outcome.

2.  Do you expect that this circumstance actually matches with the
    meaning that you’ve just written down? Why or why not?

No I do not. The above statement is a guarantee with true random
assignment. The circumstances that the teacher put forth are not at all
random. It is very likely that individuals who choose to listen to death
metal music vary in substantial ways from individuals who do not since
assignment was not random. Therefore, I do not think the average test
scores would be the same between the groups and therefore the inequality
would not hold.

Problem Set 2
================
Alex, Micah, and Scott
12/07/2020

``` r
library(data.table)
library(sandwich)
library(lmtest)

library(ggplot2)
library(knitr)
```

# 1\. What happens when pilgrims attend the Hajj pilgrimage to Mecca?

What happens when a diverse set of people are brought together toward a
common purpose? Maybe it brings people together, or maybe instead it
highlights the differences between groups. [Clingingsmith, Khwaja and
Kremer (2009)](https://dash.harvard.edu/handle/1/3659699) investigate
the question. by asking Pakistani nationals to provide information about
their views about people from other nations.

The random assignment and data is collected in the following way
(detailed in the paper):

  - Pakistani nationals apply for a chance to attend the Hajj at a
    domestic bank. Saudi Arabia agreed in the time period of the study
    (2006) to grant 150,000 visas.
  - Of the 135,000 people who applied for a visa, 59% of those who
    applied were successful.
  - The remainder of the visas were granted under a different allocation
    method that was not randomly assigned, and so this experiment cannot
    provide causal evidence about other allocation mechanisms.
  - Among the 135,000 who apply, the authors conduct a random sample and
    survey about these respondents views about others.

Using the data collected by the authors, test, using randomization
infernece, whether there is a change in beliefs about others as a result
of attending the Hajj.

  - Use, as your primary outcome the `views` variable. This variable is
    a column-sum of each respondent’s views toward members of other
    countries.
  - Use, as your treatment feature `success`. This variable encodes
    whether the respondent successfully attended the Hajj.

<!-- end list -->

``` r
d <- fread("../data/clingingsmith_2009.csv")
```

1.  State the sharp-null hypothesis that you will be testing.

> In this case, the sharp-null hypothesis would be that for every
> individual the Hajj (the treatment) has no impact on the beliefs about
> others. This is in contrast to the null hypothesis which means there
> is no mean treatment effect.

2.  Using `data.table`, group the data by `success` and report whether
    views toward others are generally more positive among lottery
    winners or lottery non-winners. This answer should be of the form
    `d[ , .(mean_views = ...), keyby = ...]` where you have filled in
    the `...` with the appropriate functions and
varaibles.

<!-- end list -->

``` r
hajj_group_mean <- d[, .(mean_views = mean(views)), keyby = success] # the result should be a data.table with two colums and two rows
hajj_group_mean
```

    ##    success mean_views
    ## 1:       0   1.868304
    ## 2:       1   2.343137

``` r
hajj_ate <- d[, .(mean(views[success == 1]))] - d[, .(mean(views[success == 0]))]
hajj_ate
```

    ##           V1
    ## 1: 0.4748337

``` r
  # from the `hajj_group_mean` produce a single, numeric vector that is the ate. check that it is numeric using `class(hajj_ate)`
```

> From this simple test, it appears that the mean views towards other in
> the Hajj group is higher than in the control group. The ATE is 0.4748
> which indicates this difference.

But is this a “meaningful” difference? Or, could a difference of this
size have arisen from an “unlucky” randomization? Conduct 10,000
simulated random assignments under the sharp null hypothesis to find
out. (Don’t just copy the code from the async, think about how to write
this yourself.)

``` r
## do your work to conduct the randomiation inference here.
## as a reminder, RI will randomly permute / assign the treatment variable
## and recompute the test-statistic (i.e. the mean difference) under each permutation


randomize <- function(units_per_group) { 
  
  assignment_vector <- rep(c('success', 'no_success'), each = units_per_group)
  sample(assignment_vector)
} 




hajj_ri_distribution <- replicate(10000,  d[, .(mean = mean(views)), keyby = .(randomize(479))][ , diff(mean)])  # this should be a numeric vector that has a length equal to the number of RI permutations you ran
```

3.  How many of the simulated random assignments generate an estimated
    ATE that is at least as large as the actual estimate of the ATE?
    Conduct your work in the code chunk below, saving the results into
    `hajj_count_larger`, but also support your coding with a narrative
    description. In that narrative description (and throughout), use R’s
    “inline code chunks” to write your answer consistent with each time
    your run your
code.

<!-- end list -->

``` r
hajj_count_larger <- sum(hajj_ri_distribution >= 0.4748337)  # length 1 numeric vector from comparison of `hajj_ate` and `hajj_ri_distribution`
hajj_count_larger
```

    ## [1] 23

> Very few permutations led to an ATE highter or equal to the actual
> ATE. Since the generation is random, I did not include a specific
> value to avoid changes between tries.

4.  If there are `hajj_count_larger` randomizations that are larger than
    `hajj_ate`, what is the implied *one-tailed* p-value? Both write the
    code in the following chunk, and include a narrative description of
    the result following your
code.

<!-- end list -->

``` r
hajj_one_tailed_p_value <- mean(hajj_ri_distribution > hajj_ate) # length 1 numeric vector 
hajj_one_tailed_p_value
```

    ## [1] 0

> The results are highly significant at p = 0. This means under the
> sharp null the probability of geting an ATE as large as 0.4748337 is
> 0. This test in one directional or one tailed. This signifies that the
> Hajj does change one’s beliefs.

5.  Now, conduct a similar test, but for a two-sided p-value. You can
    either use two tests, one for larger than and another for smaller
    than; or, you can use an absolute value (`abs`). Both write the code
    in the following chunk, and include a narrative description of the
    result following your
code.

<!-- end list -->

``` r
hajj_two_tailed_p_value <- mean(abs(hajj_ri_distribution) > hajj_ate) # length 1 numeric vector 
hajj_two_tailed_p_value
```

    ## [1] 0

> The results are highly significant\! This can be interpreted as under
> the sharp null, the probability of getting an ATE of 0.4748337 is 0.
> This means we have substantial evidence to reject the sharp null.
> Unlike the one tailed test conducted earlier, this tests in both
> directions. This signifies that the Hajj does change one’s beliefs.

# 2\. Randomization Inference Practice

Suppose that you’ve been hired as the data scientist at a quack
nootropics company. Despite their fraudulent intent, you’re dedicated to
doing good data science. Or, at least science as good as you can.

Their newest serum, *kniht* purports to raise its users’ executive
function. You think that it is just amphetamines.

As the data scientist for the company, you convince them to conduct a
trial. Great\! The good news is this:

  - Each person is measured twice.
  - Before one of the measurements, they are given a placebo. Before the
    other of the measurements they are given *kniht*.
  - You ask for instrumentation on two concepts:
      - Creativity, measured as number of proposed alternative uses of
        an object. (This is a classic, test of “creativity”, proposed by
        J.P. Guilford. For example, how many things can you propose
        doing with a camera tripod? )
      - Physical Arousal, measured through skin conductance (i.e. how
        sweaty is someone).

The bad news is this: The company knows that they’re selling nonsense,
and they don’t want you to be able to prove it. They reason that if they
provide you only six test subjects, that you won’t be able to prove
anything, and that they can hide behind a “fail-to-reject” claim.

``` r
kniht <- data.table(
  person  = rep(LETTERS[1:6], each = 4), 
  treat   = rep(0:1, each = 2), 
  measure = rep(c('creative', 'sweat'))
)


kniht[measure == 'creative' & treat == 0, 
      value := c(10, 13, 14, 16, 25, 40)]
kniht[measure == 'creative' & treat == 1, 
      value := c(12, 11, 13, 20, 21, 46)]
kniht[measure == 'sweat' & treat == 0, 
      value := c(0.4, 0.7, 0.3, 0.8, 1.0, 1.4)]
kniht[measure == 'sweat' & treat == 1, 
      value := c(0.4, 0.7, 2.0, 0.9, 1.6, 2.2)]
```

Conduct the following tests.

1.  Conduct the appropriate t-test that respects the repeated-measures
    nature of the data (is this a paired or independent samples t-test?)
    for both the `creative` and the `sweat` outcomes. After you conduct
    your tests, write a narrative statement about what you
conclude.

<!-- end list -->

``` r
t_test_creative <-kniht[measure == "creative"][,t.test(value~treat, paired = T)]
t_test_creative 
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  value by treat
    ## t = -0.53483, df = 5, p-value = 0.6157
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -4.838641  3.171975
    ## sample estimates:
    ## mean of the differences 
    ##              -0.8333333

``` r
t_test_sweat <- kniht[measure == "sweat"][,t.test(value~treat, paired = T)]
t_test_sweat
```

    ## 
    ##  Paired t-test
    ## 
    ## data:  value by treat
    ## t = -1.9725, df = 5, p-value = 0.1056
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.228395  0.161728
    ## sample estimates:
    ## mean of the differences 
    ##              -0.5333333

> Neither the results for the paired t-test on creativity nor sweat are
> significant. This means that the mean outcome variable between the
> control group and the treatment group are not significantly different
> under the assumptions of a t-test.

2.  Conduct the appropriate randomization inference test that respects
    the repeated-measures nature of the data. After you conduct your
    tests, write a narrative statement about what you conclude.

<!-- end list -->

``` r
# do your work, and then save your computed p-value in the object
calc_ate <- function(data, m) {
  ate <- data[measure == m & treat == 1, value] - data[measure == m & treat == 0, value]
  return(mean(ate))
}


creative_ate <-calc_ate(kniht, "creative")
creative_ate
```

    ## [1] 0.8333333

``` r
creative_ri <- replicate(10000, kniht[measure == 'creative', .(treat = sample(treat), value), 
                                      keyby = person][, .(mean_values = mean(value)), keyby = treat][,diff(mean_values)])

creative_p_value <- mean(abs(creative_ri > creative_ate))
creative_p_value
```

    ## [1] 0.2659

> With the randomization test on creativity, the results are not
> statistically significant. This means we do not have sufficient
> evidence to reject the null hypothesis that Kniht doees not affect
> executive function.

``` r
# do your work, and then save your computed p-value in the object
sweat_ate <- calc_ate(kniht, "sweat")
sweat_ri      <- replicate(10000, kniht[measure == 'sweat', .(treat = sample(treat), value), 
                                      keyby = person][, .(mean_values = mean(value)), keyby = treat][,diff(mean_values)])
sweat_p_value <- mean(sweat_ri > sweat_ate)
sweat_ate
```

    ## [1] 0.5333333

``` r
sweat_p_value
```

    ## [1] 0

> The significance of this test is actually highly significant with a
> p-value of 0. We have evidence to reject the null hypothesis. This
> means we have sufficient evidence to reject the null hypothesis that
> Kniht has no impact on sweat.

3.  Which of these tests are more appropriate to the task at hand, and
    why? Based on the tests that you have run, what do you conclude
    about the effectiveness of *kniht*?

> The randomization tests are far more appropriate. Our sample only
> contains two measurements for 6 subjects and therefore the assumptions
> to conduct a T-test are not met. For this reason, the results from the
> T-test are not trustworthy. There is not any evidence that Kniht
> impacts creativity, but there is influence it’ll increase sweat.

# 3\. Sports Cards

In this experiment, the experimenters invited consumers at a sports card
trading show to bid against one other bidder for a pair trading cards.
We abstract from the multi-unit-auction details here, and simply state
that the treatment auction format was theoretically predicted to produce
lower bids than the control auction format. We provide you a relevant
subset of data from the experiment.

In this question, we are asking you to produce p-values and confidence
intervals in three different ways:

1.  Using a `t.test`;
2.  Using a regression; and,
3.  Using randomization inference.

<!-- end list -->

``` r
library(data.table)
d <- fread('../data/list_data_2019.csv')
```

1.  Using a `t.test`, compute a 95% confidence interval for the
    difference between the treatment mean and the control mean. After
    you conduct your test, write a narrative statement, using inline
    code evaluation that describes what your tests find, and how you
    interpret these results. (You should be able to look into
    `str(t_test_cards)` to find the pieces that you want to pull to
    include in your written
results.)

<!-- end list -->

``` r
t_test_cards <- t.test(bid ~uniform_price_auction, d)# this should be the t.test object. Extract pieces from this object in-text below the code chunk. 
t_test_cards$conf.int
```

    ## [1]  3.557141 20.854624
    ## attr(,"conf.level")
    ## [1] 0.95

``` r
#t_test_cards <- t.test(x = d[uniform_price_auction == 1, bid], y = d[uniform_price_auction == 0, bid])
#t_test_cards
```

> The confidence interval is from 11.9334 to 12.478. Since 0 is not
> contained in the interval, therefore there is significant evidence to
> reject the null hypothesis.

2.  In plain language, what does this confidence interval mean?

> For a t test, the confidence interval is the probability that the mean
> will fall between a given set of values for a certain percentage of
> the time. In this test, the mean will fall between 11.9334 to 12.478
> 95% of the time with repeated experimentation.

3.  Conduct a randomization inference process using an estimator that
    you write by hand (i.e. in the same way as earlier questions). On
    the sharp-null distribution that this process creates, compute the
    2.5% quantile and the 97.5% quantile using the function `quantile`
    with the appropriate vector passed to the `probs` argument. After
    you conduct your test, write a narrative statement of your test
    results.

<!-- end list -->

``` r
## first, do you work for the randomization inference
randomize <- function(len_control, len_tretment) {
  return(sample(c(rep(0,len_control), rep(1, len_tretment))))
}


ate <- function(data) {
  data[, con := randomize(.N-sum(uniform_price_auction), sum(uniform_price_auction))]
  res <- data[, .(mean_b = mean(bid)), keyby = con]
  return(res[con == 1, mean_b]- res[con == 0, mean_b])
}

#ate(d)
ri_distribution <- replicate(10000, ate(d)) # numeric vector of length equal to your number of RI permutations
ri_quantiles    <- quantile(ri_distribution, probs = c(0.025, 0.975)) # there's a built-in to pull these. 
ri_quantiles
```

    ##      2.5%     97.5% 
    ## -8.911765  8.852941

> The 95% confidence interval extends from -8.852941 to 9.088235 which
> contains 0. This means that the results are not statistically
> significant and thus, we fail to reject the null. In practical terms,
> this means that the value falls somewhere in that range 95% of the
> time.

4.  Do you learn anything different if you regress the outcome on a
    binary treatment variable? To answer this question, regress `bid` on
    a binary variable equal to 0 for the control auction and 1 for the
    treatment auction and then calculate the 95% confidence interval
    using *classical standard errors* (in a moment you will calculate
    with *robust standard errors*). There are two ways to do this – you
    can code them by hand; or use a built-in, `confint`. After you
    conduct your test, write a narrative statement of your test
results.

<!-- end list -->

``` r
mod <- lm(bid ~ uniform_price_auction, data = d) # this should be a model object, class = 'lm'. 
confint(mod)
```

    ##                           2.5 %    97.5 %
    ## (Intercept)            22.71534 34.931716
    ## uniform_price_auction -20.84416 -3.567603

> Using regression with classical standard errors we get a confidence
> interval from 20.844 to -3.5676. Since zero is not contained, there is
> sufficient evidence to reject the null hypothesis.

5.  Calculate the 95% confidence interval using robust standard errors,
    using the `sandwich` package. There is a function in `lmtest` called
    `coefci` that can help with this. It is also possible to do this
    work by hand. After you conduct your test, write a narrative
    statement of your test results.

<!-- end list -->

``` r
library(sandwich)
lmtest::coefci(mod, vcov = vcovHC(mod, type ="HC"), "uniform_price_auction")
```

    ##                           2.5 %    97.5 %
    ## uniform_price_auction -20.71618 -3.695584

> The results for the robust standard errors are also significant as
> zero is not contained. They range from -20.716 to 3.6955. Using robust
> standard erros means that they are more reliable under
> heteroskedacity.

6.  Characterize what you learn from each of these different methods –
    are the results contingent on the method of analysis that you
    choose? \> I was surprised that the results from randomization
    inference were so different from the t-test and regression. Both
    regression and the t-test produced significant results whereas
    randomization inference did not.

# Power Analysis

Understanding whether your experiment design and data collection
strategy are able to reject the null hypothesis *when they should* is
valuable\! And, this isn’t theoretical value. If your design and data
collection cannot reject the null hypothesis, why even run the
experiment in the first place?

The classical formulation of power asks, “Given a test procedure and
data, what proportion of the tests I *could conduct* would reject the
null hypothesis?”

Imagine that you and David Reiley are going to revive the sports card
experiment from the previous question. However, because it is for a
class project, and because you’ve already spent all your money on a
shiny new data science degree :raised\_hands: :money\_with\_wings: ,
you’re not going to be able to afford to recruit as many participants
as before.

1.  Describe a t-test based testing procedure that you might conduct for
    this experiment. What is your null hypothesis, and what would it
    take for you to reject this null hypothesis? (This second statement
    could either be in terms of p-values, or critical values.)

> A t-test experiment for this hypothesis would test if the treatment
> mean is different than the control mean. In this case, the outcome for
> the treatment group is the selling price with a paired auction and the
> outcome for the control group is the selling price for the traditional
> auction. The null hypothesis is that there is no difference in the
> means between both groups. To reject the null hypothsis at a
> significance level of \(\alpha = 0.05\), the p-value would have to be
> below 0.05.

2.  Suppose that you are only able to recruit 10 people to be a part of
    your experiment – 5 in treatment and another 5 in control. Simulate
    “re-conducting” the sports card experiment once by sampling from
    the data you previously collected, and conducting the test that
    you’ve written down in part 1 above. Given the results of this 10
    person simulation, would your test reject the null
hypothesis?

<!-- end list -->

``` r
t_test_ten_people <- d[, .SD[sample(.N, 5, replace = T)], by = uniform_price_auction][, t.test(bid ~ uniform_price_auction)] # this should be a test object
t_test_ten_people
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  bid by uniform_price_auction
    ## t = 0.088056, df = 6.2009, p-value = 0.9326
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -21.25717  22.85717
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##            24.8            24.0

> No we would not reject the null hypothesis. The p-value is \(0.5609\)
> which is way above the significance level. Moreover, the confidence
> interval includes zero which is another sign that the results are not
> statistically significant at the \(\alpha = 0.05\) level.

3.  Now, repeat this process – sampling 10 people from your existing
    data and conducting the appropriate test – one-thousand times. Each
    time that you conduct this sample and test, pull the p-value from
    your t-test and store it in an object for later use. Consider
    whether your sampling process should sample with or without
    replacement.

<!-- end list -->

``` r
t_test_p_values <- rep(NA, 1000) # fill this in with the p-values from your power analysis

sample_and_t <- function(data, size) {
  data[, .SD[sample(.N, size, replace = T)], by = uniform_price_auction][, t.test(bid ~ uniform_price_auction)$p.value]
}

t_test_p_values <- replicate(1000, sample_and_t(d, 5))


  ## you can either write a for loop, use an apply method, or use replicate (which is an easy-of-use wrapper to an apply method)
```

4.  Use `ggplot` and either `geom_hist()` or `geom_density()` to produce
    a distribution of your p-values, and describe what you see. What
    impression does this leave you with about the power of your test?

<!-- end list -->

``` r
library(ggplot2)
ggplot() + aes(t_test_p_values) +
  geom_histogram(binwidth=.05) +
  labs( x = "P-Values", y = "Count", title ="Sample Distribution of Test Statistic")
```

![](ps2_files/figure-gfm/histogram%20of%20ten%20person%20samples-1.png)<!-- -->

5.  Suppose that you and David were to actually run this experiment and
    design – sample 10 people, conduct a t-test, and draw a conclusion.
    **And** suppose that when you get the data back, **lo and behold**
    it happens to reject the null hypothesis. Given the power that your
    design possesses, does the result seem reliable? Or, does it seem
    like it might be a false-positive result?

> In this case, the statistically significant results are simply due to
> chance since statistically significant results are seldomly occuring.
> With a significance level of 0.05, significant results occur due to
> chance in every 20 executions of the experiment.

6.  Apply the decision rule that you wrote down in part 1 above to each
    of the simulations you have conducted. What proportion of your
    simulations have rejected your null hypothesis? This is the p-value
    that this design and testing procedure generates. After you write
    and execute your code, include a narrative sentence or two about
    what you see.

<!-- end list -->

``` r
t_test_rejects <- t_test_p_values < 0.05
sum(t_test_rejects)/1000
```

    ## [1] 0.146

> 13.9% of the simulations have rejected the null hypothesis. This is
> slightly more than I would have espected purely based on chance, which
> would have been 5% since the significance level was set at 0.05.

7.  Does buying more sample increase the power of your test? Apply the
    algorithm you have just written onto different sizes of data.
    Namely, conduct the exact same process that you have for 10 people,
    but now conduct the process for every 10% of recruitment size of the
    original data: Conduct a power analysis with a 10%, 20%, 30%, … 200%
    sample of the original data. (You could be more granular if you
    like, perhaps running this task for every 1% of the data).

<!-- end list -->

``` r
percentages_to_sample <- c()
for(percent in seq(from=100, to=2000, by=100)) {
          res <- sample_and_t(d, percent)
         percentages_to_sample <- c(percentages_to_sample, res)
}
percentages_to_sample
```

    ##  [1] 3.138077e-05 3.033138e-13 2.987335e-13 9.536144e-18 7.419336e-21
    ##  [6] 4.242594e-35 1.237684e-26 4.183085e-46 4.115303e-43 7.405293e-49
    ## [11] 7.994562e-67 4.833174e-49 2.795168e-80 7.629593e-86 7.673626e-81
    ## [16] 8.912379e-77 2.194700e-89 2.175288e-96 2.939822e-95 6.335002e-91

``` r
sum(percentages_to_sample <= 0.05)/length(percentages_to_sample)
```

    ## [1] 1

> With a bigger sample size all the results are significant. This
> demonstrates the power of increasing the sample size. Notably, the
> p-value actually increases every time the sample size increases\!

Experiments and Causality: Problem Set 3
================
Alex, Micah and Scott
12/7/2020

``` r
library(data.table)

library(sandwich)
library(lmtest)

library(ggplot2)
library(patchwork)

library(foreign)
```

# 1\. Replicate Results

Skim [Broockman and Green’s](./readings/brookman_green_ps3.pdf) paper on
the effects of Facebook ads and download an anonymized version of the
data for Facebook users only.

``` r
library(data.table)
d <- fread("../data/broockman_green_anon_pooled_fb_users_only.csv")
```

1.  Using regression without clustered standard errors (that is,
    ignoring the clustered assignment), compute a confidence interval
    for the effect of the ad on candidate name recognition in Study 1
    only (the dependent variable is `name_recall`). After you estimate
    your model, write a narrative description about what you’ve learned.

<!-- end list -->

  - **Note**: Ignore the blocking the article mentions throughout this
    problem.
  - **Note**: You will estimate something different than is reported in
    the
study.

<!-- end list -->

``` r
mod_study1 <- d[studyno == 1,lm(name_recall ~ treat_ad, data = d)] # should be a lm class object
summary(mod_study1)
```

    ## 
    ## Call:
    ## lm(formula = name_recall ~ treat_ad, data = d)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.4542 -0.4542 -0.2991  0.5458  0.7009 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.45420    0.01219  37.262   <2e-16 ***
    ## treat_ad    -0.15507    0.01876  -8.265   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4816 on 2699 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  0.02469,    Adjusted R-squared:  0.02432 
    ## F-statistic: 68.31 on 1 and 2699 DF,  p-value: < 2.2e-16

``` r
confint(mod_study1)
```

    ##                  2.5 %     97.5 %
    ## (Intercept)  0.4302949  0.4780971
    ## treat_ad    -0.1918631 -0.1182834

> From this model, it appears that the treatment add does not have a
> significant impact on name\_recall. The coefficient is -0.009798 with
> non-robust standard error 0.021012 and the p-value is 0.641.

2.  What are the clusters in Broockman and Green’s study? Why might
    taking clustering into account increase the standard errors?

> First, they reduced the potential sample to individuals between the
> age of 30 and 75. This reduced the sample size to 32,029 voters. Then
> individuals were then broken into 1,220 clusters across 18 age groups,
> the 34 towns in the candidates’ district, and 2 genders. Standard
> error increases with clusters since the variation between clusters is
> likely rather large. This makes sense since each individual cluster
> contains relatively similar individuals.

3.  Estimate a regression that estimates the effect of the ad on
    candidate name recognition in Study 1, but this time take take
    clustering into account when you compute the standard errors.

<!-- end list -->

  - The estimation of the *model* does not change, only the estimation
    of the standard errors.
  - You can estimate these clustered standard errors using
    `sandwich::vcovCL`, which means: "The `vcovCL` function from the
    `sandwich` package.
  - We talk about this more in code that is availbe in the course repo.

<!-- end list -->

``` r
#uses mod_study_1
mod_study1$vcovCL_val <- vcovCL(mod_study1, cluster = d[, cluster])
mod_study1_test <- coeftest(x = mod_study1, level = 0.95, vcov. = mod_study1$vcovCL_val)
mod_study1_conf <- coefci(x = mod_study1, level = 0.95, vcov. = mod_study1$vcovCL_val, 'treat_ad')
mod_study1_test
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  0.454196   0.018576 24.4504 < 2.2e-16 ***
    ## treat_ad    -0.155073   0.026730 -5.8014 7.344e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod_study1_conf
```

    ##               2.5 %    97.5 %
    ## treat_ad -0.2074875 -0.102659

> Using clustered standard errors made the estimated confidence interval
> somewhat wider than the model with non-clustered standard errors. This
> likely occcured because the inclusion of clusters essentially shrunk
> the sample size which was used for the standard error calculation.
> Moreover, variation between clusters is likely rather large.

4.  Change the context: estimate the treatment effect in Study 2, using
    clustered standard errors. If you’ve written your code for part 3
    carefully, you should be able to simply change the row-scoping that
    you’re calling. If you didn’t write it carefully, for legibility for
    your colleagues, you might consider re-writting your solution to the
    last question. Descriptively, do the treatment effects look
    different between the two studies? Are you able to conduct a formal
    test by comparing these coefficients? Why, or why not?

<!-- end list -->

``` r
#conduct regression just on studyno 2
mod_study2 <- d[studyno == 2, lm(name_recall ~ treat_ad)]
mod_study2$vcovCL_val <- vcovCL(mod_study2, cluster = d[studyno == 2, cluster])
mod_study2_test <- coeftest(x = mod_study2, level = 0.95, vcov. = mod_study2$vcovCL_val)
mod_study2_conf <- coefci(x = mod_study2, level = 0.95, vcov. = mod_study2$vcovCL_val, 'treat_ad')
mod_study2_test
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.6057884  0.0181889  33.305   <2e-16 ***
    ## treat_ad    -0.0028033  0.0355033  -0.079   0.9371    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod_study2_conf
```

    ##                2.5 %     97.5 %
    ## treat_ad -0.07245159 0.06684489

> The treatment effects between the two studies are slightly different.
> The interval for study 2 is larger than it is for study 2. This means
> that there is more uncertainty arround the estimate. Moreover, the
> coefficient for `treat_ad` is smaller in study 2 than in study 1,
> meaning the treatment effect is smaller. I am not aware of a test that
> would allow a comparison between the two models. An F test in this
> case is not appropriate since an F test compares models measures a
> restricted and an unrestricted regression (or a regression with less
> and more coefficients). Since this test would have to measure results
> from two studies, I do not think there’s an appropriate test since
> these is an apples to oranges comparison.

5.  Run a regression to test for the effect of the ad on candidate name
    recognition, but this time use the entire sample from both studies –
    do not take into account which study the data is from (more on this
    in a moment), but just “pool” the data.

<!-- end list -->

  - Does this estimate tell you anything useful?
  - Why or why not?
  - Can you say that the treatment assignment procedure used is fully
    random when you estimate this model? Or is there some endogeneous
    process that could be confounding your
estimate?

<!-- end list -->

``` r
mod_pooled <- d[, lm(name_recall ~ treat_ad)] # should be a lm class object
mod_pooled$vcovCL_val <- vcovCL(mod_pooled, cluster = d[,cluster])
mod_pooled_test <- coeftest(x = mod_pooled, level = 0.95, vcov. = mod_pooled$vcovCL_val)
mod_pooled_conf <- coefci(x = mod_pooled, level = 0.95, vcov. = mod_pooled$vcovCL_val, 'treat_ad')
mod_pooled_test
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  0.454196   0.018576 24.4504 < 2.2e-16 ***
    ## treat_ad    -0.155073   0.026730 -5.8014 7.344e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod_pooled_conf
```

    ##               2.5 %    97.5 %
    ## treat_ad -0.2074875 -0.102659

> Running a regression in this fashion drammatically reduces the utility
> of the test. Running the regression in this fashion does not make
> logical sense since the context of the study varies substantiall with
> study 1 being conducted in a Republican controlled non-battleground
> state and study 2 being conducted for a Democratic Candidate in a
> non-battleground state. I think running the statistical tests in this
> fashion could impact random assignment since the demographics of study
> 1 are going to be completely different than study 2, essentially
> breaking the purpose of a randomized experiment.

6.  Estimate a model that uses all the data, but this time include a
    variable that identifies whether an observation was generated during
    Study 1 or Study 2.

<!-- end list -->

  - What is estimated in the “Study 2 Fixed Effect”?
  - What is the treatment effect estimate and associated p-value?
  - Think a little bit more about the treatment effect that you’ve
    estimated: Can this treatment effect, as you’ve entered it in the
    model be *different* between Study 1 and Study 2?
  - Why or why not?

<!-- end list -->

``` r
# add dummy for study number with 0/1
d[, study2_dummy := ifelse(studyno == 2,1,0)]
mod_fe <- d[, lm(name_recall ~ treat_ad + study2_dummy)]
mod_fe$vcovCL_val <- vcovCL(mod_fe, cluster = d[,cluster])
mod_fe_test <- coeftest(x = mod_fe, level = 0.95, vcov. = mod_fe$vcovCL_val)
mod_fe_conf <- coefci(x = mod_fe, level = 0.95, vcov. = mod_fe$vcovCL_val, 'treat_ad')
mod_fe_test
```

    ## 
    ## t test of coefficients:
    ## 
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.1806848  0.0169702 10.6472   <2e-16 ***
    ## treat_ad     -0.0067752  0.0204154 -0.3319     0.74    
    ## study2_dummy  0.4260988  0.0206969 20.5875   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod_fe_conf
```

    ##                2.5 %     97.5 %
    ## treat_ad -0.04680668 0.03325618

> I included `study2_dummy` as a coefficient in the model to better
> facilitate understanding of the covariates impact. The fixed effect of
> the covariate for study 2 is 0.4260988 (0.0206969) with a highly
> significant p-value. The treatment effect estimate is -0.0067752
> (0.0204154) with p-value 0.74. The treatment effects can be different
> between study 1 and study 2. The population in study 1 and study 2 is
> quite different. It is possible that these different groups in fact
> respond to treatment differently. Including an interaction term in the
> model would most likely be beneficial to see the heterogeneous
> treatment effects.

7.  Estimate a model that lets the treatment effects be different
    between Study 1 and Study 2. With this model, conduct a formal test
    – it must have a p-value associated with the test – for whether
    the treatment effects are different in Study 1 than Study
2.

<!-- end list -->

``` r
mod_interaction <- d[, lm(name_recall ~ treat_ad + study2_dummy + treat_ad * study2_dummy)]
mod_interaction$vcovCL_val <- vcovCL(mod_interaction, cluster = d[,cluster])
mod_interaction_test <- coeftest(x = mod_interaction, level = 0.95, vcov. = mod_interaction$vcovCL_val)
mod_interaction_conf <- coefci(x = mod_interaction, level = 0.95, vcov. = mod_interaction$vcovCL_val, 'treat_ad')
mod_interaction_test
```

    ## 
    ## t test of coefficients:
    ## 
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            0.1824687  0.0184880  9.8696   <2e-16 ***
    ## treat_ad              -0.0097979  0.0237491 -0.4126   0.6800    
    ## study2_dummy           0.4233197  0.0259296 16.3257   <2e-16 ***
    ## treat_ad:study2_dummy  0.0069945  0.0427010  0.1638   0.8699    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
mod_interaction_conf
```

    ##                2.5 %    97.5 %
    ## treat_ad -0.05636617 0.0367704

``` r
#check differences between models
anova(mod_fe, mod_interaction, test = "F")
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: name_recall ~ treat_ad + study2_dummy
    ## Model 2: name_recall ~ treat_ad + study2_dummy + treat_ad * study2_dummy
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1   2698 517.88                           
    ## 2   2697 517.87  1 0.0069749 0.0363 0.8489

> This model, `mod_interaction` effectively shows the heterogenous
> treatment effect of study number. This means we can see the difference
> in treatment effects by the study. The heterogenous treatment effects
> of the study are not significant. The ANOVA F test failed to show any
> significant improvements in this new model as compared to the previous
> model as the RSS has not improved and there is not a significant
> p-value.

# 2\. Peruvian Recycling

Look at [this article](./readings/recycling_peru.pdf) about encouraging
recycling in Peru. The paper contains two experiments, a “participation
study” and a “participation intensity study.” In this problem, we will
focus on the latter study, whose results are contained in Table 4 in
this problem. You will need to read the relevant section of the paper
(starting on page 20 of the manuscript) in order to understand the
experimental design and variables. (*Note that “indicator variable” is a
synonym for “dummy variable,” in case you haven’t seen this language
before.*)

1.  In Column 3 of Table 4A, what is the estimated ATE of providing a
    recycling bin on the average weight of recyclables turned in per
    household per week, during the six-week treatment period? Provide a
    95% confidence interval.

> The point estimate for the ATE of providing a recycling bin is 0.187.
> The 95% confidence is calculated in the chunk below. The range is from
> 0.12428 ~ 0.24972 which is significant since zero is not contained.

``` r
coeficient = 0.187
se = 0.032
lower_bound = coeficient - 1.96 * se
upper_bound = coeficient + 1.96 * se
paste(lower_bound, "~", upper_bound)
```

    ## [1] "0.12428 ~ 0.24972"

2.  In Column 3 of Table 4A, what is the estimated ATE of sending a text
    message reminder on the average weight of recyclables turned in per
    household per week? Provide a 95% confidence interval.

> The estimated ATE of sending a text message reminder is 20.024 with a
> standard error of 0.039. The 95% confidence is calculated in the chunk
> below (-0.10044 ~ 0.05244).

``` r
coef_2 = -0.024
se_2 = 0.039
lower_bound = coef_2 - 1.96*se_2
upper_bound = coef_2 + 1.96 *se_2
paste(lower_bound, "~", upper_bound)
```

    ## [1] "-0.10044 ~ 0.05244"

3.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of providing a recycling bin?

> Four outcome variables show statistically significant effects at the
> 5% level. The outcome variables are percentage of visits turned in
> bag, avg. number of bins turned in per week, average weight of
> recyclables turned in per week, and average market value of
> recyclables turned in per week.

4.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of sending text messages? \> None of the
    outcome measures in Table 4A show statistically significant effects
    at the 0.05 level from sending text messages.

5.  Suppose that, during the two weeks before treatment, household A
    turns in 2kg per week more recyclables than household B does, and
    suppose that both households are otherwise identical (including
    being in the same treatment group). From the model, how much more
    recycling do we predict household A to have than household B, per
    week, during the six weeks of treatment? Provide only a point
    estimate, as the confidence interval would be a bit complicated.
    This question is designed to test your understanding of slope
    coefficients in regression.

> Holding all else constant we espect household A to turn in 2.244kg
> more of recycling than household B. I calculated this by multiplying
> the slope coefficient for Avg. weight (in kg) of recyclables turned in
> per week, 0.187*2kg *6 weeks which gave a total of 2.244kg. I
> multiplied by 6 since the study duration is six weeks.

6.  Suppose that the variable “percentage of visits turned in bag,
    baseline” had been left out of the regression reported in Column 1.
    What would you expect to happen to the results on providing a
    recycling bin? Would you expect an increase or decrease in the
    estimated ATE? Would you expect an increase or decrease in the
    standard error? Explain our reasoning.

> I would espect an increase in the average treatment effect. I believe
> removing this variable would make it look like providing a recycling
> bin has a larger effect than it actually does. I would also espect the
> standard error to increase.

7.  In column 1 of Table 4A, would you say the variable “has cell phone”
    is a bad control? Explain your reasoning.

> I do no think so. I do not believe has cell phone is correlated with
> both the treatment and the outcome variable. For this reason, I do not
> think it’s a bad control. However, the necessity to include this
> variable is largely an issue with the study design since one of the
> treatments cannot be applied to some people in the treatment group.

8.  If we were to remove the “has cell phone” variable from the
    regression, what would you expect to happen to the coefficient on
    “Any SMS message”? Would it go up or down? Explain your reasoning.

> I think it would likely increase to some extent since it would take
> some variance from the has cell phone variable. This would occur since
> the information contained in the has cell phone variable would likely
> get picked up in the sms variable.

# 3\. Multifactor Experiments

Staying with the same experiment, now think about multifactor
experiments.

1.  What is the full experimental design for this experiment? Tell us
    the dimensions, such as 2x2x3. (Hint: the full results appear in
    Panel 4B.)

> The dimensions are 3 \*3. There are 3 treatment groups and within the
> groups there are no SMS messages, personal SMS message, and generic
> SMS message.

2.  In the results of Table 4B, describe the baseline category. That is,
    in English, how would you describe the attributes of the group of
    people for whom all dummy variables are equal to zero?

> This would describe people who do not have a cell phone, who did not
> receive a bin, nor an SMS message

3.  In column (1) of Table 4B, interpret the magnitude of the
    coefficient on “bin without sticker.” What does it mean?

> Bin without sticker signifies a recycling bin that does not have a
> sticker. The coefficient for bin without sticker signifies that having
> a bin without a sticker (a dummy variable) leads to an increase of
> 0.035 visits where a bag of recyclables are turned in. In contrast,
> not having a bin without a sticker leads to a 0% change in turning in
> a recyclable bag.

4.  In column (1) of Table 4B, which seems to have a stronger treatment
    effect, the recycling bin with message sticker, or the recycling bin
    without sticker? How large is the magnitude of the estimated
    difference?

> The recycling bin with a sticker seems to have a somewhat stronger
> effect. The coefficient for the recycling bin with a sticker is 0.055
> with a standard error of 0.015 and the coefficient for a recycling bin
> without a sticker is 0.035 with a standard error of 0.015.

5.  Is this difference you just described statistically significant?
    Explain which piece of information in the table allows you to answer
    this question.

> No it is not. They included an F test p-value for the difference of
> the coefficients.

6.  Notice that Table 4C is described as results from “fully saturated”
    models. What does this mean? Looking at the list of variables in the
    table, explain in what sense the model is “saturated.”

> A fully saturated model is a model that has as many parameters as data
> points. This can be seen in the number of paremeters the model has. In
> this case, the model contains a dummy variable (or indicator variable)
> for every combination of variables.

# 4\. Now\! Do it with data

Download the data set for the recycling study in the previous problem,
obtained from the authors. We’ll be focusing on the outcome variable
Y=“number of bins turned in per week” (avg\_bins\_treat).

``` r
d <- foreign::read.dta("../data/karlan_data_subset_for_class.dta")
d <- data.table(d)
head(d)
```

    ##    street havecell avg_bins_treat base_avg_bins_treat bin sms bin_s bin_g sms_p
    ## 1:      7        1      1.0416666               0.750   1   1     1     0     0
    ## 2:      7        1      0.0000000               0.000   0   1     0     0     1
    ## 3:      7        1      0.7500000               0.500   0   0     0     0     0
    ## 4:      7        1      0.5416667               0.500   0   0     0     0     0
    ## 5:      6        1      0.9583333               0.375   1   0     0     1     0
    ## 6:      8        0      0.2083333               0.000   1   0     0     1     0
    ##    sms_g
    ## 1:     1
    ## 2:     0
    ## 3:     0
    ## 4:     0
    ## 5:     0
    ## 6:     0

``` r
library(sandwich)
library(lmtest)

## Do some quick exploratory data analysis with this data. 
## There are some values in this data that seem a bit strange. 

## Determine what these are. 
## Don't make an assessment about keeping, changing, or 
## dropping just yet, but at any point that your analysis touches 
## these variables, you'll have to determine what is appropriate 
## given the analysis you are conducting. 
```

``` r
summary(d)
```

    ##      street           havecell      avg_bins_treat   base_avg_bins_treat
    ##  Min.   :-999.00   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000     
    ##  1st Qu.:  69.00   1st Qu.:0.0000   1st Qu.:0.4167   1st Qu.:0.3750     
    ##  Median : 131.50   Median :1.0000   Median :0.6250   Median :0.6250     
    ##  Mean   :  68.81   Mean   :0.5908   Mean   :0.6811   Mean   :0.7363     
    ##  3rd Qu.: 215.00   3rd Qu.:1.0000   3rd Qu.:0.8333   3rd Qu.:1.0000     
    ##  Max.   : 263.00   Max.   :1.0000   Max.   :4.1667   Max.   :6.3750     
    ##  NA's   :3         NA's   :1                                            
    ##       bin              sms             bin_s            bin_g       
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
    ##  Mean   :0.3378   Mean   :0.3087   Mean   :0.1681   Mean   :0.1697  
    ##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
    ##                                                                     
    ##      sms_p            sms_g       
    ##  Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median :0.0000   Median :0.0000  
    ##  Mean   :0.1557   Mean   :0.1529  
    ##  3rd Qu.:0.0000   3rd Qu.:0.0000  
    ##  Max.   :1.0000   Max.   :1.0000  
    ## 

1.  For simplicity, let’s start by measuring the effect of providing a
    recycling bin, ignoring the SMS message treatment (and ignoring
    whether there was a sticker on the bin or not). Run a regression of
    Y on only the bin treatment dummy, so you estimate a simple
    difference in means. Provide a 95% confidence interval for the
    treatment effect, using **of course** robust standard errors (use
    these throughout).

<!-- end list -->

``` r
d <- d[!is.na(street), ]
mod_1 <- d[, lm(avg_bins_treat ~ bin)]
summary(mod_1)
```

    ## 
    ## Call:
    ## lm(formula = avg_bins_treat ~ bin)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7692 -0.2609 -0.0526  0.1891  3.5308 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.63589    0.01175  54.137  < 2e-16 ***
    ## bin          0.13332    0.02023   6.591 5.72e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4037 on 1780 degrees of freedom
    ## Multiple R-squared:  0.02383,    Adjusted R-squared:  0.02328 
    ## F-statistic: 43.45 on 1 and 1780 DF,  p-value: 5.722e-11

``` r
#robust SE conf.int
coefci(mod_1, level = 0.95, vcov. = vcovHC, 'bin')
```

    ##          2.5 %    97.5 %
    ## bin 0.09273598 0.1738969

> The results for bin are highly statistically significant with
> coefiecient 0.13332 (0.02023).

2.  Now add the pre-treatment value of Y as a covariate. Provide a 95%
    confidence interval for the treatment effect. Explain how and why
    this confidence interval differs from the previous one.

<!-- end list -->

``` r
mod_2 <- d[, lm(avg_bins_treat ~ bin + base_avg_bins_treat)]
summary(mod_2)
```

    ## 
    ## Call:
    ## lm(formula = avg_bins_treat ~ bin + base_avg_bins_treat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.99790 -0.21372 -0.02543  0.16751  2.13378 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.35206    0.01376  25.593  < 2e-16 ***
    ## bin                  0.12424    0.01667   7.454  1.4e-13 ***
    ## base_avg_bins_treat  0.39000    0.01343  29.035  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3326 on 1779 degrees of freedom
    ## Multiple R-squared:  0.3377, Adjusted R-squared:  0.3369 
    ## F-statistic: 453.5 on 2 and 1779 DF,  p-value: < 2.2e-16

``` r
coefci(mod_2, level = 0.95, vcov. = vcovHC, 'bin')
```

    ##          2.5 %    97.5 %
    ## bin 0.09056388 0.1579208

> Adding the `base_avg_bins_treat` makes the confidence interval
> substantially more narrow. This occurs because `base_avg_bins_treat`
> explains a sizeable portion of the variance. Having a measurement for
> a past occurence `base_avg_bins_treat`, largely explains the future
> event.

3.  Now add the street fixed effects. (You’ll need to use the R command
    factor().) Provide a 95% confidence interval for the treatment
    effect.

<!-- end list -->

``` r
mod_3 <- d[, lm(avg_bins_treat ~ bin + base_avg_bins_treat + factor(street))]
coeftest(mod_3, vcov = vcovHC(mod_3, type = "HC0"))[2, ]
```

    ##     Estimate   Std. Error      t value     Pr(>|t|) 
    ## 1.138868e-01 1.659533e-02 6.862579e+00 9.637996e-12

``` r
coefci(mod_3, level = 0.95, vcov. = vcovHC, "bin")
```

    ##          2.5 %   97.5 %
    ## bin 0.07684258 0.150931

> The confidence intervals for the bin show that the effect of bin is in
> fact statitically significant since the confidence interval does not
> include 0. The coeficient is 0.1138868 (0.0165953) and the p-value is
> highly significant.

4.  Recall that the authors described their experiment as “stratified at
    the street level,” which is a synonym for blocking by street. Does
    including these block fixed effects change the standard errors of
    the estimates *very much*? Conduct the appropriate test for the
    inclusion of these block fixed effects, and interpret them in the
    context of the other variables in the regression.

\#do anova between the
models

``` r
mod_4 <- d[, lm(avg_bins_treat ~ bin + base_avg_bins_treat + factor(street))]
coeftest(mod_4, vcov = vcovHC(mod_4, type = "HC0"))[2, ] #coefficient and robust se for mod_4
```

    ##     Estimate   Std. Error      t value     Pr(>|t|) 
    ## 1.138868e-01 1.659533e-02 6.862579e+00 9.637996e-12

``` r
coefci(mod_4, level = 0.95, vcov. = vcovHC, "bin")
```

    ##          2.5 %   97.5 %
    ## bin 0.07684258 0.150931

``` r
test_fixed_effects <- anova(mod_4, mod_2, test = "F")
test_fixed_effects
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: avg_bins_treat ~ bin + base_avg_bins_treat + factor(street)
    ## Model 2: avg_bins_treat ~ bin + base_avg_bins_treat
    ##   Res.Df    RSS   Df Sum of Sq      F    Pr(>F)    
    ## 1   1600 167.50                                    
    ## 2   1779 196.78 -179   -29.278 1.5624 9.514e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> No they have practically no impact on the standard error. My
> hypothesis is that since the variable `base_avg_bins_treat` is
> included, the street level variations explain **very little**
> variance. Howeover, the F test comparing the two models does show that
> model 4 explains a higher percentage of the variation as seen in the
> RSS and the significant p-value.

5.  Perhaps having a cell phone helps explain the level of recycling
    behavior. Instead of “has cell phone,” we find it easier to
    interpret the coefficient if we define the variable " no cell
    phone." Give the R command to define this new variable, which equals
    one minus the “has cell phone” variable in the authors’ data set.
    Use “no cell phone” instead of “has cell phone” in subsequent
    regressions with this dataset.

<!-- end list -->

``` r
d <- d[, no_cell := 1 - havecell]
```

6.  Now add “no cell phone” as a covariate to the previous regression.
    Provide a 95% confidence interval for the treatment effect. Explain
    why this confidence interval does not differ much from the previous
    one.

<!-- end list -->

``` r
mod_5 <- d[, lm(avg_bins_treat ~ bin + base_avg_bins_treat + factor(street) + no_cell)] # add missing var
coeftest(mod_5, vcov = vcovHC(mod_4, type = "HC0"))[2, ] #coefficient and robust se
```

    ##     Estimate   Std. Error      t value     Pr(>|t|) 
    ## 1.151007e-01 1.659533e-02 6.935730e+00 5.848442e-12

``` r
coef_5 <- coefci(mod_5, level = 0.95, vcov. = vcovHC, 'bin')
coef_5
```

    ##          2.5 %    97.5 %
    ## bin 0.07801276 0.1521887

> The confidence interval is `coef_5`. The confidence interval does not
> change much because the `base_avg_bins_treat` variable explains much
> of the current performance based on the past performance. This
> variable explains a great deal of the variance. Adding the `nocell`
> variable fails to narrow the CI much since `base_avg_bins_treat`
> already explains much of the variance. This can also be seen in the
> fact that the standard error remains very close to the previous one.

7.  Now let’s add in the SMS treatment. Re-run the previous regression
    with “any SMS” included. You should get the same results as in Table
    4A. Provide a 95% confidence interval for the treatment effect of
    the recycling bin. Explain why this confidence interval does not
    differ much from the previous
one.

<!-- end list -->

``` r
mod_6 <- d[, lm(avg_bins_treat ~ bin + base_avg_bins_treat + factor(street)+no_cell + sms)]
#summary(mod_6)
coeftest(mod_6, vcov = vcovHC(mod_6, type = "HC0"))[2, ] #coefficient and robust se
```

    ##     Estimate   Std. Error      t value     Pr(>|t|) 
    ## 1.150536e-01 1.660434e-02 6.929132e+00 6.120805e-12

``` r
coefci(mod_6, level = 0.95, vcov. = vcovHC, 'bin')
```

    ##         2.5 %    97.5 %
    ## bin 0.0779404 0.1521669

> Like the previous question the inclussion of sms makes very little
> difference since the other variables already explain most of the
> variance.

8.  Now reproduce the results of column 2 in Table 4B, estimating
    separate treatment effects for the two types of SMS treatments and
    the two types of recycling-bin treatments. Provide a 95% confidence
    interval for the effect of the unadorned recycling bin. Explain how
    your answer differs from that in part (g), and explain why you think
    it
differs.

<!-- end list -->

``` r
mod_7 <-  d[, lm(avg_bins_treat ~ bin_s + bin_g + sms_p+ sms_g + no_cell + base_avg_bins_treat)]
coeftest(mod_7, vcov = vcovHC(mod_7, type = "HC0")) #coefficient and robust se
```

    ## 
    ## t test of coefficients:
    ## 
    ##                      Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)          0.388190   0.024409 15.9035 < 2.2e-16 ***
    ## bin_s                0.140400   0.021402  6.5601 7.037e-11 ***
    ## bin_g                0.110614   0.022971  4.8154 1.594e-06 ***
    ## sms_p               -0.042967   0.025397 -1.6918  0.090860 .  
    ## sms_g               -0.016989   0.025628 -0.6629  0.507489    
    ## no_cell             -0.064549   0.019771 -3.2648  0.001116 ** 
    ## base_avg_bins_treat  0.388873   0.029533 13.1672 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
coefci(mod_7, level = 0.95, vcov. = vcovHC, 'bin_g')
```

    ##            2.5 %    97.5 %
    ## bin_g 0.06525063 0.1559779

> This model includes more specific covariates than in part g. Now this
> regression includes a covariate for bin with sticker and without. This
> slightly shifted the estimate for bins. I think the results from this
> regression differ since the effect of bin and sms are broken into two
> sub-categories so inheritantly their impact is different.

# 5\. A Final Practice Problem

Now for a fictional scenario. An emergency two-week randomized
controlled trial of the experimental drug ZMapp is conducted to treat
Ebola. (The control represents the usual standard of care for patients
identified with Ebola, while the treatment is the usual standard of care
plus the drug.)

Here are the (fake) data.

``` r
d <- fread("../data/Ebola_rct2.csv")
head(d)
```

    ##    temperature_day0 dehydrated_day0 treat_zmapp temperature_day14
    ## 1:         99.53168               1           0          98.62634
    ## 2:         97.37372               0           0          98.03251
    ## 3:         97.00747               0           1          97.93340
    ## 4:         99.74761               1           0          98.40457
    ## 5:         99.57559               1           1          99.31678
    ## 6:         98.28889               1           1          99.82623
    ##    dehydrated_day14 male
    ## 1:                1    0
    ## 2:                1    0
    ## 3:                0    1
    ## 4:                1    0
    ## 5:                1    0
    ## 6:                1    1

You are asked to analyze it. Patients’ temperature and whether they are
dehydrated is recorded on day 0 of the experiment, then ZMapp is
administered to patients in the treatment group on day 1. Dehydration
and temperature is again recorded on day 14.

1.  Without using any covariates, answer this question with regression:
    What is the estimated effect of ZMapp (with standard error in
    parentheses) on whether someone was dehydrated on day 14? What is
    the p-value associated with this estimate?

<!-- end list -->

``` r
mod_1 <- d[, lm(dehydrated_day14 ~ treat_zmapp)]
#robust CI
confintm1 <- coefci(mod_1, level = 0.95, vcov. = vcovHC, 'treat_zmapp')

#robust SE
se1 <- coeftest(mod_1, vcov = vcovHC(mod_1, type = "HC0"))
se1
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  0.847458   0.046809 18.1046 < 2.2e-16 ***
    ## treat_zmapp -0.237702   0.089414 -2.6584  0.009169 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
confintm1
```

    ##                  2.5 %      97.5 %
    ## treat_zmapp -0.4191999 -0.05620316

> The estimated impact of the zmapp treatment is -0.23770 (0.089414)
> with a p-value of 0.009169. The heterogenous robust standard errors
> are `confintm1`. The results are statistically significant at the 0.05
> level.

2.  Add covariates for dehydration on day 0 and patient temperature on
    day 0 to the regression from part (a) and report the ATE (with
    standard error). Also report the
p-value.

<!-- end list -->

``` r
mod_2 <- d[, lm(dehydrated_day14 ~ treat_zmapp + dehydrated_day0 + temperature_day0)]

confintm2 <- coefci(mod_2, level = 0.95, vcov. = vcovHC, 'treat_zmapp')

se2 <- coeftest(mod_2, vcov = vcovHC(mod_2, type = "HC0"))
se2
```

    ## 
    ## t test of coefficients:
    ## 
    ##                    Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)      -19.469655   7.225573 -2.6945 0.008321 **
    ## treat_zmapp       -0.165537   0.078383 -2.1119 0.037291 * 
    ## dehydrated_day0    0.064557   0.169357  0.3812 0.703904   
    ## temperature_day0   0.205548   0.074138  2.7725 0.006683 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
confintm2
```

    ##                  2.5 %      97.5 %
    ## treat_zmapp -0.3282588 -0.00281468

> The ATE is -0.165537 (0.078383) with P-value 0.037291. The confidence
> interval is `confintm2`. The results are statistically significant at
> the 0.05 level.

3.  Do you prefer the estimate of the ATE reported in part (a) or part
    (b)? Why? Report the results of the F-test that you used to form
    this opinion.

<!-- end list -->

``` r
test_object <- anova(mod_1, mod_2, test = 'F')
test_object
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: dehydrated_day14 ~ treat_zmapp
    ## Model 2: dehydrated_day14 ~ treat_zmapp + dehydrated_day0 + temperature_day0
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1     98 17.383                                  
    ## 2     96 12.918  2    4.4653 16.592 6.472e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> I prefer the estimated ATE from part B. The RSS from model 2 is
> substantially lower than model 1 meaning the model better explains the
> data. This likely occurs since the `dehydrated_day0` and
> `temperature_day0` likely explain `dehydrated_day14` effectively.
> Moreover, the P-value is highly significant.

4.  The regression from part (2) suggests that temperature is highly
    predictive of dehydration. Add, temperature on day 14 as a covariate
    and report the ATE, the standard error, and the
p-value.

<!-- end list -->

``` r
mod_3 <- d[, lm(dehydrated_day14 ~ treat_zmapp + dehydrated_day0 + temperature_day0 + temperature_day14)]

confintm3 <- coefci(mod_3, level = 0.95, vcov. = vcovHC, 'treat_zmapp')

se3 <- coeftest(mod_3, vcov = vcovHC(mod_3, type = "HC0"))
se3
```

    ## 
    ## t test of coefficients:
    ## 
    ##                     Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)       -22.591585   7.274397 -3.1056 0.002503 **
    ## treat_zmapp        -0.120101   0.081469 -1.4742 0.143738   
    ## dehydrated_day0     0.046038   0.163185  0.2821 0.778464   
    ## temperature_day0    0.176642   0.072474  2.4373 0.016658 * 
    ## temperature_day14   0.060148   0.024227  2.4827 0.014792 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
confintm3
```

    ##                  2.5 %     97.5 %
    ## treat_zmapp -0.2904317 0.05023039

> The ATE is -0.12010 (0.081469) with P-value 0.143738. The robust
> confidence interval is `confintm3`. The results for the treatment are
> no longer significant.

5.  Do you prefer the estimate of the ATE reported in part (b) or part
    (d)? What is this preference based on?

<!-- end list -->

``` r
anova(mod_2, mod_3, test = "F")
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: dehydrated_day14 ~ treat_zmapp + dehydrated_day0 + temperature_day0
    ## Model 2: dehydrated_day14 ~ treat_zmapp + dehydrated_day0 + temperature_day0 + 
    ##     temperature_day14
    ##   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
    ## 1     96 12.918                              
    ## 2     95 12.372  1   0.54609 4.1933 0.04335 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> Based on the results of the F test, model 3 has a slightly lower RSS
> which is a good thing. However, I think there is some possibility that
> day 14 temperature is a bad control since it could potentially be
> associated with the treatment. For this reason, I think part b is
> better.

6.  Now let’s switch from the outcome of dehydration to the outcome of
    temperature, and use the same regression covariates as in the chunk
    titled `add pre-treatment measures`. Test the hypothesis that ZMapp
    is especially likely to reduce mens’ temperatures, as compared to
    womens’, and describe how you did so. What do the results
suggest?

<!-- end list -->

``` r
mod_4 <- d[, lm(temperature_day14 ~ treat_zmapp+ dehydrated_day0 + temperature_day0)]
confintm4 <- coefci(mod_4, level = 0.95, vcov. = vcovHC, 'treat_zmapp')
confintm4
```

    ##                 2.5 %     97.5 %
    ## treat_zmapp -1.206335 -0.3044687

``` r
se4 <- coeftest(mod_4, vcov = vcovHC(mod_4, type = "HC0"))
se4
```

    ## 
    ## t test of coefficients:
    ## 
    ##                  Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)      51.90391   24.75001  2.0971 0.0386105 *  
    ## treat_zmapp      -0.75540    0.21838 -3.4591 0.0008099 ***
    ## dehydrated_day0   0.30789    0.47651  0.6461 0.5197297    
    ## temperature_day0  0.48059    0.25422  1.8905 0.0617101 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
confintm4
```

    ##                 2.5 %     97.5 %
    ## treat_zmapp -1.206335 -0.3044687

> The results suggest that `treat_zmapp` has a negative impact on
> `temperature_day14`. The coefficient is -0.7554 (0.21838) with a
> p-value of 0.0008099. This is a highly significant finding.

7.  Which group – those that are coded as `male == 0` or `male == 1`
    have better health outcomes in control? What about in treatment? How
    does this help to contextualize whatever heterogeneous treatment
    effect you might have
estimated?

<!-- end list -->

``` r
mod_5 <- d[, lm(temperature_day14 ~ treat_zmapp+ dehydrated_day0 + temperature_day0 + treat_zmapp * male)]
confintm5 <- coefci(mod_5, level = 0.95, vcov. = vcovHC, 'treat_zmapp')
confintm5
```

    ##                  2.5 %     97.5 %
    ## treat_zmapp -0.4656964 0.00396531

``` r
se5 <- coeftest(mod_5, vcov = vcovHC(mod_5, type = "HC0"))
se5
```

    ## 
    ## t test of coefficients:
    ## 
    ##                   Estimate Std. Error  t value  Pr(>|t|)    
    ## (Intercept)      48.712690   9.528870   5.1121 1.672e-06 ***
    ## treat_zmapp      -0.230866   0.111476  -2.0710    0.0411 *  
    ## dehydrated_day0   0.041131   0.181634   0.2264    0.8213    
    ## temperature_day0  0.504797   0.097690   5.1674 1.329e-06 ***
    ## male              3.085486   0.114275  27.0005 < 2.2e-16 ***
    ## treat_zmapp:male -2.076686   0.185004 -11.2251 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
confintm5
```

    ##                  2.5 %     97.5 %
    ## treat_zmapp -0.4656964 0.00396531

> In the control group, non-males have better health outcomes. In the
> treatment group, males also have worse health outcomes. This is
> evident based on the fact the the coefficients for both the treatment
> effect in males and the treatment effect fail to be larger than the
> negative impact of being male. Yes this helps substantially. This
> gives us an idea of how the treatment works in males vs non-males.
> Although the health outcomes for males are worse, the treatment effect
> is actually larger for them.

8.  Suppose that you had not run the regression in part (7). Instead,
    you speak with a colleague to learn about heterogeneous treatment
    effects. This colleague has access to a non-anonymized version of
    the same dataset and reports that they looked at heterogeneous
    effects of the ZMapp treatment by each of 80 different covariates to
    examine whether each predicted the effectiveness of ZMapp on each of
    20 different indicators of health. Across these regressions your
    colleague ran, the treatment’s interaction with gender on the
    outcome of temperature is the only heterogeneous treatment effect
    that he found to be statistically significant. They reason that this
    shows the importance of gender for understanding the effectiveness
    of the drug, because nothing else seemed to indicate why it worked.
    Bolstering your colleague’s confidence, after looking at the data,
    they also returned to his medical textbooks and built a theory about
    why ZMapp interacts with processes only present in men to cure.
    Another doctor, unfamiliar with the data, hears your colleague’s
    theory and finds it plausible. How likely do you think it is ZMapp
    works especially well for curing Ebola in men, and why? (This
    question is conceptual can be answered without performing any
    computation.)

> This sounds like a very likely **fishing expedition**. Performing
> repeated tests as described greatly increases the chances of having a
> falsely significant result. In this case, there is almost a 100%
> chance that one covariate will be significant at the 0.05 level just
> by chance. If repeated tests cannot be avoided, a correction, such as
> the Bonferoni correction must be applied. It is bad scientific
> practice to look for theory after a significant result.

9.  Now, imagine that your colleague’s fishing expedition did not
    happen, but that you had tested this heterogeneous treatment effect,
    and only this heterogeneous treatment effect, of your own accord.
    Would you be more or less inclined to believe that the heterogeneous
    treatment effect really exists? Why?

> I would definitely be more inclined to believe it. Since repeated
> experimentation did not occur, it is far more likely that the results
> in fact are significant. Regardless, it would be useful to know if
> biological mechanisms exist which would make the treatment more
> effective in men.

10. Now, imagine that your colleague’s fishing expedition **did**
    happen, but that you on your own tested this and only this HTE,
    discover a positive result and conclude there is an effect. How does
    your colleague’s behavior change the interpretation of your test?
    Does this seem fair or reasonable?

> I still would not trust the results. Given that the data is the same,
> the test would also be significant even if the results were a false
> positive. Conducting the statistical research like this would
> essentially be using the fishing expedition to guide a second test. A
> correct way to test this would be to collect data in a new trial and
> see if the theory holds up.

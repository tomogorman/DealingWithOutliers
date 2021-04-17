This file describes the output from a computer program that was
used to estimate the signifiance level and the power of the 
Outliers test. The Outliers test is described in:

O'Gorman, T. W. (2021) Increasing the power of tests of signifiance when 
outliers are present. (Submitted for publication.)

The R code that can be used to perform the test is on the file:

  outliers.test.r

and instructions for using that code are given in the first 
part of that file.

In addition to that R code, I have included the simulation 
results to document the level of significance (size) and the 
power of the proposed test.

The following notation is used in these files:

alpha = 0.05 was used as the nominal level of significance for all tests
.
"n of permutations" was not used in these simulations.

rho   = the correlation between the covariates in the model.

ic    = the number of parameters in the full model, including 
        the intercept.

icred = number of parameters in the reduced model.

n     = number of observations.

"Distribution" is the distribution of the errors that were generated.

"t" the traditional t test.

"Adaptive" is the test based on the adaptive test in O'Gorman (2012) 
 
"ws" is the test based on weights calculated without smoothing. 

"ws-t" is the level of significance for the ws test minus that of the t test.


The first group of files give the estimates of the significance level.

In each of these simulations 10000 data sets were generated to accurately
estimate the significance level of the proposed ws test. For each of these 
data sets the traditional t test and the ws tests were performed 
and the percent of the data sets that caused the null hypothesis 
to be rejected are tabulated.


The files are:

|        File name     |              Model|
|----------------------|:------------------:|
|ws.size.ic2.bal.txt   |For two equal samples from two populations|
|ws.size.ic2.nor.txt   |Simple Linear Regression Model with normal covariate|
|ws.size.ic2.ln .txt   |Simple Linear Regression Model with lognormal covariate|
|ws.size.ic3.nor.txt   |Model with 2 normal covariates correlated r = 0.0, 0.4, 0.8|
|ws.size.ic3.ln.txtt   |Model with 2 lognormal covariates correlated r = 0.0, 0.4, 0.8|
|ws.size.ic5.nor.txt   |Model with 4 normal covariates correlated r = .4|
|ws.size.ic5.ln.txt    |Model with 4 lognormal covariates correlated r = .4| 
|ws.size.ic4.txt       |Model with 3 categorical predictor variables|



The second group of files give the estimates of the power of the ws test.
In each of these simulations 1000 data sets were generated.
 

The files are:

|        File name      |              Model |
|-----------------------|:------------------:|
|ws.power.ic2.bal.txt   |For two equal samples from two populations|
|ws.power.ic2.nor.txt   |Simple Linear Regression Model with normal covariate|
|ws.power.ic2.ln.txt    |Simple Linear Regression Model with lognormal covariate|
|ws.power.ic3.nor.txt   |Model with 2 normal covariates correlated r = 0.0, 0.4, 0.8|
|ws.power.ic3.ln.txt    |Model with 2 lognormal covariates correlated r = 0.0, 0.4, 0.8|
|ws.power.ic5.nor.txt   |Model with 4 normal covariates correlated r = .4|
|ws.power.ic5.ln.txt    |Model with 4 lognormal covariates correlated r = .4|
|ws.power.ic4.txt       |Model with 3 categorical predictor variables|






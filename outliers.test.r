#
#
#                                       Program:  outliers.test.r
#                                 Revision Date:  April 17, 2021
#
#  This outliers.test function computes a p-value for any single 
#  coefficients in a linear model having fixed effects.
#  This function cannot be used to test any subset of coefficients
#  in a linear model; only one coefficients can be tested at a time.
#  The observations are weighted by default. 
#
#  References: 
#
#
#  The outliers.test is performed by calling the function
#
#       outliers.test<- function(dftest, complete, reduced,
#         r = 2000, s1=23932, s2=12993, s3=8932, details=1)
#
#  The function arguments are:
#
#    1)  dftest is an R data frame that includes all of the variables that
#        will be used in the analysis.
#    2)  complete is a character string that specifies the complete model. 
#        This model uses the same syntax as the lm() function.
#    3)  reduced is the model that is specified under the null 
#        hypothesis. It contains all the terms in the complete model except
#        the single variable that is to be tested.
#    4)  r is the number of permutations used in the test. Note that
#        r = 2000 is the default.
#    5)  s1 is one of three random number seeds. It can be any integer
#        in the range of 1 to 30268.
#    6)  s2 is another random number seed in the range of 1 to 30306.
#    7)  s3 is the last random number seed in the range of 1 to 30322.
#    8)  if details = 1 (default) the details will be printed.
#        if details = 0 the p-value will be returned by the function, but
#        no output will be printed.
#        if details = 2 the weights given to the unpermuted data 
#        in addition to the details = 1 output.
#
#  Notes:
#
#    1) The first argument must be the data set name, the second argument
#       must be the complete model, the third model is the reduced model,
#       the remaining arguments, if any, must be specified by
#       their complete names, as shown in the examples below.
#    2) The first three arguments are required.
#    3) The data frame cannot contain missing values for any variables
#       used in the complete model.
#    4) This outliers.test function calls the outliers, outliersweights,
#       and shufflewh functions.
#    5) This function is written in base R.  No packages are required. 
#
#  Examples:
#
#    If blood pressure data is used to create a data frame (dfbp) that has
#    blood pressure (bp), age (age), and a treatment indicator (group),
#    then we could compute a p-value  for the group effect by
#    using this code:
#
#      source("outliers.test.r")
#      bplimits <- outliers.test(dftest = dfbp, complete = c("bp~group",
#                                reduced = c("bp~1") )
#
#    We could expand this example if we needed to include age as a
#    covariate and wanted to specify the random number seeds.
#
#      source("outliers.test.r")
#      bplimits <- outliers.test(dftest=dfbp, complete = c("bp ~ age + group"),
#                     reduced = c("bp ~ age")  
#                     s1 = 3682, s2 = 27812, s3 = 12973 )
#
#  These R functions were carefully checked and I believe
#  that the functions are correct.  However, the author is not
#  responsible for any errors that may still exist in the code.
#
#  Please report any issues concerning this code to T. W. O'Gorman via 
#  email at twogorman@gmail.com
#

outliers.test <- function(dftest, complete, reduced, r=2000,
                 s1=7832, s2=25933, s3=19857, details=1) {
                  
vars <- strsplit(complete,"~")[[1]]
depvar <- vars[[1]]
vars <- strsplit(complete,"+",fixed=TRUE)[[1]]
nvars <- length(vars)               

depvar  <- gsub(" ","",depvar)
reduced <- gsub(" ","",reduced)

n <- length(dftest[,depvar])
                  
# The value of r specifies the number of permutations.

if(details >= 1) {
  cat("\n")
  cat("Function arguments for the outliers.test function:","\n\n")
  cat("  Data frame:",deparse(substitute(dftest)),"\n","\n")
  cat("  Dependent variable: ",depvar, "\n")
  cat("  Complete model: ",complete,"\n")
  cat("  Reduced model : ",reduced,"\n")
  cat("  Number of permutations = ",r,"\n")
  cat("  Random seeds = ",s1,s2, s3, "\n","\n")
                  }  
if( (s1 < 1) | (s1 > 30268) ) stop("s1 must be in the range of 1 to 30268.")
if( (s2 < 1) | (s2 > 30306) ) stop("s2 must be in the range of 1 to 30306.")
if( (s3 < 1) | (s3 > 30322) ) stop("s3 must be in the range of 1 to 30322.")
if( (details < 0) | (details > 2) ) stop("details must be 0, 1, or 2.")

pvalueslist <- outliers(dftest,depvar,complete,reduced,r,
                        s1,s2,s3,n,details)
  pvalue <- pvalueslist[[1]]
  s1     <- pvalueslist[[2]]
  s2     <- pvalueslist[[3]]
  s3     <- pvalueslist[[4]]
  if(details >= 1) cat("  p-value = ", pvalue, "\n\n")
                                                                              
  return(pvalue)   
                                                                  }
#                                                                 
#  The outliers function produces a two-tailed p-value
#  based on F test statistics from a weighted model.
#
#  If the reduced model contains only an intercept term, the weights
#  will be permuted, rather than computed from the permuted data.
#

outliers <- function(dfoutliers, depvar, complete, reduced, r,
                    s1, s2, s3, n, details)                     {
localwt             <- double(n)
dfoutliers$w2      <- double(n)
dfoutliers$w       <- double(n)

redu <- lm(as.formula(reduced), data = dfoutliers)
yhat <- predict(redu)
yresidual <- residuals(redu)


  dfoutliers    <- outliersweights(dfoutliers,reduced)
  if(details == 2){
    cat("\n\n")
    cat(" The weights for the unpermuted data.",
        "\n\n")
    print(dfoutliers, digits=3)
    cat("\n\n")
                   }
 
compu <- lm(as.formula(complete), data = dfoutliers, weights = dfoutliers$w2)

    compwc  <- lm(as.formula(complete), data = dfoutliers,
               weights = dfoutliers$w2)
    ssec <- deviance(compwc)
    dfc <- df.residual(compwc)

    compwr  <- lm(as.formula(reduced), data = dfoutliers,
               weights = dfoutliers$w2)
    sser <- deviance(compwr)
    dfr <- df.residual(compwr)

    funperm <- ( (sser -ssec)/(dfr-dfc))/(ssec/dfc)

if(details >= 1) cat("  F test statistic for unpermuted data = ", funperm, "\n")

e <- 0

dfoutliers$w2perm  <- double(n)

simple <- paste(depvar,c("~1"),sep="")

#  Simple models have the intercept as the only predictor variable in
#  the reduced model.  In these models the weights do not need
#  to be recomputed, they can be permuted because the recomputed weights
#  will equal the permuted weights.

if( (reduced == simple)  )        {
  countnum <- double(n)
  ynew     <- double(n)
  yresshuf <- double(n)
  dfoutliers$w2perm <- double(n)
  countnum <- c(1:n)
  for (k in 1:r)     {
    permlist <- shufflewh(countnum,s1,s2,s3,n)
    s1 <- permlist[[2]]
    s2 <- permlist[[3]]
    s3 <- permlist[[4]]

    permnums <- permlist[[1]]
    yresshuf <- yresidual[permnums]
    dfoutliers$w2perm <- dfoutliers$w2[permnums]

    dfoutliers[,depvar] <- yhat + yresshuf

    compwc  <- lm(as.formula(complete), data = dfoutliers,
               weights = dfoutliers$w2perm)
    ssec <- deviance(compwc)
    dfc <- df.residual(compwc)

    compwr  <- lm(as.formula(reduced), data = dfoutliers,
               weights = dfoutliers$w2perm)
    sser <- deviance(compwr)
    dfr <- df.residual(compwr)

    fperm <- ( (sser -ssec)/(dfr-dfc))/(ssec/dfc)

    if( fperm > funperm)  e  <-  e + 1
    if((details==2) & (k <=20) )cat("k, fperm, funperm, e = ",k,fperm,funperm,e,"\n")
                             }
  p <- (e+1)/(r+1)
  plist <- list(p,s1,s2,s3)
  return(plist)
                                    }

if( ( reduced != simple ) )     {
  for (k in 1:r) {
    sresidlist <- shufflewh(yresidual,s1,s2,s3,n)
    s1 <- sresidlist[[2]]
    s2 <- sresidlist[[3]]
    s3 <- sresidlist[[4]]

    dfoutliers[,depvar] <- yhat+sresidlist[[1]]
    dfoutliers    <- outliersweights(dfoutliers,reduced)
              
    compwc  <- lm(as.formula(complete), data = dfoutliers,
               weights = dfoutliers$w2)
    ssec <- deviance(compwc)
    dfc <- df.residual(compwc)

    compwr  <- lm(as.formula(reduced), data = dfoutliers,
               weights = dfoutliers$w2)
    sser <- deviance(compwr)
    dfr <- df.residual(compwr)
#
    fperm <- ( (sser -ssec)/(dfr-dfc))/(ssec/dfc)
    if(fperm > funperm) e  <-  e + 1
if( (details == 2) & (k <= 20)) cat(" F perm, F unperm = ", fperm, funperm, "\n")
                                     }                                             
p <- (e+1)/(r+1)
plist <- list(p,s1,s2,s3)
return(plist)
                                            }
                  }
#  The outliersweights function produces weights for observations
#  based on residuals from the reduced model.

outliersweights <- function(dfwts,reduced) {

red <- lm(as.formula(reduced), data=dfwts)

dfwts$resid <- residuals(red)
#                               compute traditional quantiles
probs <- c(0.25, 0.50, 0.75)
q <- quantile(dfwts$resid,probs,type=6)
q25 <- q[1]; q50 <- q[2]; q75 <- q[3]
iqr <- q75 - q25
sigma <- iqr/1.349
#cat("iqr, sigma = ",iqr, sigma, "\n")
n <- length(dfwts$resid)
weights  <- double(n)
#                               compute weights

dfwts$s <- (dfwts$resid-q50)/sigma
dfwts$resrnk <- rank(dfwts$resid)
ridits <- dfwts$resrnk/(n+1)
dfwts$z <- qnorm(ridits)

for (k in 1:n){
  dfwts$w[k] <-1
  if( (dfwts$s[k] < -0.001) | (dfwts$s[k] >0.001) ) weights[k]<- dfwts$z[k]/dfwts$s[k] 
               }

dfwts$w <- pmin(weights,1)

dfwts$w2 <- dfwts$w*dfwts$w 

return(dfwts)
                                                }

#   The shufflewh function computes n-1 uniform random numbers
#   using the Wichmann-Hill method and then performs the Durstenfeld
#   shuffle on the rows of y. The vector y and the random number
#   seeds are returned in a list.

shufflewh <- function(y, s1, s2, s3, n) {

k <- c(1:n)
yold <- double(n)
yold <- y
for (i in n:2)            {
  s1 <- (171*s1) %% 30269
  s2 <- (172*s2) %% 30307
  s3 <- (170*s3) %% 30323
  u  <- (s1/30269.0 + s2/30307.0 + s3/30323.0) %% 1.000000
  itrade      <- floor(i*u + 1) 
  ktemp       <- k[itrade]
  k[itrade]   <- k[i]
  k[i]        <- ktemp
                          }
for (i in 1:n)  {
  iplace <- k[i]
  y[iplace] <- yold[i]
                 }
yandseeds <- list(y, s1, s2, s3)
return(yandseeds)
                                         }



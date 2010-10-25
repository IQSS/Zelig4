\name{Zelig-package}
\alias{Zelig-package}
\alias{Zelig}
\docType{package}
\title{
Zelig: Everyone's Statistical Software
}
\description{
Zelig is an easy-to-use program that can estimate, and
help interpret the results of, an enormous range of statistical
models. It literally is ``everyone's statistical software''
because Zelig's simple unified framework incorporates everyone
else's (R) code. We also hope it will become ``everyone's
statistical software'' for applications and teaching, and so
have designed Zelig so that anyone can easily use it or add
their programs to it.  Zelig also comes with infrastructure
that facilitates the use of any existing method, such as by
allowing multiply imputed data for any model, and mimicking the
program Clarify (for Stata) that takes the raw output of
existing statistical procedures and translates them into
quantities of direct interest.
}
\details{
\tabular{ll}{
Package: \tab Zelig\cr
Version: \tab 2.8-5\cr
Date: \tab 2007-06-12\cr
Depends: \tab R (>= 2.4.0), MASS, boot\cr
Suggests: \tab VGAM (>= 0.7-1), MCMCpack (>= 0.7-4), mvtnorm, survival,
sandwich (>= 2.0-0), zoo (>= 1.2-1), coda, nnet, sna, gee,
systemfit, mgcv\cr
License: \tab GPL version 2 or newer\cr
URL: \tab http://gking.harvard.edu/zelig\cr
}

Index:
\preformatted{
approval                U.S. Presidential Approval Data
coalition               Coalition Dissolution in Parliamentary
                        Democracies
current.packages        Find all packages in a dependency chain
dims                    Return Dimensions of Vectors, Arrays, and Data
                        Frames
eidat                   Simulation Data for Ecological Inference
friendship              Simulated Example of Schoolchildren Friendship
                        Network
gsource                 Read Data As a Space-Delimited Table
help.zelig              HTML Help for Zelig Commands and Models
hoff                    Social Security Expenditure Data
immigration             Individual Preferences Over Immigration Policy
macro                   Macroeconomic Data
match.data              Output matched data sets
MatchIt.url             Table of links for Zelig
mexico                  Voting Data from the 1988 Mexican Presidental
                        Election
mi                      Bundle multiply imputed data sets as a list
mid                     Militarized Interstate Disputes
model.end               Cleaning up after optimization
model.frame.multiple    Extracting the "environment" of a model formula
model.matrix.multiple   Design matrix for multivariate models
network                 Format matricies into a data frame for social
                        network analysis
newpainters             The Discretized Painter's Data of de Piles
parse.formula           Parsing user-input formulas into multiple
                        syntax
parse.par               Select and reshape parameter vectors
PErisk                  Political Economic Risk Data from 62 Countries
                        in 1987
plot.ci                 Plotting Vertical confidence Intervals
plot.zelig              Graphing Quantities of Interest
put.start               Set specific starting values for certain
                        parameters
repl                    Replicating Analyses
rocplot                 Receiver Operator Characteristic Plots
sanction                Multilateral Economic Sanctions
set.start               Set starting values for all parameters
setx                    Setting Explanatory Variable Values
sim                     Simulating Quantities of Interest
sna.ex                  Simulated Example of Social Network Data
summary.zelig           Summary of Simulated Quantities of Interest
SupremeCourt            U.S. Supreme Court Vote Matrix
swiss                   Swiss Fertility and Socioeconomic Indicators
                        (1888) Data
ternaryplot             Ternary diagram
ternarypoints           Adding Points to Ternary Diagrams
tobin                   Tobin's Tobit Data
turnout                 Turnout Data Set from the National Election
                        Survey
user.prompt             Pause in demo files
Weimar                  1932 Weimar election data
zelig                   Estimating a Statistical Model
zeligDepStatus          Zelig Dependencies Packages Client Status
zeligDepUpdate          Download Zelig Dependencies Packages
zeligDescribeModelXML   Zelig interface functions
Zelig-package           Everyone's Statistical Software
Zelig.url               Table of links for Zelig
zideal                  Zelig Matrix of Dependencies
}

Further information is available in the following vignettes:

\tabular{ll}{
\code{arima} \tab ARIMA Models for Time Series Data (source)\cr
\code{blogit} \tab Bivariate Logistic Regression for Two Dichotomous Dependent Variables (source)\cr
\code{bprobit} \tab Bivariate Probit Regression for Dichotomous Dependent Variables (source)\cr
\code{ei.RxC} \tab Hierarchical Multinomial-Dirichlet Ecological Inference Model (source)\cr
\code{ei.dynamic} \tab Quinn's Dynamic Ecological Inference (source)\cr
\code{ei.hier} \tab Hierarchical Ecological Inference Model (source)\cr
\code{exp} \tab Exponential Regression for Duration Dependent Variables (source)\cr
\code{factor.bayes} \tab Bayesian Factor Analysis (source)\cr
\code{factor.mix} \tab Mixed Data Factor Analysis (source)\cr
\code{factor.ord} \tab Ordinal Data Factor Analysis (source)\cr
\code{gam.logit} \tab gam.logit: Generalized Additive Model for Dichotomous Dependent Variables (source)\cr
\code{gam.normal} \tab Generalized Additive Model for Continuous Dependent Variables (source)\cr
\code{gam.poisson} \tab Generalized Additive Model for Count Dependent Variables (source)\cr
\code{gam.probit} \tab Generalized Additive Model for Dichotomous Dependent Variables (source)\cr
\code{gamma} \tab Gamma Regression for Continuous, Positive Dependent Variables (source)\cr
\code{irt1d} \tab  One Dimensional Item Response Mode (source)\cr
\code{irtkd} \tab K-Dimensional Item Response Model (source)\cr
\code{logit} \tab Logistic Regression for Dichotomous Dependent Variables (source)\cr
\code{logit.bayes} \tab Bayesian Logistic Regression for Dichotomous Dependent Variables (source)\cr
\code{logit.gee} \tab Generalized Estimating Equation for Logistic Regression (source)\cr
\code{lognorm} \tab Log-Normal Regression for Duration Dependent Variables (source)\cr
\code{ls} \tab Least Squares Regression for Continuous Dependent Variables (source)\cr
\code{mlogit} \tab Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values (source)\cr
\code{mlogit.bayes} \tab Bayesian Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values (source)\cr
\code{mloglm} \tab Multinomial Log-Linear Regression for Contingency Table Models (source)\cr
\code{negbin} \tab Negative Binomial Regression for Event Count Dependent Variables (source)\cr
\code{netcloglog} \tab Least Squares Regression for Continuous Dependent Variables (source)\cr
\code{netgamma} \tab Least Squares Regression for Continuous Dependent Variables (source)\cr
\code{netlogit} \tab Least Squares Regression for Continuous Dependent Variables (source)\cr
\code{netls} \tab Network Least Squares Regression for Continuous Proximity Matrix Dependent Variables (source)\cr
\code{netnormal} \tab Least Squares Regression for Continuous Dependent Variables (source)\cr
\code{netpoisson} \tab Least Squares Regression for Continuous Dependent Variables (source)\cr
\code{netprobit} \tab Least Squares Regression for Continuous Dependent Variables (source)\cr
\code{normal} \tab Normal Regression  for Continuous Dependent Variables (source)\cr
\code{normal.bayes} \tab Bayesian Normal Linear Regression (source)\cr
\code{ologit} \tab Ordinal Logistic Regression  for Ordered Categorical Dependent Variables (source)\cr
\code{oprobit} \tab Ordinal Probit Regression for Ordered Categorical Dependent Variables (source)\cr
\code{oprobit.bayes} \tab Bayesian Ordered Probit Regression (source)\cr
\code{poisson} \tab Poisson Regression for Event Count Dependent Variables (source)\cr
\code{poisson.bayes} \tab Bayesian Poisson Regression (source)\cr
\code{probit} \tab Probit Regression for Dichotomous Dependent Variables (source)\cr
\code{probit.bayes} \tab Bayesian Probit Regression for Dichotomous Dependent Variable (source)\cr
\code{relogit} \tab Rare Events Logistic Regression for Dichotomous Dependent Variables (source)\cr
\code{sur} \tab Seemingly Unrelated Regression (source)\cr
\code{threesls} \tab  Three Stage Least Squares (source)\cr
\code{tobit} \tab Linear regression for Left-Censored Dependet Variable (source)\cr
\code{tobit.bayes} \tab Bayesian Linear Regression for a Censored Dependent Variable (source)\cr
\code{twosls} \tab  Two Stage Least Squares (source)\cr
\code{weibull} \tab Weibull Regression for Duration Dependent Variables (source)\cr
}
}
\author{
Kosuke Imai <kimai@Princeton.Edu>, Gary King
<king@harvard.edu>, Olivia Lau <olau@fas.harvard.edu>

Maintainer: Kosuke Imai <kimai@Princeton.Edu>
}
\keyword{ package }

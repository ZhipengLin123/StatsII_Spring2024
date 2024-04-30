#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))
DF<-climateSupport

#1#

## check data
summary(climateSupport)

## prepare data
DF$choice<- ifelse(DF$choice == "Supported", 1, 0)
DF$countries<- factor(DF$countries,ordered=FALSE)
DF$sanctions<- factor(DF$sanctions,ordered=FALSE)

## check data again
summary(climateSupport)

## Run the logit regression
logit <- glm(choice ~ countries+sanctions, 
           family = binomial(link = "logit"), data = DF)

## check the result
# summary output
summary(logit)#summary output
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                      -0.27469    0.07534  -3.646 0.000267 ***
#  countries80 of 192                0.37562    0.10627   3.535 0.000408 ***
#  countries160 of 192               0.61266    0.10801   5.672 1.41e-08 ***
#  sanctions5%                       0.12179    0.10518   1.158 0.246909    
#sanctions15%                     -0.09687    0.10822  -0.895 0.370723    
#sanctions20%                     -0.25260    0.10806  -2.338 0.019412 *  
#  countries80 of 192:sanctions5%    0.09471    0.15232   0.622 0.534071    
#countries160 of 192:sanctions5%   0.13009    0.15103   0.861 0.389063    
#countries80 of 192:sanctions15%  -0.05229    0.15167  -0.345 0.730262    
#countries160 of 192:sanctions15% -0.05165    0.15267  -0.338 0.735136    
#countries80 of 192:sanctions20%  -0.19721    0.15104  -1.306 0.191675    
#countries160 of 192:sanctions20%  0.05688    0.15367   0.370 0.711279    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 11783  on 8499  degrees of freedom
#Residual deviance: 11562  on 8488  degrees of freedom
#AIC: 11586

#Number of Fisher Scoring iterations: 4

# the global null hypothesis and p-value

#the global null hypothesis is that efficient equals to 0.

#the p-value for the coefficient of countries80 of 192 is 4.05e-10, the null hypothesis is 
#that the coefficient = 0, that is, is the average difference on log-odds value between
#countries80 of 192 and countries20 of 192 is 0.

#the p-value for the coefficient of countries160 of 192 is < 2e-16, the null hypothesis is 
#that the coefficient = 0, that is, is the average difference on log-odds value between
#countries160 of 192 and countries20 of 192 is 0.

#the p-value for the coefficient of sanctions5%  is 0.00203, the null hypothesis is 
#that the coefficient = 0, that is, is the average difference on log-odds value between
#sanctions5% and None is 0.

#the p-value for the coefficient of sanctions15%  is 0.03183, the null hypothesis is 
#that the coefficient = 0, that is, is the average difference on log-odds value between
#sanctions15% and None is 0.

#the p-value for the coefficient of sanctions20%  is 1.01e-06, the null hypothesis is 
#that the coefficient = 0, that is, is the average difference on log-odds value between
#sanctions20% and None is 0.

#Conclusion:All coefficient are significantly not equal to 0. Thus, we get the predicion equation:
#log(odds)=-0.27266+0.33636*D(countries80 of 192)+0.64835*D(countries160 of 192)
#           +0.19186*D(sanctions5% )-0.13325*D(sanctions15%)-0.30356*D(sanctions20%)

#2(a)#

#log(odds)=-0.27266+0.33636*D(countries80 of 192)+0.64835*D(countries160 of 192)
#           +0.19186*D(sanctions5% )-0.13325*D(sanctions15%)-0.30356*D(sanctions20%)

# when countries participate [160 of 192] & sanctions is 5%, the equation is :
#log(odds)=-0.27266+0.64835+0.19186=0.56755
#odds=exp(0.56755)=1.76394

# when countries participate [160 of 192] & sanctions is 15%, the equation is :
#log(odds)=-0.27266+0.64835-0.13325=0.24244
#odds=exp(0.24244)=1.274355


# the change in odds is 1.274355-1.76394=-0.489585

# change in log-odds equals to the coefficient of D(sanctions15%) minus the coefficient of D(sanctions5%),
#so the change in odds is -0.489585

#2(b)#

#estimated probability that an individual will support a policy 
#if there are 80 of 192 countries participating with no sanctions

# log(odds)=-0.27266+0.33636=0.0637
# p=exp(log(odds))/(1+exp(log(odds)))=0.5159196

# the estimated probability that an individual will support a policy if there
#are 80 of 192 countries participating with no sanctions is 0.5159196

#2(c)#
# include the interaction term in this model


# Yes, the answers to 2a and 2b would potentially change if we included the interaction term in this model,
# because when an interaction term is included in the model, it allows for the possibility that the effect 
# of one predictor on the outcome variable depends on the level of another predictor. This means that the 
# relationship between one predictor and the outcome variable is not constant across different levels of the 
# other predictor. As a result, the interpretation of the coefficients associated with each predictor becomes 
# more complex, as the effect of one predictor depends on the level of the other predictor.

logit1 <- glm(choice ~ countries+sanctions, 
             family = binomial, data = DF)

logit2 <- glm(choice ~ countries*sanctions, 
             family = binomial, data = DF)

anova (logit1, logit2,test="Chisq")

# The P-value of chisq-test >0.05, which means we cannot reject the null hypothesis 
# that the equation including interaction items cannot perform better significantly than equatio not including those .
# Therefore, including an interaction is not appropriate.


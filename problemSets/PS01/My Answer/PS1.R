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

set.seed(123)
# Generate Cauchy random variables
n<-1000
data <- rcauchy(n, location = 0, scale = 1)
# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))
#Calculate p_value
p_value <- sqrt(2 * pi) / D * sum(exp(-(2 * (1:n) - 1)^2 * pi^2 / (8 * D^2)))
print(p_value) # p_value=5.652523e-29

#the p-value is significantly less than 0.05, which means the rejection of the 
#hypothesis that the empirical distribution matches the queried theoretical distribution.


#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

#define linear.lik
linear.lik <- function ( theta , y , X ) {
  n <- nrow ( X )
  k <- ncol ( X )
  beta <- theta [ 1 : k ]
  sigma2 <- theta [ k + 1 ] ^2
  e <- y - X%*%beta
  logl <- -.5 *n* log ( 2 * pi ) -.5 *n* log ( sigma2 ) - ( ( t ( e ) %*% e ) / ( 2 * sigma2 ) )
  return ( -logl )
}


#run BFGS
linear.MLE <- optim ( fn = linear.lik , par=c(1,1,1), hessian = TRUE, y =data$y, X= cbind ( 1, data$x ), method = "BFGS" )
linear.MLE$par # 0.1429829  2.7263116 -1.4423360

#run lm
summary (lm ( y ~ x , data ) )

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.13919    0.25276   0.551    0.582    
#x            2.72670    0.04159  65.564   <2e-16 ***
# the BFGS produces the equivalent results to using lm



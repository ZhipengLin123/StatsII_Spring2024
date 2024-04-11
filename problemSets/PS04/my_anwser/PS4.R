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

lapply(c("survival", "eha", "tidyverse", "ggfortify", "stargazer"),  pkgTest)

data(child)

# a) Using the Surv() function to build a survival object
child_surv <- with(child, Surv(enter, exit, event))


# b) Run a Cox Proportional Hazard regression
cox <- coxph(child_surv ~ m.age + sex, data = child)

summary(cox)


# There is a -0.082215 decrease in the expected log of the hazard for female babies compared to 
# male, holding mother’s age constant. There is a 0.007617 increase in the expected log of the hazard
# for one increase in mother’s age, holding sex constant.

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

lapply(c("nnet", "MASS","AER","pscl","ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# (1) unordered multinomial
# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

# subset dataset
gdp_data_sub <- gdp_data[,c("GDPWdiff","OIL","REG","COUNTRY")]

# identify if there are na in the df
rows_with_missing <- which(apply(gdp_data_sub, 1, function(x) any(is.na(x))))
print(rows_with_missing) #there are no na

# summarize the original data
summary(gdp_data_sub)
print(gdp_data_sub)

# wrangling
gdp_data_sub$OIL <- as.factor (gdp_data_sub$OIL)
gdp_data_sub$REG <- as.factor (gdp_data_sub$REG)
gdp_data_sub$COUNTRY <- as.factor (gdp_data_sub$COUNTRY)

gdp_data_sub$GDPWdiff_category <- ifelse(gdp_data_sub$GDPWdiff > 0, 
                                         "positive", 
                                      ifelse(gdp_data_sub$GDPWdiff < 0, 
                                            "negative", "no change"))
gdp_data_sub$GDPWdiff_category_unordered <- factor(gdp_data_sub$GDPWdiff_category, ordered = FALSE)
gdp_data_sub$GDPWdiff_category_unordered <- relevel(gdp_data_sub$GDPWdiff_category_unordered, ref = "no change")

# summarize the original data again
summary(gdp_data_sub)

# run unordered multinomial model
gdp_unorderd <- multinom(gdp_data_sub$GDPWdiff_category_unordered ~ OIL + REG, data = gdp_data_sub)
summary(gdp_unorderd)

# Call:
#   multinom(formula = gdp_data_sub$GDPWdiff_category_unordered ~ 
#              OIL + REG, data = gdp_data_sub)
# 
# Coefficients:
#   (Intercept)     OIL1     REG1
# negative    3.805370 4.783968 1.379282
# positive    4.533759 4.576321 1.769007
# 
# Std. Errors:
#   (Intercept)     OIL1      REG1
# negative   0.2706832 6.885366 0.7686958
# positive   0.2692006 6.885097 0.7670366
# 
# Residual Deviance: 4678.77 
# AIC: 4690.77 




#Answer:
# The unordered multinomial logit with GDPWdiff as the output and ”no change” as the reference category is as follows:

# ln(p(negative)/p(no change)) = 3.805370 + OIL1*4.783968 + REG1*1.379282

# coefficient for OIL1:
# Holding REG1 constant, for every one unit increase in OIL1, the odds of Y= "negative" vs. Y= "no change" increase by exp(4.783968)
# coefficient for REG1:
# Holding OIL1 constant, for every one unit increase in REG1, the odds of Y= "negative" vs. Y= "no change" increase by exp(1.379282)
# Intercept
# When OIL1 and REG1 both equal to 0, the odds of Y= "negative" vs. Y= "no change" equals to exp(3.805370)

# ln(p(negative)/p(no change)) = 4.533759 + OIL1*4.576321 + REG1*1.769007
# coefficient for OIL1:
# Holding REG1 constant, for every one unit increase in OIL1, the odds of Y= "positive" vs. Y= "no change" increase by exp(4.576321)
# coefficient for REG1:
# Holding OIL1 constant, for every one unit increase in REG1, the odds of Y= "positive" vs. Y= "no change" increase by exp(1.769007)
# Intercept
# When OIL1 and REG1 both equal to 0, the odds of Y= "positive" compared to Y= "no change" equals to exp(4.533759)


###########################

# example：
# in a given country, there is an increase in the baseline odds that the Church will expand by 1.68 times when a government supports policies that the Church supports


# convert the coefficient to exp-form
#exp(coef(gdp_unorderd))

# (Intercept)      OIL1     REG1
# negative    44.94186 119.57794 3.972047
# positive    93.10789  97.15632 5.865024



############################
# (2) ordered multinomial
gdp_data_sub$GDPWdiff_category_ordered <- factor(gdp_data_sub$GDPWdiff_category, ordered = TRUE, levels = c("positive", "no change", "negative"))
gdp_orderd <- polr(gdp_data_sub$GDPWdiff_category_ordered ~ OIL + REG, data = gdp_data_sub, Hess = TRUE)
summary(gdp_orderd)

# Call:
#   polr(formula = gdp_data_sub$GDPWdiff_category_ordered ~ OIL + 
#          REG, data = gdp_data_sub, Hess = TRUE)
# 
# Coefficients:
#   Value Std. Error t value
# OIL1  0.1987    0.11572   1.717
# REG1 -0.3985    0.07518  -5.300
# 
# Intercepts:
#   Value   Std. Error t value
# positive|no change  0.7105  0.0475    14.9554
# no change|negative  0.7312  0.0476    15.3597
# 
# Residual Deviance: 4687.689 
# AIC: 4695.689  

# Answer
# ln(p(GDPWdiff_category_ordered+1)/p(GDPWdiff_category_ordered)) = 0.1987*OIL1 + (-0.3985 *REG1)

# coefficient for OIL1:
# Holding REG1 constant, for every one unit increase in OIL1, the odds of GDP becoming from positive change to no change or from no change to positive change increased by exp(0.1987) times
# coefficient for REG1:
# Holding OIL1 constant, for every one unit increase in REG1, the odds of GDP becoming from positive change to no change or from no change to positive change decreased by exp(0.3985) times

# The estimated cutoffs for odds between Y= positive and Y= no change is 0.7105; The estimated cutoffs for odds between Y= no change and Y= negative is 0.7312.


############################
# exp(coef(gdp_orderd))

# OIL1      REG1 
# 0.8197813 1.4895639 

# example： For those who take protective measures (staying at home, wearing a mask outside), odds of being more concerned about COVID are
# 2.6 to 3.5× higher compared to those who do not wear masks outside, holding constant all other variables

# example：Interpretation of β1: ↑ x1 by one unit changes the probability of going to next µj by β units



#####################
# Problem 2
#####################

# (a)
# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# Subset the dataset
mexico_elections_subset <- mexico_elections[,c("PAN.visits.06","competitive.district","marginality.06","PAN.governor.06")]

# identify if there are na in the df
rows_with_missing_mexico <- which(apply(mexico_elections_subset, 1, function(x) any(is.na(x))))
print(rows_with_missing_mexico) #there are no na

# summarize the data
summary(mexico_elections_subset)

# wrangling
mexico_elections_subset$competitive.district <- as.factor (mexico_elections_subset$competitive.district)
mexico_elections_subset$PAN.governor.06 <- as.factor (mexico_elections_subset$PAN.governor.06 )

# summarize the original data again
summary(mexico_elections_subset)

# run poisson regression
mexico_poisson <- glm(formula = PAN.visits.06 ~ ., family = poisson, data = mexico_elections_subset)
summary(mexico_poisson) 

# Call:
#   glm(formula = PAN.visits.06 ~ ., family = poisson, data = mexico_elections_subset)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)           -3.81023    0.22209 -17.156   <2e-16 ***
#   competitive.district1 -0.08135    0.17069  -0.477   0.6336    
# marginality.06        -2.08014    0.11734 -17.728   <2e-16 ***
#   PAN.governor.061      -0.31158    0.16673  -1.869   0.0617 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 1473.87  on 2406  degrees of freedom
# Residual deviance:  991.25  on 2403  degrees of freedom
# AIC: 1299.2
# 
# Number of Fisher Scoring iterations: 7

# check equal variance assumption
dispersiontest( mexico_poisson )

# Overdispersion test
# 
# data:  mexico_poisson
# z = 1.0668, p-value = 0.143
# alternative hypothesis: true dispersion is greater than 1
# sample estimates:
#   dispersion 
# 2.09834 


# A over-dispersion test was run. The p-value of the test is 0.143 which is bigger than 0.05. So we cannot reject the null hyposthesis that true dispersion is smaller or equal than 1.



# Answer

# According to the regression, the equation is as follow:
# ln(visit) = -3.81023 + (-0.08135*competitive) + (-2.08014*marginality) + (-0.31158*governor)

# The equation suggests that holding other varibles consatant, PAN visited wing district about 8% (exp(-0.08135)=0.92186932) less than "saft seat" district.
####################################

# example:
# βˆ0 : No inherent meaning in the context of the data since age= 0 is not meaningful, outside of range of possible data
# βˆ1 : An increase of 1 year in age increases expected number of elephant mates by a multiplicative factor of e 0.06859 ≈ 1.07

# (Intercept) competitive.district1        marginality.06      PAN.governor.061 
# 0.02214298            0.92186932            0.12491227            0.73228985 

############################
# (b)


# the marginality.06 coefficients
# Holding other variables constant, an unit of increase in maginality expected number of PAN visit by a multiplicative factor of eˆ-2.08014 ≈ 0.12491227 
# the PAN.governor.06 coefficients
# Holding other variables constant, PAN visit the states with a PAN-affiliated governor about 27% (exp(-0.31158)=0.73228985) times less than the states with a non-PAN-affiliated governor 

############################
# example:
#βˆ1 : An increase of 1 year in age increases expected number of elephant mates by a multiplicative factor of eˆ0.06859 ≈ 1.07

############################
# (c)

# example 
# Estimated mean number of mates = 4.5

#extract coefficient
cfs <- coef(mexico_poisson)
# the estimated mean number
exp(cfs[1] + cfs[2]*1 + cfs[3]*0 + cfs[4]*1)#0.01494818

# Therefore, the estimated number is 0.01494818.






#####################################################
###Impact of PDS on Food Intakes in Madhya Pradesh###
#####################################################


###CLEANING###

#Packages

library(haven) #importing a .dta type file
library(dplyr) #data manipulation
library(ggplot2) #plotting
library(stargazer) #LaTeX output
library(dagitty) #DAGs
library(ggdag) #visualising DAGs
library(MatchIt) #for implementing propensity score matching
library(AER) #for implementing IV 2SLS
library(sandwich) #for heteroskedasticity corrected standard errors 
library(lmtest) #for coeftest()
library(optmatch) #for optimal matching
library(fastDummies) #for creatind dummy variables
library(sandwich) #for vcov in IV summary 


#Importing Data 

x <- paste("https://raw.github.com/AnanyaIyengar/EC402_Causal_Inference/main/nss_pds.dta", sep = "")
raw_nss <- read_dta(x)
raw_nss <- as.data.frame(raw_nss)


#Selecting Madhya Pradesh Data

nss_mp <- raw_nss %>%
  dplyr::filter(state_name == "MP")


#Summary Statistics, exportable to LaTeX

summary(nss_mp)
stargazer(nss_mp)


###BASELINE SPECIFICATION: ONLY MPCE AS PREDICTOR OF PARTICIPATION###

#Pre-Matching Balance Checks. We consider Di to be PDS_RWS, and Y to be cal_pc_pd.Let the predictor of participation X be MPCE_MRP (since the MRP might be a better way to measure HH expenditure)

m1 <- matchit(PDS_RWS ~ MPCE_MRP, data = nss_mp, method = NULL, distance = "glm")
summary(m1)

#We see imbalances in MPCE_MRP across those who received the PDS and those who did not. There is evidence of selection on observables. We can proceed with matching.


#Implementing Matching 1: Nearest Neighbour Matching, Logit used for computing propensity scores. There are 1623 treatment units. According to Rosenbaum (2020), taking more than 4 nearest neighbours considers more imperfect matches which can worsen balance. So, we use k = 1. This is because using k=2,3 did not lead to balance in MPCE_MRP.

m2 <- matchit(PDS_RWS ~ MPCE_MRP, data = nss_mp, method = "nearest", distance = "logit", ratio = 1)
summary(m2) #1453 observations in control unmatched, which is 47% of the control group. Out of the total sample of N = 4717 HHs, 30% is trimmed out during psm. 

#Creating a Data Frame of the Matched Data

matched_data_baseline_1 <- match.data(m2)

#Looking at Covariate Balance in the Matched Data by plotting propensity scores 

plot(m2, type = "ecdf", which.xs = c("MPCE_MRP"))
                           
#Implementing Matching 2: Optimal Matching, Logit used for computing propensity scores. k=1 is maintained.

m3 <- matchit(PDS_RWS ~ MPCE_MRP, data = nss_mp, method = "nearest", distance = "mahalanobis", ratio = 1)
summary(m3) 

matched_data_baseline_2 <- match.data(m3)

#Looking at Covariate Balance in the Matched Data 

plot(m3, type = "density", which.xs = c("MPCE_MRP"))

###SPECIFICATION WITH MORE COVARIATES###

#Information on Variables impating PDS Participation

#1# Household Size: hhsize
#2# Land Ownership: whetherownsland (1: Yes, 2:No)
#3# Social Group: socialgrp (ST:1, SC:2, OBC:3, Other:9)
#4# Regular Earner or Contractual Worker: regularsal_earner (1:Yes, 2:No)
#5# Primary Source of Energy for Lighting: lightingcode (kerosene:1, other oil:2, gas:3, candle:4, electricity:5, others:9, no lighting arrangement:6)
#6# MPCE_MRP (Mixed Recall Period)
#7# Assets: count_assets
#8# Land Possessed by HH (Highly Correlated with Cultivation and so Self-Consumption): landtotpossd
#9# Religion: religion (Hinduism:1, Islam:2, Christianity:3, Sikhism:4, Jainism:5, Buddhism:6, Zoroastrianism:7, others:9)

#Creating Dummy Variables 

nss_mp <- dummy_cols(nss_mp, select_columns = "whetherownsland") #Ref; No
nss_mp$land_own_dummy <- nss_mp$whetherownsland_1

nss_mp <- dummy_cols(nss_mp, select_columns = "socialgrp")  #Ref: Other
nss_mp$sc <- nss_mp$socialgrp_1
nss_mp$st <- nss_mp$socialgrp_2
nss_mp$obc <- nss_mp$socialgrp_3

nss_mp <- dummy_cols(nss_mp, select_columns = "regularsal_earner") #Ref: Not Regular Salary Earner
nss_mp$regular_salary <-  nss_mp$regularsal_earner_1

nss_mp <- dummy_cols(nss_mp, select_columns = "lightingcode") #Ref: No light arrangements
nss_mp$kerosenelight <- nss_mp$lightingcode_1
nss_mp$otheroil <- nss_mp$lightingcode_2
nss_mp$gas <- nss_mp$lightingcode_3
nss_mp$candle <- nss_mp$lightingcode_4
nss_mp$electricity <- nss_mp$lightingcode_5
nss_mp$otherlight <- nss_mp$lightingcode_9

nss_mp <- dummy_cols(nss_mp, select_columns = "religion") #Ref: Other
nss_mp$hindu <- nss_mp$religion_1
nss_mp$islam <- nss_mp$religion_2
nss_mp$christianity <- nss_mp$religion_3
nss_mp$sikh <- nss_mp$religion_4
nss_mp$jain <- nss_mp$religion_5
nss_mp$buddhism <- nss_mp$religion_6

#Where are the missing values?

colSums(is.na(nss_mp))

#Replacing NA observations with 0 for duringjuly10tojune11_cultivated 

nss_mp$duringjuly10tojune11_cultivated[is.na(nss_mp$duringjuly10tojune11_cultivated)] <- 0
nss_mp$duringjuly10tojune11_irrigated[is.na(nss_mp$duringjuly10tojune11_irrigated)] <- 0
nss_mp1 <- subset(nss_mp, select = -c(typeofrationcard))

nss_mp1 <- nss_mp1[complete.cases(nss_mp1), ]



#Implementing Nearest Neighbour Matching
m4 <- matchit(PDS_RWS ~ MPCE_MRP + count_assets + sc + st + obc + land_own_dummy +regular_salary + hhsize + hindu + islam + kerosenelight + otheroil + gas + candle + electricity + otherlight,
              data = nss_mp1, method = "nearest", distance = "logit", ratio = 1)
summary(m4)
head(m4)

matched_data_alt_1 <- match.data(m4) #Almost 50% of control trimmed! Common Support is an issue! 

#Covariate Balance for MPCE in the Matched Data 

plot(m4, type = "density", which.xs = c("MPCE_MRP"))

###Estimating ATT Using Matched Data in both the Baseline and Alternative Cases###


#Baseline Specification (not v good balance)

outcome_1 <- lm(data = matched_data_baseline_1, calpcpd_cersct ~ PDS_RWS, weights = weights)
summary(outcome_1)

#Heteroskedasticity Check and Correction

durbinWatsonTest(outcome_1) #p = 0, reject H0 
robust_outcome_1 <- coeftest(outcome_1, vcov = vcovHC(outcome_1, type = "HC0"))
robust_outcome_1


#Alternate Specification (good balance)

outcome_2 <- lm(data = matched_data_alt_1, calpcpd_cercst ~ PDS_RWS, weights = weights)
summary(outcome_2) #significant at the 5% level 

#Heteroskedasticity Check and Correction

durbinWatsonTest(outcome_2) #p=0, reject H0
robust_outcome_2 <- coeftest(outcome_2, vcov = vcovHC(outcome_2,type = "HC0"))
robust_outcome_2



###Instrumental Variable Specification: LATE Estimator###

#Motivating the use of the instrument: Possession of AAY+BPL Ration Card -> MPCE -> Calorie Intake. 

#Creating Dummy for Ration Card Ownership. If typeof rationcard == 1 | 2, then dummy = 1, else if typeof rationcard == 3 | 0 , then dummy = 1

nss_mp$typeofrationcard[is.na(nss_mp$typeofrationcard)] <- 0

nss_mp$typeofrationcard <- as.numeric(nss_mp$typeofrationcard)
nss_mp$typeofrationcard <- replace(nss_mp$typeofrationcard, nss_mp$typeofrationcard == 3, 0)
nss_mp$typeofrationcard <- replace(nss_mp$typeofrationcard, nss_mp$typeofrationcard == 2, 1)

summary(nss_mp$typeofrationcard)

#Baseline IV Regression: No covariates 

iv1 <- ivreg(calpcpd_cercst ~ PDS_RWS | typeofrationcard, data = nss_mp)

#Diagnostics
# -"Weak Instruments" is an F test on the First Stage that checks Instrument Relevance
# -"Wu Hausman" has H0 that OLS estimators are consistent i.e there is no endogeneity. 
# -"Sargan" is a test of overidentification for when no. of instruments > no. of regressors 
summary(iv1, vcov = sandwich, diagnostics = TRUE)
stargazer(iv1)



#IV with Covariates: hhsize, socialgrp, count_assets, religion, land_ownership, religion, MPCE_MRP, education of HH Head

#Creating Education Dummy (~ : Present in Data)
# - 1: Not Literate ~
# - 2: Literate Without Formal Schooling ~
# - 3: TLC Education ~
# - 4: Others ~
# - 5: Below Primary ~
# - 6: Primary 
# - 7: Middle
# - 8: Secondary
# - 10: Senior Secondary
# - 11: Diploma/Certificate Course
# - 12: Graduate
# - 13: Post Graduate and Above

nss_mp <- dummy_cols(nss_mp, select_columns = "edu_hhh")

#nss_mp <- dummy_cols(nss_mp, select_columns = "whetherownsland") #Ref: No


iv2 <- ivreg(calpcpd_cercst ~ PDS_RWS + hhsize + MPCE_MRP + count_assets + sc + st + obc + land_own_dummy + regular_salary + hindu + islam + edu_hhh_1 + edu_hhh_2 + edu_hhh_3 + edu_hhh_4 + edu_hhh_5   |  hhsize + MPCE_MRP + count_assets + sc + st + obc + land_own_dummy + regular_salary + hindu + islam + edu_hhh_1 + edu_hhh_2 + edu_hhh_3 + edu_hhh_4 + edu_hhh_5 + typeofrationcard, data = nss_mp)
stargazer(iv2)


#Diagnostics for the 2nd IV Specification
summary(iv2, vcov = sandwich, diagnostics = TRUE)

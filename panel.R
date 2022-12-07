#################################
###Panel Data: Mincer Equation###
#################################

#Setting working directory 

setwd("C:/Ananya Iyengar/Delhi School of Economics/402_Causal Inference/Causal_Inference_Assignments")

################################################################################

#Installing Required Packages

library(haven)
library(dplyr)
library(ggplot2)
library(AER)
library(car)
library(lmtest)
library(plm)
library(sandwich)
library(stargazer)
library(rmarkdown)
library(fastDummies)
library(rddtools)

################################################################################

#Downloading Data for Chattisgarh 

raw_data <- read_dta("ihds_c.dta")

################################################################################

#Variable Description for Mincer Equation: Linear in Education, Quadratic in Experience


# - state: State Under Study: Chattisgarh
# - district: District Codes
# - sex: Sex of Respondent (1: Male, 2: Female)
# - age: Age of Respondent
# - mstatus : Marital Status 
# - literate: Whether Respondent is Literate (0: No, 1: Yes)
# - years_education: Total Years of Education
# - urban: Whether Urban Area (Census 2011)
# - hhsize: HH Size 
# - rel: Religion of HH Head
# - socialgroup : Caste of HH Head
# - num_asset_owned: Number of Assets Owned
# - POOR: Dummy for Poverty
# - year: Year of Survey 
# - migrated: Migration Status 
# - id: Person ID
# - bus1_per: Per Capita Business Income 
# - new_inc: imputed per capita income 
# - emp: Whether employed or not 

#Creating Data Frame with Selected Variables

panel <- raw_data%>%select(state, district, sex, age, mstatus, literate, years_education, urban, hhsize, rel, socialgroup, num_asset_owned, POOR, year, migrated, id, bus1_per, newinc, emp, district, PSUID)

###############################################################################

#Dealing with NA observations

print(sum(complete.cases(panel)))

#Which variables have NAs

print(sum(is.na(panel$literate))) #15 NAs
print(sum(is.na(panel$years_education))) #25 NAs

#Extracting NA variables

panel[which(is.na(panel$literate)), ]

#Complete Cases: 6141  Observations

panel <- panel[complete.cases(panel$years_education), ]

###############################################################################

#Creating Dummy Variables

panel <- dummy_cols(panel, select_columns = c("mstatus", "rel", "socialgroup", "migrated", "sex"))

panel$married_spouse_absent <- panel$mstatus_0
panel$married <- panel$mstatus_1
panel$unmarried <- panel$mstatus_2
panel$widowed <- panel$mstatus_3
panel$sep_div <- panel$mstatus_4

panel$hindu <- panel$rel_1
panel$muslim <- panel$rel_2

panel$brahmin <- panel$socialgroup_1
panel$gen_except_brahmin <- panel$socialgroup_2
panel$obc <- panel$socialgroup_3
panel$sc <- panel$socialgroup_4
panel$st <- panel$socialgroup_5

panel$not_migrated <- panel$migrated_0
panel$migrated_same_state <- panel$migrated_1
panel$migrated_diff_state <- panel$migrated_2
panel$migrated_diff_country <- panel$migrated_3

panel$male <- panel$sex_1
panel$female <- panel$sex_2

###############################################################################

#Summary Statistics   

panel <- as.data.frame(panel)
summary(panel)
stargazer(panel)


###############################################################################

#Specification Detail for Mincer Equation: Full Paper 

# Y: log(newinc)
# X : years_education, age, age^2
# Time-Invariant : rel, socialgroup, sex
# Time-Variant: mstatus, POOR, urban, hhsize, num_assets_owned, migrated, literate
# Fixed Effects: year, id 

###############################################################################

#Pooled OLS: Full Data Model  + Diagnostics for log(newinc) as Y

#Balance Checks (3058 =/= 3083)

data2005 <- panel%>%dplyr::filter(year == 2005)
data2012 <- panel%>%dplyr::filter(year == 2012)

length(unique(data2005$id))
length(unique(data2012$id))

#Creating log(newinc), log(bus1_per), age, age squared and log(wage_pc) variables 

for (i in 1:length(panel$age)) {
  if (panel$year[i] == 2012){
    panel$age[i] <- panel$age[i]
  }
  else if (panel$year[i] == 2005) {
    panel$age[i] <- panel$age[i+1] - 7
  }
}


panel$lognewinc <- log(panel$newinc)
panel$lognewinc[is.infinite(panel$lognewinc)] <- 0
panel$agesq <- (panel$age)^2

panel <- panel%>%dplyr::mutate(wage_pc = newinc - bus1_per)

panel$logwage_pc <- log(panel$wage_pc)
panel$logwage_pc[is.infinite(panel$logwage_pc)] <- 0

panel$logbusinc <- log(panel$bus1_per)
panel$logbusinc[is.infinite(panel$logbusinc)] <- 0


#Creating plm object

panel <- pdata.frame(panel, index = c("id", "year"), drop.index = TRUE, row.names = FALSE)
head(attr(panel, "index"))

pdim(panel)$balanced #FALSE

#Creating Balance by dropping 25 observations

panel <- make.pbalanced(panel, balance.type = "shared.individuals")
pdim(panel)$balanced #TRUE

#Running a Pooled OLS Specification: log(newinc) ~ years_education + age + age^2 + Time-Variant + e_it 

attach(panel)

reg_pooled_baseline_newinc <- plm(lognewinc ~ years_education + age + agesq, data = panel, model = "pooling", index = c("id", "year"), effect = "individual")
summary(reg_pooled_baseline_newinc)

reg_pooled_alternate_newinc <- plm(lognewinc ~ years_education + age + agesq + hhsize + married + num_asset_owned + urban, data = panel, model = "pooling", effect = "individual")
summary(reg_pooled_alternate_newinc)

#Diagnostics: Heteroskedasticity

bptest(reg_pooled_baseline_newinc) #reject Ho
bptest(reg_pooled_alternate_newinc) #reject Ho

#Diagnostics: Autocorrelation

pdwtest(reg_pooled_baseline_newinc) #reject Ho
pdwtest(reg_pooled_alternate_newinc) #reject Ho

#Newey-West HAC Standard Errors

reg_pooled_baseline_newinc_robust <- coeftest(reg_pooled_baseline_newinc, vcov. = function(x) vcovNW(x))
reg_pooled_baseline_newinc_robust

reg_pooled_alternate_newinc_robust <- coeftest(reg_pooled_alternate_newinc, vcov. = function(x) vcovNW(x))
reg_pooled_alternate_newinc_robust

#LaTeX Tables 

stargazer(reg_pooled_baseline_newinc, reg_pooled_alternate_newinc, se = list(reg_pooled_baseline_newinc_robust[, "Std. Error"], reg_pooled_alternate_newinc_robust[, "Std. Error"]))


###############################################################################

#Fixed Effects Within Specifications With Age 

baseline_within_newinc <- plm(lognewinc ~ years_education + age + agesq, data = panel, model = "within", effect = "individual", index = c("id", "year"))
summary(baseline_within_newinc)

alternate_within_newinc <- plm(lognewinc ~ years_education + age + agesq + hhsize + married + num_asset_owned + urban, data = panel, model = "within", effect = "individual", index = c("id", "year"))
summary(alternate_within_newinc)

#Fixed Effects Within Specification with Potential Experience 

panel <- panel%>%dplyr::mutate(exp = age - years_education - 6)
panel$exp[panel$exp < 0] <- 0
panel$exp[panel$age > 80] <- 70
panel <- panel%>%dplyr::mutate(expsq = (exp)^2)

baseline_within_exp <- plm(lognewinc ~ years_education + exp + expsq, data = panel, model = "within", effect = "individual", index = c("id", "year"))
summary(baseline_within_exp)

alternate_within_exp <- plm(lognewinc ~ years_education + exp + expsq + hhsize + married + num_asset_owned + urban, data = panel, model = "within", effect = "individual", index = c("id", "year"))
summary(alternate_within_exp)


################################################################################

#Diagnostics: Heteroskedasticity

bptest(baseline_within_newinc) #reject H0
bptest(alternate_within_newinc) #reject H0

#Diagnostics: Autocorrelation 

pdwtest(baseline_within_newinc) #don't reject null
pdwtest(alternate_within_newinc) #don't reject null

#Clustered Standard Errors

robust_within_baseline_age <- coeftest(baseline_within_newinc, vcov = vcovHC(baseline_within_newinc, type = "sss", cluster = "group"))
robust_within_alternate_age <- coeftest(alternate_within_newinc, vcov = vcovHC(baseline_within_newinc, type = "sss", cluster = "group"))

robust_within_baseline_exp <- coeftest(baseline_within_exp, vcov = vcovHC(baseline_within_newinc, type = "sss", cluster = "group"))
robust_within_alternate_exp <- coeftest(alternate_within_exp, vcov = vcovHC(baseline_within_newinc, type = "sss", cluster = "group"))


################################################################################
#TWFE with Potential Experience 

#Baseline

baseline_twfe_exp <- plm(lognewinc ~ years_education + exp + expsq, data = panel, effect = "twoway", index = c("id", "year"))
summary(baseline_twfe_exp)

#Alternate

alternate_twfe_exp <- plm(lognewinc ~ years_education + exp + expsq + hhsize + married + num_asset_owned + urban, data = panel, effect = "twoway", index = c("id", "year"))
summary(alternate_twfe_exp)

#Robust Standard Errors 

robust_twfe_baseline <- coeftest(baseline_twfe_exp, vcov = vcovHC(baseline_twfe_exp, type = "sss", cluster = "group"))
robust_twfe_alternate <- coeftest(alternate_twfe_exp, vcov = vcovHC(alternate_twfe_exp, type = "sss", cluster = "group"))


################################################################################

#LaTeX Tables













###############################################################################

#Comments 

#On what basis is age a proxy for experience?
#NO data on when edu getting over? 
#Bias: measurement errors in age. 
#Selection into employment is endogenous
#cross sectional variation!!!
#clustering???
#12 - 7 
# for NA -> 0, can't say that these were ppl with 0 income or ppl who refused to record 
# people in school ca also work! problems with potential exp specification

###############################################################################

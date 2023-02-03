####################################################
###MLA CRIMINALITY AND SERVICE PROVISION IN INDIA###
####################################################

#ANANYA IYENGAR & SAMEER GHUGRE#
#SEMESTER 3, IMPACT EVALUATION#
#DELHI SCHOOL OF ECONOMICS#

################################################################################

#SECTION 1: DATA IMPORT: SHRUG, NSS 78: HEALTH AND SANITATION

################################################################################

#Loading Packages 

library(readr)
library(dplyr)
library(fastDummies)
library(readxl)

#NSS78: Sanitatation. Levels 1, 3, 4, 5. Household Level Outcomes#

sanlvl1 <- paste("R76120L01.txt")

sanitation_level1 <- read_fwf(file = sanlvl1, 
                              fwf_cols(centrecd = c(1,3), fsuslno = c(4,8),
                                       round = c(9,10), sch = c(11,13),
                                       sample = c(14,14), sector = c(15, 15),
                                       nssregion = c(16,18), district = c(19,20),
                                       stratum = c(21,22), substratum = c(23,24),
                                       subround = c(25,25), fodsubrg = c(26,29),
                                       secondstgstrno = c(30,30), hh = c(31,32),
                                       level = c(33,34), filler = c(35,39),
                                       slnoinformant = c(40,41), responsecode = c(42,42),
                                       surveycode = c(43,43), substitutioncode = c(44,44),
                                       blank = c(45,126), nsc = c(127,129), multiplier = c(130,139)),
                              col_types = cols(centrecd = col_character(), fsuslno = col_character(),
                                               round = col_character(), sch = col_character(),
                                               sample = col_character(), sector = col_integer(),
                                               nssregion = col_character(), district = col_character(),
                                               stratum = col_character(), substratum = col_character(),
                                               subround = col_character(), fodsubrg = col_character(),
                                               secondstgstrno = col_character(), hh = col_character(),
                                               level = col_character(), filler = col_character(),
                                               slnoinformant = col_character(), responsecode = col_character(),
                                               surveycode = col_character(), substitutioncode = col_character(),
                                               blank = col_character(), nsc = col_number(), multiplier = col_number()))


sanlvl3 <- paste("R76120L03.txt")

sanitation_level3 <- read_fwf(file = sanlvl3,
                              fwf_cols(commonid = c(1,32), level = c(33,34),
                                       filler = c(35,39), hhsize = c(40,41),
                                       religion = c(42,42), socialgroup = c(43,43),
                                       landpossessed = c(44,45), outofpurchase = c(46,55),
                                       imputedownconsumption = c(56,65), imputedkindconsumption = c(66,75),
                                       durablesexpenditure = c(76,85), mce = c(86,95),
                                       dwelling = c(96,96), dwellarea = c(97,97),
                                       dwellamount = c(98,108), sourcefin1 = c(109,110),
                                       sourcefin2 = c(111,112), sourcefin3 = c(113,114),
                                       sourcefin4 = c(115,116), maxdisttravmale = c(117,117),
                                       maxdisttravfemale = c(118,118), maxdisttravtrans = c(119,119),
                                       blank = c(120,126), nsc = c(127,129), multiplier = c(130,139)),
                              col_types = cols(commonid = col_character(), level = col_character(),
                                               filler = col_character(), hhsize  = col_integer(),
                                               religion = col_integer(), socialgroup = col_integer(),
                                               landpossessed = col_integer(), outofpurchase = col_integer(),
                                               imputedownconsumption = col_double(), imputedkindconsumption = col_double(),
                                               durablesexpenditure = col_double(), mce = col_double(),
                                               dwelling = col_double(), dwellarea = col_double(),
                                               dwellamount = col_double(), sourcefin1  = col_double(),
                                               sourcefin2 = col_double(), sourcefin3 = col_double(),
                                               sourcefin4 = col_double(), maxdisttravmale = col_double(),
                                               maxdisttravfemale = col_double(), maxdisttravtrans = col_double(),
                                               blank = col_character(), nsc = col_number(), multiplier = col_number()))  


sanlvl4 <- paste("R76120L04.txt")

sanitation_level4 <- read_fwf(file = sanlvl4,
                              fwf_cols(commonid = c(1,32), level = c(33,34),
                                       filler = c(35,39), drinkingwaterbenefit = c(40,40),
                                       drinkingwaterbenefitlast3yrs = c(41,41), drinkingwaterscheme = c(42,42),
                                       sanitationbenefit = c(43,43), sanitationbenefitlast3yrs = c(44,44),
                                       sanitationscheme = c(45,45), housingbenefit = c(46,46),
                                       housingbenefitlast3yrs = c(47,47), housingscheme = c(48,48),
                                       electrificationbenefit = c(49,49), electrificationbenefitlast3yrs = c(50,50),
                                       electrificationscheme = c(51,51), lpgbenefit = c(52,52),
                                       lpgbenefitlast3yrs = c(53,53), lpgscheme = c(54,54),
                                       blank = c(55,126), nsc = c(127,129), multiplier = c(130,139)),
                              col_types = c(commonid = col_character(), level = col_character(),
                                            filler = col_character(), drinkingwaterbenefit = col_double(),
                                            drinkingwaterbenefitlast3yrs = col_double(), drinkingwaterscheme = col_double(),
                                            sanitationbenefit = col_double(), sanitationbenefitlast3yrs = col_double(),
                                            sanitationscheme = col_double(), housingbenefit = col_double(),
                                            housingbenefitlast3yrs = col_double(), housingscheme = col_double(),
                                            electrificationbenefit = col_double(), electrificationbenefitlast3yrs = col_double(),
                                            electrificationscheme = col_double(), lpgbenefit = col_double(),
                                            lpgbenefitlast3yrs = col_double(), lpgscheme = col_double(),
                                            blank = col_character(), nsc = col_number(), multiplier = col_number()))                              


sanlvl5 <- paste("R76120L05.txt")

sanitation_level5 <-read_fwf(file = sanlvl5,
                             fwf_cols(commonid = c(1,32), level = c(33,34),
                                      filler = c(35,39), principalsourceofdrinkingwater = c(40,41),
                                      sufficientdrinkingwater = c(42,42), insuffjan = c(43,43),
                                      insufffeb = c(44,44), insuffmarch = c(45,45), insuffapr = c(46,46),
                                      insuffmay = c(47,47), insuffjune = c(48,48), insuffjuly = c(49,49),
                                      insuffaug = c(50,50), insuffsept = c(51,51), insuffoct = c(52,52),
                                      insuffnov = c(53,53), insuffdec = c(54,54), acesstoprincipaldrinkingwater = c(55,55),
                                      distancetoprincipaldrinkingwater = c(56,56), whofetcheswater = c(57,57),
                                      timetofetchwater = c(58,60), waitingtimewater =c(61,63), numberoftripsforwater = c(64,65),
                                      stagnantwateraroundsource = c(66,66), supplementarysourceofwater = c(67,68),
                                      methodoftreatment = c(69,69), materialofmaincontainer = c(70,70), howlongstored = c(71,71),
                                      whethercovered = c(72,72), howtakenout = c(73,73), nondrinkprincipalwatersource = c(74,75),
                                      sufficientnondrinkingwater = c(76,76), freqofsupply = c(77,77),
                                      metered = c(78,78), ifanyamountpaid = c(79,79), avgamountpaidforwater = c(80,89),
                                      accesstobathroom = c(90,91), typeofbathroom = c(92,92),
                                      distancefrombathingplace = c(93,93), accesstolatrine = c(94,94),
                                      typeoflatrine = c(95,96), withinpremises = c(97,97),
                                      ifseptictankemptied = c(98,98), wholastemptiedseptictank = c(99,99),
                                      disposalplace = c(100,100), amoutpaidfordisposal = c(101,108),
                                      freqofdisposal = c(109,109), disposalforchildren = c(110,110),
                                      handwashbeforemeal = c(112,112), handwashbeforedefecation = c(113,113),
                                      blank = c(114,126), nsc = c(127,129), multiplier = c(130,139)))

sanitation_level1 <- as.data.frame(sanitation_level1)
sanitation_level3 <- as.data.frame(sanitation_level3)
sanitation_level4 <- as.data.frame(sanitation_level4)
sanitation_level5 <- as.data.frame(sanitation_level5)

#Primary Key Formation

sanitation_level1$primarykey <- paste0(sanitation_level1$fsuslno, sanitation_level1$secondstgstrno, sanitation_level1$hh)

sanitation_level3$fsuslno <- substr(sanitation_level3$commonid, 4, 8)
sanitation_level3$secondstgstrno <- substr(sanitation_level3$commonid, 30, 30)
sanitation_level3$hh <- substr(sanitation_level3$commonid, 31, 32)
sanitation_level3$primarykey <- paste0(sanitation_level3$fsuslno, sanitation_level3$secondstgstrno, sanitation_level3$hh)

sanitation_level4$fsuslno <- substr(sanitation_level4$commonid, 4, 8)
sanitation_level4$secondstgstrno <- substr(sanitation_level4$commonid, 30, 30)
sanitation_level4$hh <- substr(sanitation_level4$commonid, 31, 32)
sanitation_level4$primarykey <- paste0(sanitation_level4$fsuslno, sanitation_level4$secondstgstrno, sanitation_level4$hh)

sanitation_level5$fsuslno <- substr(sanitation_level5$commonid, 4, 8)
sanitation_level5$secondstgstrno <- substr(sanitation_level5$commonid, 30, 30)
sanitation_level5$hh <- substr(sanitation_level5$commonid, 31, 32)
sanitation_level5$primarykey <- paste0(sanitation_level5$fsuslno, sanitation_level5$secondstgstrno, sanitation_level5$hh)


#Merging All Sanitation Data Levels

san13 <- inner_join(sanitation_level1, sanitation_level3, by = "primarykey")
san134 <- inner_join(sanitation_level4, san13, by = "primarykey")
san1345 <- inner_join(sanitation_level5, san134, by = "primarykey")

sanitation_outcomes <- as.data.frame(san1345)


#Combining NSS Region and District to create "admin_district" variable 

sanitation_outcomes$admin_district <- paste0(sanitation_outcomes$nssregion, sanitation_outcomes$district)

#Recoding Dummy Variables

sanitation_outcomes$sufficientdrinkingwater <- replace(sanitation_outcomes$sufficientdrinkingwater, sanitation_outcomes$sufficientdrinkingwater == 2, 0)
sanitation_outcomes <- dummy_cols(sanitation_outcomes, select_columns = c("acesstoprincipaldrinkingwater", "distancetoprincipaldrinkingwater"))

sanitation_outcomes$hhdrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_1
sanitation_outcomes$commonhhdrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_2
sanitation_outcomes$neighbourdrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_3
sanitation_outcomes$publicsourcecommunitydrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_4
sanitation_outcomes$publicsourceunrestricteddrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_5
sanitation_outcomes$pvtsourcecommunitydrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_6
sanitation_outcomes$pvtsourceunrestricteddrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_7
sanitation_outcomes$otherdrinkingwater <- sanitation_outcomes$acesstoprincipaldrinkingwater_7

sanitation_outcomes$drinkingwaterwithindwelling <- sanitation_outcomes$distancetoprincipaldrinkingwater_1
sanitation_outcomes$drinkingwateroutsidedwellingwithinpremises <- sanitation_outcomes$distancetoprincipaldrinkingwater_2
sanitation_outcomes$lessthan0.2kmdrinkingwater <- sanitation_outcomes$distancetoprincipaldrinkingwater_3
sanitation_outcomes$between0.2to0.5kmdrinkingwater <- sanitation_outcomes$distancetoprincipaldrinkingwater_4
sanitation_outcomes$between0.5to1kmdrinkingwater <- sanitation_outcomes$distancetoprincipaldrinkingwater_5
sanitation_outcomes$between1to1.5kmdrinkingwater <- sanitation_outcomes$distancetoprincipaldrinkingwater_6
sanitation_outcomes$morethan1.5kmdrinkingwater <- sanitation_outcomes$distancetoprincipaldrinkingwater_7


sanitation_outcomes <- dummy_cols(sanitation_outcomes, select_columns = c("drinkingwaterbenefit", "sanitationbenefit", "housingbenefit", "electrificationbenefit", "lpgbenefit"))


sanitation_outcomes$nrdwp <- sanitation_outcomes$drinkingwaterbenefit_1
sanitation_outcomes$amrut_water <- sanitation_outcomes$drinkingwaterbenefit_2
sanitation_outcomes$smartcity_water <- sanitation_outcomes$drinkingwaterbenefit_3


sanitation_outcomes$swachbharat <- sanitation_outcomes$sanitationbenefit_1
sanitation_outcomes$amrut_sanitation <- sanitation_outcomes$sanitationbenefit_2
sanitation_outcomes$smartcity_sanitation <- sanitation_outcomes$sanitationbenefit_3


sanitation_outcomes$pmay <- sanitation_outcomes$housingbenefit_1
sanitation_outcomes$otherhousing <- sanitation_outcomes$housingbenefit_2

sanitation_outcomes$ddugjy <- sanitation_outcomes$electrificationbenefit_1

sanitation_outcomes$pmuy <- sanitation_outcomes$lpgbenefit_1
sanitation_outcomes$otherlpg <- sanitation_outcomes$lpgbenefit_2


selected_data <- sanitation_outcomes %>% 
  select(admin_district ,hhdrinkingwater, commonhhdrinkingwater, neighbourdrinkingwater, publicsourcecommunitydrinkingwater, publicsourceunrestricteddrinkingwater, pvtsourcecommunitydrinkingwater, pvtsourceunrestricteddrinkingwater, otherdrinkingwater, drinkingwaterwithindwelling, drinkingwateroutsidedwellingwithinpremises, lessthan0.2kmdrinkingwater, between0.2to0.5kmdrinkingwater, between0.5to1kmdrinkingwater, between1to1.5kmdrinkingwater, morethan1.5kmdrinkingwater, nrdwp, amrut_water, smartcity_water, swachbharat, amrut_sanitation, smartcity_sanitation, pmay, otherhousing, pmuy, otherlpg, ddugjy)

#Taking averages across districts grouping by the "admin_district" variable


mean_sanitation_outcomes <- selected_data%>%group_by(admin_district)%>%summarise_all("mean")


#Making NAs in drinking water distance proportions to 0

mean_sanitation_outcomes$drinkingwaterwithindwelling[is.na(mean_sanitation_outcomes$drinkingwaterwithindwelling)] <- 0
mean_sanitation_outcomes$drinkingwateroutsidedwellingwithinpremises[is.na(mean_sanitation_outcomes$drinkingwateroutsidedwellingwithinpremises)] <- 0
mean_sanitation_outcomes$lessthan0.2kmdrinkingwater[is.na(mean_sanitation_outcomes$lessthan0.2kmdrinkingwater)] <- 0
mean_sanitation_outcomes$between0.2to0.5kmdrinkingwater[is.na(mean_sanitation_outcomes$between0.2to0.5kmdrinkingwater)] <- 0
mean_sanitation_outcomes$between0.5to1kmdrinkingwater[is.na(mean_sanitation_outcomes$between0.5to1kmdrinkingwater)] <- 0
mean_sanitation_outcomes$between1to1.5kmdrinkingwater[is.na(mean_sanitation_outcomes$between1to1.5kmdrinkingwater)] <- 0
mean_sanitation_outcomes$morethan1.5kmdrinkingwater[is.na(mean_sanitation_outcomes$morethan1.5kmdrinkingwater)] <- 0

#Downloading Data 
shrug <- read_excel("merged_df.xlsx")
shrug$admin_district <- shrug$Group.1

#Merging outcome sanitation data

merged_data_sanitation <- dplyr::inner_join(shrug, mean_sanitation_outcomes, by = "admin_district")

#Downloading Merged Data

write.csv(merged_data_sanitation, "C:\\Users\\anniy\\OneDrive\\Desktop\\Impact Term Paper\\merged_sanitation.csv", row.names = FALSE)



###########################################################################################################

#SECTION 2: PROPENSITY SCORE MATCHING: LOGIT MODELS: FULL DATA, SEVERE AND NON-SEVERE CRIMES AS TREATMENT

###########################################################################################################

setwd("C:/Users/anniy/OneDrive/Desktop/Impact Term Paper")


#####################################################################

#Installing Packages

library(dplyr) #for data manipulation
library(ggplot2) #for graphing
library(cobalt) #for balance checks
library(AER) #for implementing IV 2SLS
library(sandwich) #for heteroskedasticity corrected standard errors 
library(lmtest) #for coeftest()
library(MatchIt) #for matching
library(stargazer) #for LaTeX tables
library(marginaleffects) #for ATT under MatchIt

#####################################################################

#Merged Data Frame

head(merged_data_sanitation)
summary(merged_data_sanitation)

#####################################################################

#Creating Criminality Dummy (All and Severe) for different cutoffs: median and mean 



cdf_crime <- ggplot(merged_data_sanitation, aes(num_crim)) + stat_ecdf(geom = "smooth") +  geom_vline(aes(xintercept = median(num_crim)))
print(cdf_crime)

crime_median <- median(merged_data_sanitation$num_crim)
crime_mean <- mean(merged_data_sanitation$num_crim)

merged_data_sanitation$severe <- ifelse(merged_data_sanitation$adr_major_crime > 0 , 1, 0)
merged_data_sanitation$severe[is.na(merged_data_sanitation$severe)]<-0

merged_data_sanitation$crim_median <- ifelse(merged_data_sanitation$severe == 1 , 1, ifelse(merged_data_sanitation$num_crim > crime_median, 1, 0))

merged_data_sanitation$crim_mean <- ifelse(merged_data_sanitation$severe == 1 , 1, ifelse(merged_data_sanitation$num_crim > crime_mean, 1, 0))


#######################################################################

#Matching Specification 1: Logit Propensity Scores

#Variable Descriptions for Observable Predictors of Criminality (Baseline Yar 2011)

# - avglight2011: average nighttime luminosity measures in lumins for each 1km * 1km grid cell, proxy for population, employment, per-capita consumption, and electrification at very local levels.
# - propsc: proportion of sc population in the district 
# - prop_pop_lit: proportion of literate population in the district
# - totcoll: total number of colleges in the district
# - totsch: total number of schools in the district 
# - pc11_vd_tar_road: proportion of districts having a tar road according the population census 2011
# - net_assets: average net assets of winning MLAs in the district 
# - age: average age of winning MLAs in the district
# - ed: average education of winning MLAs in the district 
# - pc11_vd_power_agr : proportion of HHs in the district who access power consumption for agriculture 
# - secc_cons_pc_rural : small-area estimate of rural per capita consumption

#Finding Missing Values in these covariates

print(sum(is.na(merged_data_sanitation$totcoll))) #316 NAs: impute with national average 
print(sum(is.na(merged_data_sanitation$totsch))) #316 NAs: impute with national avergae
print(sum(is.na(merged_data_sanitation$pc11_vd_tar_road))) #47 NAs: impute with lower quartile 0.6081
print(sum(is.na(merged_data_sanitation$ed))) #42 NAs: impute with 10 
print(sum(is.na(merged_data_sanitation$pc11_vd_power_agr))) #97 NAs: impute with mean
print(sum(is.na(merged_data_sanitation$secc_cons_pc_rural)))#65 NA's
print(sum(is.na(merged_data_sanitation$secc_inc_cultiv_share)))#129 NA's

#Which are these missing values?

tar_missing <- merged_data_sanitation[which(is.na(merged_data_sanitation$pc11_vd_tar_road)), ]
agrpower_missing <- merged_data_sanitation[which(is.na(merged_data_sanitation$pc11_vd_power_agr)), ]

#Replace Missing Values

merged_data_sanitation$totcoll[is.na(merged_data_sanitation$totcoll)] <- mean(merged_data_sanitation$totcoll, na.rm = TRUE)
merged_data_sanitation$totsch[is.na(merged_data_sanitation$totsch)] <- mean(merged_data_sanitation$totsch, na.rm = TRUE)
merged_data_sanitation$pc11_vd_tar_road[is.na(merged_data_sanitation$pc11_vd_tar_road)] <- 0.6081
merged_data_sanitation$ed[is.na(merged_data_sanitation$ed)] <- 10
merged_data_sanitation$pc11_vd_power_agr[is.na(merged_data_sanitation$pc11_vd_power_agr)] <- mean(merged_data_sanitation$pc11_vd_power_agr, na.rm = TRUE)
merged_data_sanitation$secc_cons_pc_rural[is.na(merged_data_sanitation$secc_cons_pc_rural)]<-mean(merged_data_sanitation$secc_cons_pc_rural,na.rm=TRUE)
merged_data_sanitation$secc_inc_cultiv_share[is.na(merged_data_sanitation$secc_inc_cultiv_share)]<-mean(merged_data_sanitation$secc_inc_cultiv_share,na.rm=TRUE)
merged_data_sanitation$year<-round(merged_data_sanitation$year)
merged_data_sanitation$year<-as.factor(merged_data_sanitation$year)
merged_data_sanitation$propsc<-merged_data_sanitation$pc11_pca_p_sc/merged_data_sanitation$pc11_pca_tot_p


#Attaching Data 

attach(merged_data_sanitation)


#Pre-Matching Balance Checks 

pre_balance <- matchit(crim_median ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr, data = merged_data_sanitation, method = NULL, distance = "glm")
summary(pre_balance)
v <- data.frame(old = c("avglight2011", "propsc", "secc_cons_pc_rural", "prop_pop_lit", "totcoll", "totsch", "pc11_vd_tar_road", "net_assets", "age", "ed", "pc11_vd_power_agr"), new = c("Mean Luminosity 2011", "Proportion of SC Population", "Rural PC Consumption", "Literacy Rate", "Total Colleges", "Total Schools", "Tar Roads", "MLA Net Assets", "MLA Age", "MLA Education", "Agriculture Power Use"))
unbalanced_plot <- love.plot(bal.tab(pre_balance), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v, stars = "raw")
print(unbalanced_plot)
unbalanced_check <- bal.tab(crim_median ~ avglight2011 + propsc + prop_pop_lit + totcoll + totsch + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr, data = merged_data_sanitation, v.threshold = 2)
unbalanced_check

#Matching: Logit 1, Nearest Neighbour

matchlogit1 <- matchit(crim_median ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr, data = merged_data_sanitation, method = "nearest", distance = "logit", ratio = 1, estimand = "ATT")
summary(matchlogit1)
matched_data_logit_1 <- match.data(matchlogit1)

balance_logit1_plot <- love.plot(bal.tab(matchlogit1), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v, stars = "raw")
print(balance_logit1_plot)

logit1_balance_check <- bal.tab(crim_median ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + totsch + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr, data = matched_data_logit_1, v.threshold = 2)
logit1_balance_check

#Matching: Logit 2, Nearest Neighbour with State Dummies 

matchlogit2 <- matchit(crim_median ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + totsch + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id), data = merged_data_sanitation, method = "nearest", distance = "logit", ratio = 1, estimand = "ATT")
summary(matchlogit2)
matched_data_logit_2 <- match.data(matchlogit2)

balance_logit2_plot <- love.plot(bal.tab(matchlogit2), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v, stars = "raw")
print(balance_logit2_plot)

logit2_balance_check <- bal.tab(crim_median ~ avglight2011 + propn_res + prop_pop_lit + totcoll + totsch + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id), data = matched_data_logit_2, v.threshold = 2)
logit2_balance_check

################################################################################

#Finding ATT using the marginaleffects package with cluster robust standard errors as per Abadie and Spiess (2019);  Austin 2009, 2013a; Austin and Small 2014; Gayat et al. 2012; Wan 2019.

#Outcome Variable: Public Source Unrestricted Drinking Water Access

att_logit_publicwater <- lm(data = matched_data_logit_2, publicsourceunrestricteddrinkingwater ~ crim_median, weights = weights)
att_publicwater <- comparisons(att_logit_publicwater, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_logit_2, crim_median == 1), wts = "weights")


#Outcome Variable: More than 1.5 km distance travel to access drinking water

att_logit_1.5km <- lm(data = matched_data_logit_2, morethan1.5kmdrinkingwater ~ crim_median, weights = weights)
att_1.5km <- comparisons(att_logit_1.5km, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_logit_2, crim_median == 1), wts = "weights")

#Outcome Variable: Less than 0.2 km distance travel to access drinking water
att_logit_0.2km <- lm(data = matched_data_logit_2, lessthan0.2kmdrinkingwater ~ crim_median, weights = weights)
att_0.2km <- comparisons(att_logit_0.2km, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_logit_2, crim_median == 1), wts = "weights")


#Outcome Variable: Swach Bharat 
att_logit_swachbharat <- lm(data = matched_data_logit_2, swachbharat ~ crim_median, weights = weights)
att_swachbharat <- comparisons(att_logit_swachbharat, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_logit_2, crim_median == 1), wts = "weights")

#Outcome Variable: Amrut Sanitation
att_logit_amrutsanitation <- lm(data = matched_data_logit_2, amrut_sanitation ~ crim_median, weights = weights)
att_amrutsanitation <- comparisons(att_logit_amrutsanitation, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_logit_2, crim_median == 1), wts = "weights")

#Outcome Variable: Amrut Water
att_logit_amrutwater <- lm(data = matched_data_logit_2, amrut_water ~ crim_median, weights = weights)
att_amrutwater <- comparisons(att_logit_amrutwater, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_logit_2, crim_median == 1), wts = "weights")

#Outcome Variable: NRDWP
att_logit_nrdwp <- lm(data = matched_data_logit_2, nrdwp ~ crim_median, weights = weights)
att_nrdwp <- comparisons(att_logit_nrdwp, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_logit_2, crim_median == 1), wts = "weights")

summary(att_publicwater)
summary(att_1.5km)
summary(att_0.2km)
summary(att_swachbharat)
summary(att_amrutsanitation)
summary(att_amrutwater)
summary(att_nrdwp)

nrow(matched_optimal_sev)
####################################################################################

#Matching Specification 2: Logit, But Only for Severe Crimes VS Everyone else. Many-One Matching

#Pre-Matching Data 


pre_balance_severe <- matchit(severe ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + totsch + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id), data = merged_data_sanitation, method = NULL, distance = "logit")
summary(pre_balance_severe)

#Matching with Logit: K = 3 Nearest Neighbours

matchlogit1_severe <- matchit(severe ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + totsch + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id), data = merged_data_sanitation, method = "nearest", distance = "logit", ratio = 3)
summary(matchlogit1_severe)
matched_data_logit_1_severe <- match.data(matchlogit1_severe) #316 Obs, common support: only 79 treated vs 237 control

balance_logit1_plot_severe <- love.plot(bal.tab(matchlogit1_severe), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v)
print(balance_logit1_plot_severe)

balance_check_logit1_severe <- bal.tab(severe ~ avglight2011 + propsc+secc_cons_pc_rural + prop_pop_lit + totcoll + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + pc01_state_id, data = matched_data_logit_1_severe, v.threshold = 2)
balance_check_logit1_severe #achieved balance! 

#Computing ATT For Severe Crimes using the matched_data_logit_severe data set

#Outcome Variable: Public Source Unrestricted Drinking Water Access
att_logit_publicwater_severe <- lm(data = matched_data_logit_1_severe, publicsourceunrestricteddrinkingwater ~ severe)
att_publicwater_severe <- comparisons(att_logit_publicwater_severe, variables = "severe", vcov = ~subclass, newdata = subset(matched_data_logit_1_severe, severe == 1), wts = "weights")

#Outcome Variable: More than 1.5 km distance travel to access drinking water
att_logit_1.5km_severe <- lm(data = matched_data_logit_1_severe, morethan1.5kmdrinkingwater ~ severe)
att_1.5km_severe <- comparisons(att_logit_1.5km_severe, variables = "severe", vcov = ~subclass, newdata = subset(matched_data_logit_1_severe, severe == 1), wts = "weights")


#Outcome Variable: Less than 0.2 km distance travel to access drinking water
att_logit_0.2km_severe <- lm(data = matched_data_logit_1_severe, lessthan0.2kmdrinkingwater ~ severe)
att_0.2km_severe <- comparisons(att_logit_0.2km_severe, variables = "severe", vcov = ~subclass, newdata = subset(matched_data_logit_1_severe, severe == 1), wts = "weights")


#Outcome Variable: Swach Bharat 
att_logit_swachbharat_severe <- lm(data = matched_data_logit_1_severe, swachbharat ~ severe)
att_swachbharat_severe <- comparisons(att_logit_swachbharat_severe, variables = "severe", vcov = ~subclass, newdata = subset(matched_data_logit_1_severe, severe == 1), wts = "weights")


#Outcome Variable: Amrut Sanitation
att_logit_amrutsanitation_severe <- lm(data = matched_data_logit_1_severe, amrut_sanitation ~ severe)
att_amrutsanitation_severe <- comparisons(att_logit_amrutsanitation_severe, variables = "severe", vcov = ~subclass, newdata = subset(matched_data_logit_1_severe, severe == 1), wts = "weights")


#Outcome Variable: Amrut Water
att_logit_amrutwater_severe <- lm(data = matched_data_logit_1_severe, amrut_water ~ severe)
att_amrutwater_severe <- comparisons(att_logit_amrutwater_severe, variables = "severe", vcov = ~subclass, newdata = subset(matched_data_logit_1_severe, severe == 1), wts = "weights")


#Outcome Variable: NRDWP
att_logit_nrdwp_severe <- lm(data = matched_data_logit_1_severe, nrdwp ~ severe)
att_nrdwp_severe <- comparisons(att_logit_nrdwp_severe, variables = "severe", vcov = ~subclass, newdata = subset(matched_data_logit_1_severe, severe == 1), wts = "weights")


summary(att_publicwater_severe)
summary(att_1.5km_severe)
summary(att_0.2km_severe)
summary(att_swachbharat_severe)
summary(att_amrutsanitation_severe)
summary(att_amrutwater_severe)
summary(att_nrdwp_severe)


##############################################################################

#What is driving results? Severe or Non-severe crimes?

non_severe_crimes <- merged_data_sanitation%>%dplyr::filter(severe == 0)

#There are 283 Comparison and 227 Treatment

#Matching with Logit using crim_median as the treatment (trim 56 districs)


matchlogit_nonsevere <- matchit(crim_median ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + totsch + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id), data = non_severe_crimes, method = "nearest", distance = "logit", ratio = 1)
summary(matchlogit_nonsevere)

matched_data_nonsevere <- match.data(matchlogit_nonsevere)

balance_nonsevere <- love.plot(bal.tab(matchlogit_nonsevere), stats = "mean.diffs", threshold = 0.10, abs = FALSE, var.names = v, stars = "raw")
print(balance_nonsevere)

#Variance Ratios < 2 for all variables => Balance! 


#Computing ATT for non-severe crimes on all outcome variables!


#Outcome Variable: Public Source Unrestricted Drinking Water Access
att_logit_publicwater_nonsevere <- lm(data = matched_data_nonsevere, publicsourceunrestricteddrinkingwater ~ crim_median)
att_publicwater_nonsevere <- comparisons(att_logit_publicwater_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: More than 1.5 km distance travel to access drinking water
att_logit_1.5km_nonsevere <- lm(data = matched_data_nonsevere, morethan1.5kmdrinkingwater ~ crim_median)
att_1.5_nonsevere <- comparisons(att_logit_1.5km_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Less than 0.2 km distance travel to access drinking water
att_logit_0.2_nonsevere <- lm(data = matched_data_nonsevere, lessthan0.2kmdrinkingwater ~ crim_median)
att_0.2_nonsevere <- comparisons(att_logit_0.2_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Swach Bharat
att_logit_swachbharat_nonsevere <- lm(data = matched_data_nonsevere, swachbharat ~ crim_median)
att_swachbharat_nonsevere <- comparisons(att_logit_swachbharat_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Amrut Sanitation
att_logit_amrutsanitation_nonsevere <- lm(data = matched_data_nonsevere, amrut_sanitation ~ crim_median)
att_amrutsanitation_nonsevere <- comparisons(att_logit_amrutsanitation_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Amrut Water
att_logit_amrutwater_nonsevere <- lm(data = matched_data_nonsevere, amrut_water ~ crim_median)
att_amrutwater_nonsevere <- comparisons(att_logit_amrutwater_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: NRDWP
att_logit_nrdwp_nonsevere <- lm(data = matched_data_nonsevere, nrdwp ~ crim_median)
att_nrdwp_nonsevere <- comparisons(att_logit_nrdwp_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_data_nonsevere, crim_median == 1), wts = "weights")

summary(att_publicwater_nonsevere)
summary(att_1.5_nonsevere)
summary(att_0.2_nonsevere)
summary(att_swachbharat_nonsevere)
summary(att_amrutsanitation_nonsevere)
summary(att_amrutwater_nonsevere)
summary(att_nrdwp_nonsevere)

################################################################################

#SECTION 3: OPTIMAL MATCHING: ALL 3 CASES AS BEFORE (TEST OF ROBUSTNESS!)

################################################################################


#Installing Packages

library(dplyr)
library(tidyverse)
library(cobalt)
library(AER) #for implementing IV 2SLS
library(sandwich) #for heteroskedasticity corrected standard errors 
library(lmtest) #for coeftest()
library(MatchIt) #for matching
library(readxl)
library(DMwR)
library(optmatch) #for optimal matching
library(cem)
library(devtools)
library(survey)
install_github("markmfredrickson/RItools")
install_github("markmfredrickson/optmatch") 
install.packages("optmatch", repos = "https://cran.microsoft.com/snapshot/2022-02-15/")
library(marginaleffects)

################################################################################

#Ease of Use: Creating Factor Variables

merged_data_sanitation$pc01_state_id <- as.factor(merged_data_sanitation$pc01_state_id)

################################################################################

#Optimal Matching for crim_median as treatment 

ps_crim <- crim_median ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id)

mopt_crim<- matchit(ps_crim,data = merged_data_sanitation, method = "optimal", estimand = "ATT",verbose = T)
summary(mopt_crim) #variance ratio < 2 for all => balance! 

matched_optimal_crim <- match.data(mopt_crim)


################################################################################

#Estimating ATT for All the Health and Sanitation Outcome Variables for crim_median

#Outcome Variable: Public Source Unrestricted Drinking Water Access

att_opt_publicwater <- lm(data = matched_optimal_crim, publicsourceunrestricteddrinkingwater ~ crim_median, weights = weights)
att_publicwater_opt <- comparisons(att_opt_publicwater, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_crim, crim_median == 1), wts = "weights")


#Outcome Variable: More than 1.5 km distance travel to access drinking water

att_opt_1.5 <- lm(data = matched_optimal_crim, morethan1.5kmdrinkingwater ~ crim_median, weights = weights)
att_1.5_opt <- comparisons(att_opt_1.5, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_crim, crim_median == 1), wts = "weights")


#Outcome Variable: Less than 0.2 km distance travel to access drinking water

att_opt_0.2 <- lm(data = matched_optimal_crim, lessthan0.2kmdrinkingwater ~ crim_median, weights = weights)
att_0.2_opt <- comparisons(att_opt_0.2, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_crim, crim_median == 1), wts = "weights")


#Outcome Variable: Swach Bharat 

att_opt_swachbharat <- lm(data = matched_optimal_crim, swachbharat ~ crim_median, weights = weights)
att_swachbharat_opt <- comparisons(att_opt_swachbharat, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_crim, crim_median == 1), wts = "weights")


#Outcome Variable: Amrut Sanitation

att_opt_amrutsanitation <- lm(data = matched_optimal_crim, amrut_sanitation ~ crim_median, weights = weights)
att_amrutsanitation_opt <- comparisons(att_opt_amrutsanitation, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_crim, crim_median == 1), wts = "weights")


#Outcome Variable: Amrut Water

att_opt_amrutwater <- lm(data = matched_optimal_crim, amrut_water ~ crim_median, weights = weights)
att_amrutwater_opt <- comparisons(att_opt_amrutwater, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_crim, crim_median == 1), wts = "weights")



#Outcome Variable: NRDWP

att_opt_nrdwp <- lm(data = matched_optimal_crim, nrdwp ~ crim_median, weights = weights)
att_nrdwp_opt <- comparisons(att_opt_nrdwp, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_crim, crim_median == 1), wts = "weights")

summary(att_publicwater_opt)
summary(att_1.5_opt)
summary(att_0.2_opt)
summary(att_swachbharat_opt)
summary(att_amrutsanitation_opt)
summary(att_amrutwater_opt)
summary(att_nrdwp_opt)


################################################################################

#Optimal Matching with "severe" as the treatment


ps_sev <- severe ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id)

mopt_sev<- matchit(ps_sev,data = merged_data_sanitation, method = "optimal", estimand = "ATT",verbose = T, ratio = 3)
summary(mopt_sev) #variance ratio < 2 for all => balance! 

matched_optimal_sev <- match.data(mopt_sev)

################################################################################

#Estimating ATT for all Health and Sanitation outcomes for "severe" as treatment

#Outcome Variable: Public Source Unrestricted Drinking Water Access

att_opt_publicwater_sev <- lm(data = matched_optimal_sev, publicsourceunrestricteddrinkingwater ~ severe, weights = weights)
att_publicwater_opt_sev <- comparisons(att_opt_publicwater_sev, variables = "severe", vcov = ~subclass, newdata = subset(matched_optimal_sev, severe == 1), wts = "weights")


#Outcome Variable: More than 1.5 km distance travel to access drinking water

att_opt_1.5_sev <- lm(data = matched_optimal_sev, morethan1.5kmdrinkingwater ~ severe, weights = weights)
att_1.5_opt_sev <- comparisons(att_opt_1.5_sev, variables = "severe", vcov = ~subclass, newdata = subset(matched_optimal_sev, severe == 1), wts = "weights")


#Outcome Variable: Less than 0.2 km distance travel to access drinking water

att_opt_0.2_sev <- lm(data = matched_optimal_sev, lessthan0.2kmdrinkingwater ~ severe, weights = weights)
att_0.2_opt_sev <- comparisons(att_opt_0.2_sev, variables = "severe", vcov = ~subclass, newdata = subset(matched_optimal_sev, severe == 1), wts = "weights")


#Outcome Variable: Swach Bharat 

att_opt_swachbharat_sev <- lm(data = matched_optimal_sev, swachbharat ~ severe, weights = weights)
att_swachbharat_opt_sev <- comparisons(att_opt_swachbharat_sev, variables = "severe", vcov = ~subclass, newdata = subset(matched_optimal_sev, severe == 1), wts = "weights")

#Outcome Variable: Amrut Sanitation

att_opt_amrutsanitation_sev <- lm(data = matched_optimal_sev, amrut_sanitation ~ severe, weights = weights)
att_amrutsanitation_opt_sev <- comparisons(att_opt_amrutsanitation_sev, variables = "severe", vcov = ~subclass, newdata = subset(matched_optimal_sev, severe == 1), wts = "weights")


#Outcome Variable: Amrut Water

att_opt_amrutwater_sev <- lm(data = matched_optimal_sev, amrut_water ~ severe, weights = weights)
att_amrutwater_opt_sev <- comparisons(att_opt_amrutwater_sev, variables = "severe", vcov = ~subclass, newdata = subset(matched_optimal_sev, severe == 1), wts = "weights")


#Outcome Variable: NRDWP

att_opt_nrdwp_sev <- lm(data = matched_optimal_sev, nrdwp ~ severe, weights = weights)
att_nrdwp_opt_sev <- comparisons(att_opt_nrdwp_sev, variables = "severe", vcov = ~subclass, newdata = subset(matched_optimal_sev, severe == 1), wts = "weights")

#ATT with Cluster Robust SE

summary(att_publicwater_opt_sev)
summary(att_1.5_opt_sev)
summary(att_0.2_opt_sev)
summary(att_swachbharat_opt_sev)
summary(att_amrutsanitation_opt_sev)
summary(att_amrutwater_opt_sev)
summary(att_nrdwp_opt_sev)

#################################################################################

#Optimal Matching: Non-Severe Crimes vs No Crimes


ps_nonsevere <- crim_median ~ avglight2011 + propsc + secc_cons_pc_rural + prop_pop_lit + totcoll + pc11_vd_tar_road + net_assets + age + ed + pc11_vd_power_agr + factor(pc01_state_id)

mopt_nonsevere<- matchit(ps_nonsevere,data = non_severe_crimes, method = "optimal", estimand = "ATT",verbose = T)
summary(mopt_nonsevere) #variance ratio < 2 for all => balance! 

matched_optimal_nonsevere <- match.data(mopt_nonsevere)

#Finding ATT with cluster robust SE


#Outcome Variable: Public Source Unrestricted Drinking Water Access
att_opt_publicwater_nonsevere <- lm(data = matched_optimal_nonsevere, publicsourceunrestricteddrinkingwater ~ crim_median)
att_publicwater_nonsevere_opt <- comparisons(att_opt_publicwater_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: More than 1.5 km distance travel to access drinking water

att_opt_1.5_nonsevere <- lm(data = matched_optimal_nonsevere, morethan1.5kmdrinkingwater ~ crim_median)
att_1.5_nonsevere_opt <- comparisons(att_opt_1.5_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Less than 0.2 km distance travel to access drinking water

att_opt_0.2_nonsevere <- lm(data = matched_optimal_nonsevere, lessthan0.2kmdrinkingwater ~ crim_median)
att_0.2_nonsevere_opt <- comparisons(att_opt_0.2_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Swach Bharat

att_opt_swachbharat_nonsevere <- lm(data = matched_optimal_nonsevere, swachbharat ~ crim_median)
att_swachbharat_nonsevere_opt <- comparisons(att_opt_swachbharat_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Amrut Sanitation

att_opt_amrutsanitation_nonsevere <- lm(data = matched_optimal_nonsevere, amrut_sanitation ~ crim_median)
att_amrutsanitation_nonsevere_opt <- comparisons(att_opt_amrutsanitation_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: Amrut Water

att_opt_amrutwater_nonsevere <- lm(data = matched_optimal_nonsevere, amrut_water ~ crim_median)
att_amrutwater_nonsevere_opt <- comparisons(att_opt_amrutwater_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_nonsevere, crim_median == 1), wts = "weights")


#Outcome Variable: NRDWP

att_opt_nrdwp_nonsevere <- lm(data = matched_optimal_nonsevere, nrdwp ~ crim_median)
att_nrdwp_nonsevere_opt <- comparisons(att_opt_nrdwp_nonsevere, variables = "crim_median", vcov = ~subclass, newdata = subset(matched_optimal_nonsevere, crim_median == 1), wts = "weights")



summary(att_publicwater_nonsevere_opt)
summary(att_1.5_nonsevere_opt)
summary(att_0.2_nonsevere_opt)
summary(att_swachbharat_nonsevere_opt)
summary(att_amrutsanitation_nonsevere_opt)
summary(att_amrutwater_nonsevere_opt)
summary(att_nrdwp_nonsevere_opt)



























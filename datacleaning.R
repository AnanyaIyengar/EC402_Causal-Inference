###################################
###Downloading and Cleaning Data###
###################################

#Packages 

library(readr)
library(dplyr)
library(fastDummies)
library(readxl)

#NSS78: Sanitatation. Levels 1, 3, 4, 5. Household Level Outcomes#

sanlvl1 <- paste("https://raw.github.com/AnanyaIyengar/EC402_Causal_Inference/main/R76120L01.txt")

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


sanlvl3 <- paste("https://raw.github.com/AnanyaIyengar/EC402_Causal_Inference/main/R76120L03.txt")

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


sanlvl4 <- paste("https://raw.github.com/AnanyaIyengar/EC402_Causal_Inference/main/R76120L04.txt")

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
                              

sanlvl5 <- paste("https://raw.github.com/AnanyaIyengar/EC402_Causal_Inference/main/R76120L05.txt")

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

sanitation_outcomes$sufficientdrinkingwater <- replace(sanitation_outcomes$sufficientdrinkingwater, sanitation_outcomes$sufficientdrinkingwater ==2, 0)
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



       


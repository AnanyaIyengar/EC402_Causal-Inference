###################################
###Downloading and Cleaning Data###
###################################

#Packages 

library(readr)
library(dplyr)

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

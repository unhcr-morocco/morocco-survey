######## load extract from proGres for poststratification\
rm(list = ls())

## Load all required packages         #############################################
source("code/0-packages.R")
library(survey)
#################################################################
## First load the universe
##loading  case profile from progres


#### post stratification of the sample

## cf tuto here: http://www.andrew.cmu.edu/user/jsmurray/teaching/303/files/lab.html
## https://www.r-bloggers.com/survey-computing-your-own-post-stratification-weights-in-r/
## http://sdaza.com/survey/2012/08/25/raking/


#universe1 <- read.csv("data/casemorocco.csv")
universe <- read_excel("data/Caseprofil v3.xlsx")
#names(universe)

universe$case_reachable.caseid2 <- universe$CaseNo

universe$Case.size <- car::recode(universe$Num_Inds1,"'1'='Case.size.1';
                                  '2'='Case.size.2';
                                  '3'='Case.size.3';
                                  '4'='Case.size.4';
                                  '5'='Case.size.5';
                                  '6'='Case.size.6';
                                  '7'='Case.size.7.and.more';
                                  '8'='Case.size.7.and.more';
                                  '9'='Case.size.7.and.more';
                                  '10'='Case.size.7.and.more';
                                  '11'='Case.size.7.and.more';
                                  '12'='Case.size.7.and.more';
                                  '13'='Case.size.7.and.more';
                                  '14'='Case.size.7.and.more';
                                  '15'='Case.size.7.and.more';
                                  '16'='Case.size.7.and.more';
                                  '17'='Case.size.7.and.more';
                                  '18'='Case.size.7.and.more';
                                  '19'='Case.size.7.and.more';
                                  '20'='Case.size.7.and.more';
                                  '20'='Case.size.7.and.more';
                                  '21'='Case.size.7.and.more';
                                  '22'='Case.size.7.and.more';
                                  '23'='Case.size.7.and.more';
                                  '24'='Case.size.7.and.more';
                                  '25'='Case.size.7.and.more';
                                  '26'='Case.size.7.and.more';
                                  '27'='Case.size.7.and.more';
                                  '29'='Case.size.7.and.more';
                                  '36'='Case.size.7.and.more';
                                  '37'='Case.size.7.and.more';
                                  '42'='Case.size.7.and.more';
                                  '42'='Case.size.7.and.more';
                                  '46'='Case.size.7.and.more';
                                  '55'='Case.size.7.and.more';
                                  '59'='Case.size.7.and.more';
                                  '65'='Case.size.7.and.more';
                                  '67'='Case.size.7.and.more';
                                  '83'='Case.size.7.and.more';
                                  '42'='Case.size.7.and.more';
                                  NA = 'noData'")

## Merge survey with Universe to get common data for poststratification

data <- read.csv("data/dataclean.csv", encoding = "UTF-8", na.strings = "")

data <- merge(x = data, y = universe, by = "case_reachable.caseid2", all.x = TRUE)
names(data)
nomatchedcase <- data[ is.na(data$CaseNo) ,c( "case_reachable.caseid2", "X_uuid" ,   "sociodemo.location" ,
                                              "sociodemo.relationship" , "sociodemo.sex"  , "sociodemo.age" ,
                                              "sociodemo.maritalstatus" , "sociodemo.nationality"       )]

write.csv(nomatchedcase, "data/nomatchedcase.csv", row.names = FALSE, na = "")

N <- nrow(universe)
n <- nrow(data)


#The survey package provides a survey.design object, which is a container for a dataset and the
# sampling design information, including sampling scheme, weights, population sizes (and more).

# The svydesign function is used to create survey.design objects.
# It has a number of arguments, but the most important for you are:

###  ids: Name of variable in the dataframe that contains cluster ids
##  ids = ~1 means there is no clustering.

###  strata: Names of stratification variables, as a formula: ~var1 + var2 + var3
## strata = NULL means there was no stratification.

## weights	: Formula or vector specifying sampling weights as an alternative to prob
# probs: Formula or data frame specifying cluster sampling probabilities

###  fpc (finite population correction) : A vector the same length as the data, giving the stratum population size for each observation.
##The name is confusing, since you don’t actually supply the finite population correction factor.
## fpc = rep(N, n): The function call rep(N, n) generates a vector of length n where each entry is
## N (the population size).

## “Independent sampling design” means that the sampling design is an SRS - Stratified Random Sample.
## When the population size is specified (via the fpc argument) it is assumed that the SRS is without replacement.

###  data: Dataframe containing the raw survey data
## data = dat tells svydesign where to find the actual data.



######################################################################
## Option 1 - use  weight from the original sampling plan
## Abandonned because of low response rate


######################################################################
## Option 1 - Doing poststratification
## We will build 2 stratum - corresponding to the 2 dependent variable to intention as per the chi square test.
## ctry of Asylum
## Are of origin

## Relative frequencies for each of these levels from the population data frames

universe$CountryOrigin2 <- car::recode(universe$CountryOrigin,"'SYR'='Syria';
                                       'IRQ'='Iraq';
                                       'PAL'='Other';
                                       'ICO'='Cote Ivoire';
                                       'COD'='Congo RDC';
                                       'CMR'='Cameroun';
                                       'MLI'='Mali';
                                       'GUI'='Guinea';
                                       'CAR'='Central African Republic';
                                       'SOM'='Other';
                                       'AFG'='Other';
                                       'SUD'='Other';
                                       'ETH'='Other';
                                       'ERT'='Other';
                                       'TUR'='Other';
                                       'PAK'='Other';
                                       'YEM'='Yemen';
                                       'NIG'='Other';
                                       'BGD'='Other';
                                       'ARE'='Other';
                                       'COB'='Other';
                                       'LBY'='Other';
                                       'LEB'='Other';
                                       'CHD'='Other';
                                       'GBR'='Other';
                                       'FRA'='Other';
                                       'JOR'='Other';
                                       'SEN'='Other';
                                       'ALG'='Other';
                                       'GHA'='Other';
                                       'TUN'='Other';
                                       'SLE'='Other';
                                       'LBR'='Other';
                                       'CHI'='Other';
                                       'BDI'='Other';
                                       'MYA'='Other';
                                       'TOG'='Other';
                                       'GAM'='Other';
                                       'NGR'='Other';
                                       'ANG'='Other';
                                       'BKF'='Other';
                                       'BEN'='Other';
                                       'GAB'='Other';
                                       'INS'='Other';
                                       'MAU'='Other';
                                       'GNB'='Other';
                                       'AZE'='Other';
                                       'ITA'='Other';
                                       'SWA'='Other';
                                       'IRN'='Other';
                                       'SWA'='Other';
                                       'MAD'='Other';
                                       'MOR'='Other';
                                       'SSD'='Other';
                                       'VEN'='Other';
                                       'ZIM'='Other';
                                       'USA'='Other';
                                       'EGU'='Other'")

data$CountryOrigin2 <- car::recode(data$CountryOrigin,"'SYR'='Syria';
                                   'IRQ'='Iraq';
                                   'PAL'='Other';
                                   'ICO'='Cote Ivoire';
                                   'COD'='Congo RDC';
                                   'CMR'='Cameroun';
                                   'MLI'='Mali';
                                   'GUI'='Guinea';
                                   'CAR'='Central African Republic';
                                   'SOM'='Other';
                                   'AFG'='Other';
                                   'SUD'='Other';
                                   'ETH'='Other';
                                   'ERT'='Other';
                                   'TUR'='Other';
                                   'PAK'='Other';
                                   'YEM'='Yemen';
                                   'NIG'='Other';
                                   'BGD'='Other';
                                   'ARE'='Other';
                                   'COB'='Other';
                                   'LBY'='Other';
                                   'LEB'='Other';
                                   'CHD'='Other';
                                   'GBR'='Other';
                                   'FRA'='Other';
                                   'JOR'='Other';
                                   'SEN'='Other';
                                   'ALG'='Other';
                                   'GHA'='Other';
                                   'TUN'='Other';
                                   'SLE'='Other';
                                   'LBR'='Other';
                                   'CHI'='Other';
                                   'BDI'='Other';
                                   'MYA'='Other';
                                   'TOG'='Other';
                                   'GAM'='Other';
                                   'NGR'='Other';
                                   'ANG'='Other';
                                   'BKF'='Other';
                                   'BEN'='Other';
                                   'GAB'='Other';
                                   'INS'='Other';
                                   'MAU'='Other';
                                   'GNB'='Other';
                                   'AZE'='Other';
                                   'ITA'='Other';
                                   'IRN'='Other';
                                   'SWA'='Other';
                                   'MAD'='Other';
                                   'MOR'='Other';
                                   'VEN'='Other';
                                   'ZIM'='Other';
                                   'USA'='Other';
                                   'SSD'='Other';
                                   'EGU'='Other'")


prop.table(table(data$CountryOrigin2, useNA = "ifany"))
prop.table(table(universe$CountryOrigin2, useNA = "ifany"))

### Add Case size stata
data$Case.size2 <- data$Case.size
#levels(as.factor(data$Case.size2))
prop.table(table(data$Case.size, useNA = "ifany"))
#check <- data[is.na(data$Case.size2),]

#data$Case.size2 <- car::recode(data$Case.size2,"'1'='Case.size.1';
#                              '2'='Case.size.2';
#                              '3'='Case.size.3.to.5';
#                              '4'='Case.size.3.to.5';
#                              '5'='Case.size.3.to.5';
#                              '6'='Case.size.6.and.more';
#                              '7'='Case.size.6.and.more';
#                              '8'='Case.size.6.and.more';
#                              '9'='Case.size.6.and.more';
#                              '10'='Case.size.6.and.more'")

data$Case.size2 <- car::recode(data$Case.size, "'Case.size.2'='Case.size.2.to.5';
                               'Case.size.3'='Case.size.2.to.5';
                               'Case.size.4'='Case.size.2.to.5';
                               'Case.size.5'='Case.size.2.to.5';
                               'Case.size.6'='Case.size.6.and.more';
                               'Case.size.7.and.more'='Case.size.6.and.more'")


prop.table(table(data$Case.size2, useNA = "ifany"))

universe$Case.size2 <- car::recode(universe$Case.size,"'Case.size.2'='Case.size.2.to.5';
                                   'Case.size.3'='Case.size.2.to.5';
                                   'Case.size.4'='Case.size.2.to.5';
                                   'Case.size.5'='Case.size.2.to.5';
                                   'Case.size.6'='Case.size.6.and.more';
                                   'Case.size.7.and.more'='Case.size.6.and.more'")

prop.table(table(universe$Case.size2, useNA = "ifany"))





################################
cat("create the unweighted survey object\n")
## create the unweighted survey object  ####
data.svy.unweighted <- svydesign(ids =  ~ 1,  data = data)

## Post stratify on those relative frequency
## Use post-stratify
#postStratify(design, strata, population, partial = FALSE, ...)
## strata           A formula or data frame of post-stratifying variables
## population       A table, xtabs or data.frame with population frequencies
## partial          if TRUE, ignore population strata not present in the sample
## sometimes it is necessary to trim weights, if they have grown too large or too small.
## This will make your data fit less well the population marginal distributions,
## but inflating a few cases to too much weight, is rarely ever sensible:
## Perhaps that one person that now counts as 50 is somewhat deranged, or otherwise not representative.
# So it is best to keep an eye on your weights.




cat("post stratification only on ctr of Origin\n")
####################
universe.CountryOrigin2 <- table(CountryOrigin2 = universe$CountryOrigin2)
CountryOrigin2 <- levels(as.factor(data$CountryOrigin2))

## Try post stratification only on ctr of Origin
data.svy.rake.ctr <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ CountryOrigin2),
  population.margins = list(universe.CountryOrigin2)
)

summary(weights(data.svy.rake.ctr))
#data.svy.rake.ctr.trim <- trimWeights(data.svy.rake.ctr,
#                                      lower = 3, upper = 30,  strict = TRUE)


cat("post stratification only on ctr of Origin & Gender of PA\n")
####################
data$key <- paste(data$CountryOrigin2,data$dem_sex,sep = "-")
prop.table(table(data$key, useNA = "ifany"))

universe$key <- paste(universe$CountryOrigin2,universe$dem_sex,sep = "-")
prop.table(table(universe$key, useNA = "ifany"))

teststata <- merge(x = as.data.frame(table(key = universe$key) ) , y = as.data.frame(table(key = data$key)) , by = "key", all.x = TRUE)

universe.key <- table(key = universe$key)
key <- levels(as.factor(data$key))
data.svy.unweighted <- svydesign(ids =  ~ 1,  data = data)
## Try post stratification only on ctr of Origin
data.svy.rake.ctr.gender <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ key),
  population.margins = list(universe.key)
)

summary(weights(data.svy.rake.ctr.gender))
#data.svy.rake.ctr.gender.trim <- trimWeights(data.svy.rake.ctr,
#                                      lower = 3, upper = 30,  strict = TRUE)


#################
data$key2 <- paste(data$CountryOrigin2,data$Case.size2,sep = "-")
prop.table(table(data$key2, useNA = "ifany"))

universe$key2 <- paste(universe$CountryOrigin2,universe$Case.size2,sep = "-")
prop.table(table(universe$key2, useNA = "ifany"))


teststata <- merge(x = as.data.frame(table(key = universe$key2) ) , y = as.data.frame(table(key = data$key2)) , by = "key", all.x = TRUE)


## Rework the key
#data$key2[data$key2 == "Cote Ivoire-Case.size.6.and.more"] <- "Cote Ivoire-Case.size.2.and.more"
#data$key2[data$key2 == "Cote Ivoire-Case.size.2.to.5"] <- "Cote Ivoire-Case.size.2.and.more"

#data$key2[data$key2 == "Guinea-Case.size.6.and.more"] <- "Guinea-Case.size.2.and.more"
#data$key2[data$key2 == "Guinea-Case.size.2.to.5"] <- "Guinea-Case.size.2.and.more"

data$key2[data$key2 == "Mali-Case.size.1"] <- "Mali"
data$key2[data$key2 == "Mali-Case.size.2.to.5"] <- "Mali"

#universe$key2[universe$key2 == "Cote Ivoire-Case.size.6.and.more"] <- "Cote Ivoire-Case.size.2.and.more"
#universe$key2[universe$key2 == "Cote Ivoire-Case.size.2.to.5"] <- "Cote Ivoire-Case.size.2.and.more"

#universe$key2[universe$key2 == "Guinea-Case.size.6.and.more"] <- "Guinea-Case.size.2.and.more"
#universe$key2[universe$key2 == "Guinea-Case.size.2.to.5"] <- "Guinea-Case.size.2.and.more"

universe$key2[universe$key2 == "Mali-Case.size.1"] <- "Mali"
universe$key2[universe$key2 == "Mali-Case.size.2.to.5"] <- "Mali"

universe.key2 <- table(key2 = universe$key2)
key2 <- levels(as.factor(data$key2))

data.svy.unweighted <- svydesign(ids =  ~ 1,  data = data)
## Try post stratification only on ctr of Origin
data.svy.rake.ctr.casesize <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ key2),
  population.margins = list(universe.key2)
)

summary(weights(data.svy.rake.ctr.casesize))
#data.svy.rake.ctr.casesize.trim <- trimWeights(data.svy.rake.ctr,
#                                      lower = 3, upper = 30,  strict = TRUE)




cat("post stratification on ctr, area of Origin & case size\n")
################3
data$stratum <- paste(data$CountryOrigin2, data$Case.size2, data$dem_sex, sep = "/")
#prop.table(table(data$stratum, useNA = "ifany"))

universe$stratum <- paste(universe$CountryOrigin2, universe$Case.size2, universe$dem_sex, sep = "/")
#prop.table(table(universe$stratum, useNA = "ifany"))

teststata <- merge(x = as.data.frame(table(key = universe$stratum) ) , y = as.data.frame(table(key = data$stratum)) , by = "key", all.x = TRUE)


data$stratum[data$stratum == "Central African Republic/Case.size.2.to.5/F"] <- "Central African Republic/Case.size.2.or.more/F"
data$stratum[data$stratum == "Central African Republic/Case.size.6.and.more/F"] <- "Central African Republic/Case.size.2.or.more/F"
universe$stratum[universe$stratum == "Central African Republic/Case.size.2.to.5/F"] <- "Central African Republic/Case.size.2.or.more/F"
universe$stratum[universe$stratum == "Central African Republic/Case.size.6.and.more/F"] <- "Central African Republic/Case.size.2.or.more/F"


data$stratum[data$stratum == "Congo RDC/Case.size.6.and.more/F"] <- "Congo RDC/Case.size.6.and.more"
data$stratum[data$stratum == "Congo RDC/Case.size.6.and.more/M"] <- "Congo RDC/Case.size.6.and.more"
universe$stratum[universe$stratum == "Congo RDC/Case.size.6.and.more/F"] <- "Congo RDC/Case.size.6.and.more"
universe$stratum[universe$stratum == "Congo RDC/Case.size.6.and.more/M"] <- "Congo RDC/Case.size.6.and.more"


data$stratum[data$stratum == "Congo RDC/Case.size.2.to.5/M"] <- "Congo RDC/Case.size.2.to.5"
data$stratum[data$stratum == "Congo RDC/Case.size.2.to.5/F"] <- "Congo RDC/Case.size.2.to.5"
universe$stratum[universe$stratum == "Congo RDC/Case.size.2.to.5/M"] <- "Congo RDC/Case.size.2.to.5"
universe$stratum[universe$stratum == "Congo RDC/Case.size.2.to.5/F"] <- "Congo RDC/Case.size.2.to.5"


data$stratum[data$stratum == "Guinea/Case.size.1/M"] <- "Guinea/Case.size.1.to.5/M"
data$stratum[data$stratum == "Guinea/Case.size.2.to.5/M"] <- "Guinea/Case.size.1.to.5/M"
universe$stratum[universe$stratum == "Guinea/Case.size.1/M"] <- "Guinea/Case.size.1.to.5/M"
universe$stratum[universe$stratum == "Guinea/Case.size.2.to.5/M"] <- "Guinea/Case.size.1.to.5/M"



data$stratum[data$stratum == "Guinea/Case.size.2.to.5/F"] <- "Guinea/Case.size.2.or.more/F"
data$stratum[data$stratum == "Guinea/Case.size.6.and.more/F"] <- "Guinea/Case.size.2.or.more/F"
universe$stratum[universe$stratum == "Guinea/Case.size.2.to.5/F"] <- "Guinea/Case.size.2.or.more/F"
universe$stratum[universe$stratum == "Guinea/Case.size.6.and.more/F"] <- "Guinea/Case.size.2.or.more/F"

#data$stratum[data$stratum == "Cote Ivoire/Case.size.2.to.5/Male"] <- "Cote Ivoire/Case.size.2.or.more/Male"
#data$stratum[data$stratum == "Cote Ivoire/Case.size.6.and.more/Male"] <- "Cote Ivoire/Case.size.2.or.more/Male"
#universe$stratum[universe$stratum == "Cote Ivoire/Case.size.2.to.5/Male"] <- "Cote Ivoire/Case.size.2.or.more/Male"
#universe$stratum[universe$stratum == "Cote Ivoire/Case.size.6.and.more/Male"] <- "Cote Ivoire/Case.size.2.or.more/Male"


data$stratum[data$stratum == "Iraq/Case.size.1/F"] <- "Iraq/Case.size.1"
data$stratum[data$stratum == "Iraq/Case.size.1/M"] <- "Iraq/Case.size.1"
universe$stratum[universe$stratum == "Iraq/Case.size.1/F"] <- "Iraq/Case.size.1"
universe$stratum[universe$stratum == "Iraq/Case.size.1/M"] <- "Iraq/Case.size.1"


data$stratum[data$stratum == "Mali/Case.size.1/F"] <- "Mali/Case.size.2.or.more/F"
data$stratum[data$stratum == "Mali/Case.size.2.to.5/F"] <- "Mali/Case.size.2.or.more/F"
universe$stratum[universe$stratum == "Mali/Case.size.1/F"] <- "Mali/Case.size.2.or.more/F"
universe$stratum[universe$stratum == "Mali/Case.size.2.to.5/F"] <- "Mali/Case.size.2.or.more/F"


data$stratum[data$stratum == "Mali/Case.size.1/M"] <- "Mali/Case.size.2.or.more/M"
data$stratum[data$stratum == "Mali/Case.size.2.to.5/M"] <- "Mali/Case.size.2.or.more/M"
universe$stratum[universe$stratum == "Mali/Case.size.1/M"] <- "Mali/Case.size.2.or.more/M"
universe$stratum[universe$stratum == "Mali/Case.size.2.to.5/M"] <- "Mali/Case.size.2.or.more/M"

#data$stratum[data$stratum == "Other/Case.size.6.and.more/Female"] <- "Other/Case.size.6.and.more"
#data$stratum[data$stratum == "Other/Case.size.6.and.more/Male"] <- "Other/Case.size.6.and.more"
#universe$stratum[universe$stratum == "Other/Case.size.6.and.more/Female"] <- "Other/Case.size.6.and.more"
#universe$stratum[universe$stratum == "Other/Case.size.6.and.more/Male"] <- "Other/Case.size.6.and.more"




#universe.stratum <- table(stratum = universe$stratum)
universe.stratum <- as.data.frame(table(stratum = universe$stratum))

stratum  <- levels(as.factor(data$stratum ))
data.svy.unweighted <- svydesign(ids =  ~ 1,  data = data)
## Try post stratification on ctr, area of Origin & case size
data.svy.rake.ctr.casesize.gender <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ stratum),
  population.margins = list(universe.stratum)
)

str(data.svy.rake.ctr.casesize.gender)

summary(weights(data.svy.rake.ctr.casesize))
#data.svy.rake.ctr.casesize.gender.trim <- trimWeights(data.svy.rake.ctr.casesize.gender,
#                                      lower = 3, upper = 30,  strict = TRUE)

weight <-

str(data.svy.rake.ctr.casesize)
data.svy.rake.ctr.casesize@popStrata

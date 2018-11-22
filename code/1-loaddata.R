rm(list = ls())

## Load all required packages         #############################################
source("code/0-packages.R")
source("code/0-config.R")

### Double Check that you have the last version
#source("https://raw.githubusercontent.com/Edouard-Legoupil/koboloadeR/master/inst/script/install_github.R")
#install.packages("devtools")
#library("devtools")
#install_github("unhcr/koboloadeR")

library(koboloadeR)

## kobo_projectinit()
## Now Position your form & your data in the data folder



## Load form and building dictionnary #############################################
#rm(form)
#form <- "form.xls"
## Generate & Load dictionnary
cat("\n\n\n Generate dictionnary from the xlsform \n\n\n\n")
kobo_dico(form)
dico <- read.csv(paste("data/dico_",form,".csv",sep = ""), encoding = "UTF-8", na.strings = "")
#rm(form)


# Load data #######################################################################
cat("\n\n\n Load original dataset \n\n\n\n")
data.or <- read.csv(path.to.data, sep = ";", encoding = "UTF-8", na.strings = "")

## Account for case when separator is a coma....
if (ncol(data.or) == 1) {
  data.or <- read.csv(path.to.data, sep = ",", encoding = "UTF-8", na.strings = "") } else {
    cat("\n")
  }

#names(data.or)
### Need to replace slash by point in the variable name
## get variable name from data
#datalabel <- as.data.frame( names(data.or))
#names(datalabel)[1] <- "nameor"
#datalabel$nameor <- as.character(datalabel$nameor)

## new variables name without /
#datalabel$namenew <- str_replace_all(datalabel$nameor, "/", ".")
## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
#names(data.or) <- datalabel[, 2]


### Generate anonymisation report  ################################################
##Uncomment ot generate an anonymisation report
#kobo_anonymisation_report(data.or)


## Check to split select_multiple if data is extracted from ODK ###################
cat("\n\n\n Now split select_multiple  variables \n\n\n\n")
#household <- kobo_split_multiple(data.or, dico)
household <- data.or

## Clean variable if any ##########################################################
cat("\n\n\n Clean variable if any \n\n\n\n")
household <- kobo_clean(household, dico)

## Build anonymised version of the frame ##########################################
cat("\n\n\n Anonymise Household \n\n\n\n")
#kobo_anonymise(household, dico)

## Save preliminary version before encoding or adding indicators ##################
cat("\n\nWrite backup before encoding or indicators calculation..\n")
write.csv(household,"data/household.csv", row.names = FALSE, na = "")


## Compute indicators if defined ##################################################
#source("code/2-create-indicators.R")
names(household)
summary(household$arrival.document)
table(household$arrival.document)
levels(household$arrival.document)
household$arrival.document <- as.character(household$arrival.document)
household$arrival.document[household$arrival.document == "onlyUNHCR  المفوضية فقط" ] <- "onlyUNHCR"

table(household$arrival.document)

## Clean Unique forms ########
library(readxl)
#base <- read_excel("data/base.xlsx")
#base <- read_excel("data/base.xlsx")
base <- read_excel("data/base2.xlsx")
#names(base)
#View(base[ ,c("weight", "_id", "_uuid" ,  "_submission_time", "_index" )])

#names(household)

## using metadata column for the merge

baseclean <- base[ ,c(  "_uuid" ,  "weight",  "N° du ménage", "N° Individuel"   )]
#need to rename the column
#names(baseclean)[1] <- "X_id"
#names(baseclean)[3] <- "X_submission_time"
#names(baseclean)[4] <- "X_index"


names(baseclean)[1] <- "X_uuid"
names(baseclean)[3] <- "case_reachable.caseid2"
names(baseclean)[4] <- "Individualid"

baseclean$caseidconcat <- paste0(baseclean$case_reachable.caseid2, baseclean$Individualid)

nrow(baseclean)
nrow(as.data.frame(unique(baseclean$case_reachable.caseid2)))
nrow(as.data.frame(unique(baseclean$Individualid)))
nrow(as.data.frame(unique(baseclean$caseidconcat)))

#household.remove <- merge(x = baseclean, y = household, by = "X_uuid" , all.y = TRUE)
#household.remove <- household.remove[is.na(household.remove$case_reachable.caseid2), ]
household2 <- merge(x = baseclean, y = household, by = "X_uuid", all.x = TRUE)

nrow(household2)

household2 <- household2[ !(is.na(household2$case_reachable.caseid2)), ]
nrow(as.data.frame(unique(household2$case_reachable.caseid2)))
household2$caseidconcatrelation <- paste0(household2$case_reachable.caseid2, household2$sociodemo.relationship)
nrow(as.data.frame(unique(household2$caseidconcatrelation)))

household2$duplicatedidpa <- ""
household2[duplicated(household2$caseidconcatrelation ), c( "duplicatedidpa") ] <- "duplicated"
table(household2$duplicatedidpa)

household2$duplicatedid <- ""
household2[duplicated(household2$case_reachable.caseid2), c( "duplicatedid") ] <- "duplicated"
table(household2$duplicatedid)
## Check if we have differnet head of household


data <- household2[ household2$duplicatedid != "duplicated", ]

######## load extract from proGres for poststratification\

library(survey)
#################################################################
## First load the universe
##loading  case profile from progres


#### post stratification of the sample

## cf tuto here: http://www.andrew.cmu.edu/user/jsmurray/teaching/303/files/lab.html
## https://www.r-bloggers.com/survey-computing-your-own-post-stratification-weights-in-r/
## http://sdaza.com/survey/2012/08/25/raking/


universe <- read.csv("data/casemorocco.csv")
#names(universe)

universe$case_reachable.caseid2 <- universe$CaseNo

## Merge survey with Universe to get common data for poststratification
data <- merge(x = data, y = universe, by = "case_reachable.caseid2")

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
                                  'PAL'='Palestine';
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
                                       'EGU'='Other'")

data$CountryOrigin2 <- car::recode(data$CountryOrigin,"'SYR'='Syria';
                                  'IRQ'='Iraq';
                                   'PAL'='Palestine';
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

data$key2 <- paste(data$CountryOrigin2,data$Case.size2,sep = "-")
universe$key2 <- paste(universe$CountryOrigin2,universe$Case.size2,sep = "-")

prop.table(table(data$key2, useNA = "ifany"))
prop.table(table(universe$key2, useNA = "ifany"))


data$stratum <- paste(data$CountryOrigin2, data$coal2, data$dem_sex, sep = "/")
universe$stratum <- paste(universe$CountryOrigin2, universe$coal2, universe$dem_sex, sep = "/")

prop.table(table(data$stratum, useNA = "ifany"))
prop.table(table(universe$stratum, useNA = "ifany"))



### reference table for post-stratification

universe.ctr <- as.data.frame(table(universe$CountryOrigin2))
names(universe.ctr)[1] <- "CountryOrigin2"
ctr <- levels(as.factor(data$CountryOrigin2))



universe.stratum <- as.data.frame(table(universe$stratum))
names(universe.stratum)[1] <- "stratum"
stratum <- levels(as.factor(data$stratum))

universe.key2 <- as.data.frame(table(universe$key2))
names(universe.key2)[1] <- "key2"
key2 <- levels(as.factor(data$key2))

#data.key2 <- as.data.frame(table(data$key2))

cat("create the unweighted survey object\n")
## create the unweighted survey object
data.svy.unweighted <- svydesign(ids =  ~ 1,
                                 data = data)

## Post stratify on those relative frequency



cat("post stratification on ctr, area of Origin & case size\n")
## Try post stratification on ctr, area of Origin & case size
data.svy.rake.ctr.coo.size <- rake(
  design = data.svy.unweighted,
  #partial = TRUE, ## Ignore some strata that are absent from sample
  sample.margins = list( ~ key2),
  population.margins = list(universe.key2)
)



cat("post stratification only on ctr of asylum\n")
## Try post stratification only on ctr of asylum
data.svy.rake.ctr <- rake(
  design = data.svy.unweighted,
  sample.margins = list( ~ ctr),
  population.margins = list(universe.ctr)
)


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
summary(weights(data.svy.rake.ctr.coo))

cat("Trim weight for Asylum, Origin\n")
data.svy.rake.trim <- trimWeights(data.svy.rake.ctr,
                                  lower = 0.3,
                                  upper = 3,
                                  strict = TRUE)

proportion.trim <- svyby(  ~ group_intro.goingback,  by =  ~ ctr+COO_L1,  design = data.svy.rake.trim,  FUN = svymean)

#names(proportion.trim)
proportion.trim$key <- paste(proportion.trim$ctr,proportion.trim$COO_L1,sep="-")


## Get original # from universe

##############################################################################
### Second one with case size in addition

cat("Trim weight for Asylum, Origin & Case size\n")

data.svy.rake.trim2 <- trimWeights(data.svy.rake.ctr.coo.size,
                                   lower = 0.3,
                                   upper = 3,
                                   strict = TRUE)

proportion.trim2 <- svyby(  ~ group_intro.goingback,  by =  ~ ctr+COO_L1+Case.size,  design = data.svy.rake.trim2,  FUN = svymean)







###########
library(haven)
data_weight <- read_dta("data/data_weight.dta")
data_weight <- as.data.frame(data_weight)
names(data_weight)
View(data_weight[ , c("n_dum_nage",  "individual_number" )])

data_weight$caseidconcat <- paste0(data_weight$n_dum_nage, data_weight$individual_number)

nrow(data_weight)
nrow(as.data.frame(unique(data_weight$n_dum_nage)))
nrow(as.data.frame(unique(data_weight$individual_number)))
nrow(as.data.frame(unique(data_weight$caseidconcat)))

### does not match.... some id are NA

### Add weights ###############################

poids <- read_excel("data/poids.xlsx")
names(poids)
names(poids)[2] <- "Caseid"
names(poids)[3] <- "Individualid"
names(poids)[19] <- "poidsnormalise1"
names(poids)[20] <- "poidsnormalise2"
names(poids)

poids$caseidconcat <- paste0(poids$Caseid, poids$Individualid)


nrow(poids)
nrow(as.data.frame(unique(poids$Caseid)))
nrow(as.data.frame(unique(poids$Individualid)))
nrow(as.data.frame(unique(poids$caseidconcat)))


#View(poids[ is.na(poids$Individualid),c("caseidconcat") ])
## We Case with empty case id...
poids2 <- poids[ !(is.na(poids$Individualid)), c("Caseid", "Individualid",
                                                "poids1",  "poids2", "poidsnormalise1", "poidsnormalise2",  "Weight")  ]
str(poids2)

poids2$poids1 <- as.numeric(poids2$poids1)
poids2$poids2 <- as.numeric(poids2$poids2)
poids2$poidsnormalise1 <- as.numeric(poids2$poidsnormalise1)
poids2$poidsnormalise2 <- as.numeric(poids2$poidsnormalise2)
poids2$Weight <- as.numeric(poids2$Weight)

sum(poids2$Weight)
sum(poids2$poids1)
sum(poids2$poids2)
sum(poids2$poidsnormalise1)
sum(poids2$poidsnormalise2)

#View(poids2[ is.na(poids2$Caseid), ])

## Now merging back with household to get all caseID

household3 <- merge(x = household2, y = poids2, by = "Individualid", all.x = TRUE)

#View(household3[ is.na(household3$Caseid) | is.na(household3$case_reachable.caseid2) ,c("Caseid", "case_reachable.caseid2", "case_reachable.caseid") ])

View(household3[ ,c("Caseid", "case_reachable.caseid2", "case_reachable.caseid", "weight","Weight") ])

str(household3$weight)
sum(household3$weight)

str(household3$weight)
sum(household3$Weight)

nrow(household3)
nrow(as.data.frame(unique(household3$case_reachable.caseid2)))
nrow(as.data.frame(unique(household3$Caseid)))



nrow(as.data.frame(unique(household3$case_reachable.caseid2)))

## Re-encoding data now based on the dictionnary -- ##############################
## the xlsform dictionnary can be adjusted this script re-runned till satisfaction
cat("\n\n\n Now  re-encode data  \n\n\n\n")
household <- kobo_encode(household, dico)


## Cheking the labels matching... #################################################
## household is the default root data componnents to be used -- in order to deal with nested dataset
cat("\n\n\n Now  labeling variables \n\n\n\n")
household <- kobo_label(household, dico)


## We now save a back up in the data folder to be used for the Rmd  ###############
cat("\n\nWrite backup ready for report generation \n")
write.csv(household,"data/data2.csv", row.names = FALSE, na = "")


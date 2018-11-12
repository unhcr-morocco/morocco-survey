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
base <- read_excel("data/base.xlsx")

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

household22 <- household2[ !(is.na(household2$case_reachable.caseid2)), ]

nrow(as.data.frame(unique(household22$case_reachable.caseid2)))



household22$caseidconcatrelation <- paste0(household22$case_reachable.caseid2, household22$sociodemo.relationship)
nrow(as.data.frame(unique(household22$caseidconcatrelation)))

household22$duplicated <- ""
household22[duplicated(household22$caseidconcatrelation ), c( "duplicated") ] <- "duplicated"

table(household22$duplicated)

## Check if we have differnet head of household


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


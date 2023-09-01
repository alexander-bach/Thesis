###############################################################

# ARAapp: Filling gaps in the ecological knowledge of spiders using 
# an automated and dynamic approach to analyze systematic collected community data

#Bach et al. 2023

###############################################################

# R code to reproduce the shiny application and analyses
# missing database querries and first preprocessing steps

# Author: Alexander Bach
# Last update: 06.07.2023

###############################################################


#### load required packages ####
library(vegan)
library(here)
library(reshape)
library(tibble)
library(plyr)
library(dplyr)
library(tidyr)
library(shiny)
library(ggplot2)
library(labdsv)
library(plotly)
library(RCurl)
library(stringr)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(data.table)
library(readxl)

#### data import and format ####

#data import
ARAMOB_data <- as.data.frame(read_xlsx(here::here("data_scheme.xlsx"), sheet = "data"))
EUNIS_list <- as.data.frame(read_xlsx(here::here("data_scheme.xlsx"), sheet = "eunis"))
TRAIT_list <- as.data.frame(read_xlsx(here::here("data_scheme.xlsx"), sheet = "traits"))

#correct data format
ARAMOB_data$startdate <- as.Date(ARAMOB_data$startdate)
ARAMOB_data$enddate <- as.Date(ARAMOB_data$enddate)

#create species and tool list for shiny UI
Species <- ARAMOB_data$LastIdentificationCache %>% unique()
#Species <- unique(Species)
Tool <- c("Companion species", "Ecological niches", "Habitat preferences", "Phenology", "Vertical distribution")


#### data preparation steps for ARAapp tools ####

#split data in site and species 
ARAMOB_Sites <- ARAMOB_data[,c(1,5:19)]
ARAMOB_Species <- ARAMOB_data[,c(2:5)]

#summarize collection event data on same sampling plot (add different pitfall trap emptying periods on same plot)
ARAMOB_Sites <- ARAMOB_Sites[!duplicated(ARAMOB_Sites[c(2,3,4)]),] %>% 
  group_by(LocalityDescription) 
ARAMOB_Sites_m <- summarise(ARAMOB_Sites, CollectionTimeSpan = sum(CollectionTimeSpan))

#create unique sampling plot list
ARAMOB_Sites <- ARAMOB_Sites[!duplicated(ARAMOB_Sites$LocalityDescription),]
ARAMOB_Sites <- with(ARAMOB_Sites, ARAMOB_Sites[order(LocalityDescription),])

#add summarized collection time span data
ARAMOB_Sites$CollectionTimeSpan <- ARAMOB_Sites_m$CollectionTimeSpan[match(ARAMOB_Sites$LocalityDescription, ARAMOB_Sites_m$LocalityDescription)]
ARAMOB_Sites <- ARAMOB_Sites[order(ARAMOB_Sites$LocalityDescription),]

#summarize male+female spiders per sampling plot
ARAMOB_Species$NumberOfUnits <- as.numeric(ARAMOB_Species$NumberOfUnits)
ARAMOB_Species <- group_by(ARAMOB_Species, LocalityDescription, LastIdentificationCache) %>% 
  summarise(NumberOfUnits = sum(NumberOfUnits))

#### preparations for ARAphenology ####

#exclude data with missing collection time data
ARAMOB_Pheno <- ARAMOB_data[complete.cases(ARAMOB_data[ , 18:19]),]

#calculate date median
ARAMOB_Pheno$date <- ARAMOB_Pheno$startdate + floor((ARAMOB_Pheno$enddate-ARAMOB_Pheno$startdate)/2) 

#summarize number of units values
ARAMOB_Pheno <- group_by(ARAMOB_Pheno, LastIdentificationCache, LocalityDescription, CollectionTimeSpan,
                         Value, Gender, Altitude, CollectingMethod, date) %>% 
                    summarise(NumberOfUnits = sum(NumberOfUnits))


#factorize Collectingmethod for UI list and define Number of Sites
ARAMOB_Pheno$CollectingMethod <- as.factor(ARAMOB_Pheno$CollectingMethod)
SitesPhen1 <- length(unique(ARAMOB_Pheno$LocalityDescription))

#### preparations for ARAhabitat ####

#remove sites missing a EUNIS Class or pitfall trap data
ARAMOB_Sites_E <- filter(ARAMOB_Sites, !is.na(EUNIS)) %>% 
  filter(!is.na(Value)) %>% 
  filter(Value > 1) %>% 
  filter(CollectionTimeSpan > 1)

#splitting EUNIS level to singeletons and add as new columns
l1 <- lapply(strsplit(as.character(ARAMOB_Sites_E$EUNIS), ''), 
             function(i){i1 <- Reduce(paste0, i, accumulate = TRUE); 
             i1 <- i1[!grepl('\\.$', i1)]; 
             i1})
final_list <- lapply(l1, `length<-`, max(lengths(l1)))
ARAMOB_Sites_E <- cbind.data.frame(ARAMOB_Sites_E, do.call(rbind, final_list), stringsAsFactors = FALSE)

#add correct columns names
colnames(ARAMOB_Sites_E)[11] <- "Lowest_EUNIS_LEVEL"
#ARAMOB_Sites_E$sixth <- NA
names(ARAMOB_Sites_E)[17:22] <- c("first", "second", "third", "fourth", "fivth", "sixth")

#calculate sum of all sites with same EUNIS level
SumsFirst <- group_by(ARAMOB_Sites_E, first) %>%
  summarise(first_sums = length(first))
ARAMOB_Sites_E <- merge(ARAMOB_Sites_E, SumsFirst)

SumsSecond <- group_by(ARAMOB_Sites_E, second) %>%
  summarise(second_sums = length(second))
ARAMOB_Sites_E <- merge(ARAMOB_Sites_E, SumsSecond)

SumsThird <- group_by(ARAMOB_Sites_E, third) %>%
  summarise(third_sums = length(third))
ARAMOB_Sites_E <- merge(ARAMOB_Sites_E, SumsThird)

SumsFourth <- group_by(ARAMOB_Sites_E, fourth) %>%
  summarise(fourth_sums = length(fourth))
ARAMOB_Sites_E <- merge(ARAMOB_Sites_E, SumsFourth)

SumsFivth <- group_by(ARAMOB_Sites_E, fivth) %>%
  summarise(fivth_sums = length(fivth))
ARAMOB_Sites_E <- merge(ARAMOB_Sites_E, SumsFivth)

SumsSixth <- group_by(ARAMOB_Sites_E, sixth) %>%
  summarise(sixth_sums = length(sixth))
ARAMOB_Sites_E <- merge(ARAMOB_Sites_E, SumsSixth)

ARAMOB_Sites_E <- ARAMOB_Sites_E[ , order(names(ARAMOB_Sites_E))]

#standardize count data according to Saska et al (2021)
ARAMOB_Species_E <- merge(ARAMOB_Species, ARAMOB_Sites_E[,c(9,18,28)], by = "LocalityDescription")  #28 wenn 6.te EUNIS Klasse besetzt
ARAMOB_Species_E$NumberOfUnits <- ((ARAMOB_Species_E$NumberOfUnits/ARAMOB_Species_E$CollectionTimeSpan)/
                                     ARAMOB_Species_E$Value)

#format corresponding sampling plot data
ARAMOB_Sites_E <- column_to_rownames(ARAMOB_Sites_E, "LocalityDescription") 
ARAMOB_Sites_E$LocalityDescription <- NULL

#filter species for available sampling plot data and creata site-by-species matrix
ARAMOB_Species_E <- setDT(subset(ARAMOB_Species_E, LocalityDescription %in% row.names(ARAMOB_Sites_E)))
CrossTable_E <- dcast(ARAMOB_Species_E, LastIdentificationCache~LocalityDescription, 
                      value.var = "NumberOfUnits", sum) %>% as.data.frame()

#add taxon names as row names
row.names(CrossTable_E) <- CrossTable_E$LastIdentificationCache
CrossTable_E$LastIdentificationCache <- NULL

#get available sampling plot number for shiny UI
SitesEUNIS1 <- ncol(CrossTable_E)

#fix rownames and  format, sort
factor_col <- c("first", "second", "third", "fourth", "fivth", "Lowest_EUNIS_LEVEL")
numerical_col <- c("first_sums", "second_sums", "third_sums", "fourth_sums", "fivth_sums")
ARAMOB_Sites_E[factor_col] <- lapply(ARAMOB_Sites_E[factor_col], as.factor)
ARAMOB_Sites_E[numerical_col] <- lapply(ARAMOB_Sites_E[numerical_col], as.numeric)
ARAMOB_Sites_E <- ARAMOB_Sites_E[order(row.names(ARAMOB_Sites_E)), ]

#generate EUNIS list with codes
EUNIS_Hierachy <- EUNIS_list[,c(2,1)]
colnames(EUNIS_Hierachy) <- c("EUNIS_Class", "EUNIS_Name")

#transpose data frame
SpiderData_Etrans <- as.data.frame(as.matrix(t(CrossTable_E)))
SpiderData_Etrans <- SpiderData_Etrans[order(row.names(SpiderData_Etrans)),]
row.names(SpiderData_Etrans) <- colnames(CrossTable_E)
colnames(SpiderData_Etrans) <- rownames(CrossTable_E)

#calculate frequency for every eunis level (only up to level 3 at the moment)
SpiderData_FreqFirst <- const(SpiderData_Etrans, ARAMOB_Sites_E$first, minval = 0, digits = 10)
SpiderData_FreqSecond <- const(SpiderData_Etrans, ARAMOB_Sites_E$second, minval = 0, digits = 10)
SpiderData_FreqThird <- const(SpiderData_Etrans, ARAMOB_Sites_E$third, minval = 0, digits = 10)
#SpiderData_FreqFourth <- const(SpiderData_Etrans, ARAMOB_Sites_E$fourth, minval = 0, digits = 10)
#SpiderData_FreqFivth <- const(SpiderData_Etrans, ARAMOB_Sites_E$fivth, minval = 0, digits = 10)
#SpiderData_FreqSixth <- const(SpiderData_Etrans, ARAMOB_Sites_E$sixth, minval = 0, digits = 10)

#### preparations for ARAaltitude ####

#remove sites missing altitude or pitfall data
ARAMOB_Sites_Alt <- filter(ARAMOB_Sites, !is.na(Altitude)) %>% filter(!is.na(Value)) %>% filter(Value > 1)

#normalize abundance data according to Saska et al. (2021)
ARAMOB_Species_Alt <- inner_join(ARAMOB_Species, ARAMOB_Sites_Alt[,c(2,9,13)], by = "LocalityDescription")
ARAMOB_Species_Alt$NumberOfUnits <- ((ARAMOB_Species_Alt$NumberOfUnits/ARAMOB_Species_Alt$CollectionTimeSpan)/
                                       ARAMOB_Species_Alt$Value)

#set correct column names
ARAMOB_Sites_Alt <- column_to_rownames(ARAMOB_Sites_Alt, "LocalityDescription")

#create data.table and filter for sites with correct altitude data
ARAMOB_Species_Alt <- setDT(subset(ARAMOB_Species_Alt, LocalityDescription %in% row.names(ARAMOB_Sites_Alt)))

#build crosstable
CrossTable_Alt <- dcast(ARAMOB_Species_Alt, LastIdentificationCache~LocalityDescription, 
                        value.var = "NumberOfUnits", sum) %>% as.data.frame()
row.names(CrossTable_Alt) <- CrossTable_Alt$LastIdentificationCache
CrossTable_Alt$LastIdentificationCache <- NULL

#transpose and order crosstable
CrossTable_Alt_t <- as.data.frame(as.matrix(t(CrossTable_Alt)))
CrossTable_Alt_t <- CrossTable_Alt_t[order(row.names(CrossTable_Alt_t)),]

#build altitude categories per 100m classes
ARAMOB_Sites_Alt$Altitude <- ceiling(ARAMOB_Sites_Alt$Altitude / 100 ) * 100

#get the number of sites in every altitude class to normalize count data
SumsAltGroups <- group_by(ARAMOB_Sites_Alt, Altitude) %>%
  summarise(CountNumber = length(Altitude))
ARAMOB_Sites_Alt <- inner_join(ARAMOB_Sites_Alt, SumsAltGroups, by = "Altitude")

#calculate species frequencies
SpiderData_FreqAlt <- const(CrossTable_Alt_t, ARAMOB_Sites_Alt$Altitude, minval = 0, digits = 10)

#save number of sites for shiny UI
SitesAlti1 <- nrow(CrossTable_Alt_t)

#### generate necessary data for welcome shiny UI ####

NoOfSites <- nrow(ARAMOB_Sites)
NoOfUnits <- format(sum(ARAMOB_Species$NumberOfUnits), big.mark = '.', decimal.mark = ",")
NoOfSpec <- length(unique(ARAMOB_Species$LastIdentificationCache))
ColMinYear <- min(ARAMOB_Sites$CollectionEndYear, na.rm = TRUE)
ColMaxYear <- max(ARAMOB_Sites$CollectionEndYear, na.rm = TRUE)


#### preparations for ARAniche ####

#using filters on site data from from Entling et al. (2007)
ARAMOB_Sites_Niche <- filter(ARAMOB_Sites, !is.na(Value)) %>% 
  filter(Value >= 3) %>% 
  filter(CollectionTimeSpan >= 90) %>%
  filter(Altitude <= 800)

#apply filters
CrossTable_Niche <- CrossTable_E[, colnames(CrossTable_E) %in% ARAMOB_Sites_Niche$LocalityDescription]
CrossTable_Niche <- CrossTable_Niche[colSums(CrossTable_Niche)!=0, ]

#save number of sites for shiny UI
SitesNiche1 <- ncol(CrossTable_Niche)

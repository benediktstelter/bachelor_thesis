#Import necessary packages
library(tidyverse)
library(readxl)
library(Synth)
library(SCtools)



#Import covid case dataset
covid_cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")


#Import covariates:

##Import GDP per capita
per_capita_gdp <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\gdp_per_capita.xlsx",sheet = "Blatt 1") 
per_capita_gdp <- per_capita_gdp[10:22,1:2]
names(per_capita_gdp)
names(per_capita_gdp)[names(per_capita_gdp) == "Daten abgefragt am 29/12/2022 15:37:21 von [ESTAT]"] <- "country"
names(per_capita_gdp)[names(per_capita_gdp) == "...2"] <- "gdpcapita20"


##Import population density
pop_density <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\pop_density.xlsx",sheet = "Sheet 1") 
pop_density <- pop_density[9:21,1:2]
names(pop_density)
names(pop_density)[names(pop_density) == "Data extracted on 29/12/2022 16:30:19 from [ESTAT]"] <- "country"
names(pop_density)[names(pop_density) == "...2"] <- "popdensity19"


##Import share of elderly
share_elderly <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\share_elderly.xlsx",sheet = "Blatt 1")
share_elderly <- share_elderly[9:21,1:2]
names(share_elderly)
names(share_elderly)[names(share_elderly) == "Daten abgefragt am 29/12/2022 15:52:45 von [ESTAT]"] <- "country"
names(share_elderly)[names(share_elderly) == "...2"] <- "elderly20"
share_elderly$elderly20 <- as.numeric(sub("%", "",share_elderly$elderly20,fixed=TRUE))/100


##Import tertiary education
share_tertiary_ed <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\share_tertiary_ed.xlsx",
                                sheet = "Blatt 1")
share_tertiary_ed <- share_tertiary_ed[12:24,1:2]
names(share_tertiary_ed)
names(share_tertiary_ed)[names(share_tertiary_ed) == "Daten abgefragt am 29/12/2022 15:42:33 von [ESTAT]"] <- "country"
names(share_tertiary_ed)[names(share_tertiary_ed) == "...2"] <- "tertiary20"
share_tertiary_ed$tertiary20 <- as.numeric(sub("%", "",share_tertiary_ed$tertiary20,fixed=TRUE))/100


##Import influenza vaccination rates
influenza_vacc_rates <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\influenza_vaccination_rates.xlsx",
                                   sheet = "Blatt 1")
influenza_vacc_rates <- influenza_vacc_rates[9:21,1:2]
names(influenza_vacc_rates)
names(influenza_vacc_rates)[names(influenza_vacc_rates) == "Daten abgefragt am 29/12/2022 15:26:09 von [ESTAT]"] <- "country"
names(influenza_vacc_rates)[names(influenza_vacc_rates) == "...2"] <- "inflzvacc19"
influenza_vacc_rates$inflzvacc19 <- as.numeric(sub("%", "",influenza_vacc_rates$inflzvacc19,fixed=TRUE))/100


#Merge covariates with country names and name countries in English
df_list <- list(influenza_vacc_rates, per_capita_gdp, pop_density, share_elderly, share_tertiary_ed)
predictors <- df_list %>% reduce(full_join, by='country')
predictors$country <- c("Bulgaria", "Czechia", "Estonia", "Greece", "Croatia", "Latvia", "Lithuania", "Hungary", "Austria",
                        "Poland", "Romania", "Slovenia", "Slovakia")


#Import data from Wellcome Global monitor (Covariate trust in science)
trst_science <- data.frame (country  = c("Bulgaria", "Czechia", "Estonia", "Greece", "Croatia", "Latvia", "Lithuania",
                                         "Hungary", "Austria", "Poland", "Romania", "Slovenia", "Slovakia"),
                            trstscinc20 = c(0.819,0.948,0.918,0.830,0.895,0.851,0.836,0.936,0.895,0.872,0.795,0.889,0.923))


#Merge this covariate with the other covariates and add country ids
sndmerge <- list(predictors, trst_science)
predictors <- sndmerge %>% reduce(full_join, by='country')
predictors$countryid <- c(1:13)



#Import Vaccination datset from our world in data and remove irrelevant countries
vaccine <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
donor_countries <- "Bulgaria|Czechia|Estonia|Greece|Croatia|Latvia|Lithuania|Hungary|Austria|Poland|Romania|Slovenia|Slovakia"
donor_pool <- vaccine[grep(donor_countries, vaccine$location), ]


#Isolating for vaccinated per hundred (removing the rest of the dataset)
poland_lottery <- donor_pool[, c(1:3,11,12) ]
names(poland_lottery)[names(poland_lottery) == "location"] <- "country"


#Merge covariates and vaccination data
mrgefrst <- list(poland_lottery, predictors)
poland_lottery <- mrgefrst %>% reduce(full_join, by='country')


#Transform percentage values into decimal values and name variables properly
poland_lottery <- as.data.frame(poland_lottery)
poland_lottery$people_vaccinated_per_hundred <- as.numeric(sub("%", "",poland_lottery$people_vaccinated_per_hundred,fixed=TRUE))/100
names(poland_lottery)[names(poland_lottery) == "people_vaccinated_per_hundred"] <- "onedose"

poland_lottery$people_fully_vaccinated_per_hundred <- as.numeric(sub("%", "",poland_lottery$people_fully_vaccinated_per_hundred,fixed=TRUE))/100
names(poland_lottery)[names(poland_lottery) == "people_fully_vaccinated_per_hundred"] <- "twodoses"


#Remove 2020 and 2022
poland_lottery <- poland_lottery[!grepl("2022|2020", poland_lottery$date), ]


#Impute missing values (TSimpute package: installation did not work, therefore functions
#are imported manually)
setwd("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data")
source("kalman.R")

##Impute values for Poland
poland <- poland_lottery[grep("Poland", poland_lottery$country), ]
poland <- na_interpolation(poland)
remaining <- poland_lottery[!grepl("Poland", poland_lottery$country), ]

##Impute values for Estonia
estonia <- remaining[grep("Estonia", remaining$country), ]
estonia <- na_interpolation(estonia)
remaining <- remaining[!grepl("Estonia", remaining$country), ]

##Impute values for Hungary
hungary <- remaining[grep("Hungary", remaining$country), ]
hungary <- na_interpolation(hungary)
remaining <- remaining[!grepl("Hungary", remaining$country), ]

##Impute values for Lithuania
lithuania <- remaining[grep("Lithuania", remaining$country), ]
lithuania <- na_interpolation(lithuania)
remaining <- remaining[!grepl("Lithuania", remaining$country), ]

##Impute values for Romania
romania <- remaining[grep("Romania", remaining$country), ]
romania <- na_interpolation(romania)
remaining <- remaining[!grepl("Romania", remaining$country), ]

##Impute values for Slovenia
slovenia <- remaining[grep("Slovenia", remaining$country), ]
slovenia <- na_interpolation(slovenia)
remaining <- remaining[!grepl("Slovenia", remaining$country), ]

##Impute values for Slovakia
slovakia <- remaining[grep("Slovakia", remaining$country), ]
slovakia <- na_interpolation(slovakia)
remaining <- remaining[!grepl("Slovakia", remaining$country), ]

##Impute values for Croatia
croatia <- remaining[grep("Croatia", remaining$country), ]
croatia <- na_interpolation(croatia)
remaining <- remaining[!grepl("Croatia", remaining$country), ]

##Impute values for Bulgaria
bulgaria <- remaining[grep("Bulgaria", remaining$country), ]
bulgaria <- na_interpolation(bulgaria)
remaining <- remaining[!grepl("Bulgaria", remaining$country), ]


#Merge countries with imputed values and countries without imputed values
mrgepol <- list(remaining, slovakia, bulgaria, croatia, slovenia, romania, lithuania, hungary, estonia, poland)
poland_lottery <- bind_rows(mrgepol)


#Remove January (data availability not good)
poland_lottery <- subset(poland_lottery, date >= as.Date("2021-02-01") & date <= as.Date("2021-12-31"))


#Transform variables to numerical variables and add numeric date for synth package
poland_lottery$date <- as.Date(poland_lottery$date)
poland_lottery$date2 <- as.numeric(poland_lottery$date)
poland_lottery$gdpcapita20 <- as.numeric(poland_lottery$gdpcapita20)
poland_lottery$popdensity19 <- as.numeric(poland_lottery$popdensity19)

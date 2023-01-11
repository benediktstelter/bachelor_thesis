library(tidyverse)
library(readxl)
library(Synth)
library(SCtools)
library(imputeTS)


percent <- function(x, digits = 2, format = "f", ...) {      # Create user-defined function
  paste0(formatC(x * 100, format = format, digits = digits, ...))
}


#GDP
per_capita_gdp <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\gdp_per_capita.xlsx",sheet = "Blatt 1") 
per_capita_gdp <- per_capita_gdp[10:22,1:2]
names(per_capita_gdp)
names(per_capita_gdp)[names(per_capita_gdp) == "Daten abgefragt am 29/12/2022 15:37:21 von [ESTAT]"] <- "country"
names(per_capita_gdp)[names(per_capita_gdp) == "...2"] <- "gdpcapita20"


#Population density
pop_density <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\pop_density.xlsx",sheet = "Sheet 1") 
pop_density <- pop_density[9:21,1:2]
names(pop_density)
names(pop_density)[names(pop_density) == "Data extracted on 29/12/2022 16:30:19 from [ESTAT]"] <- "country"
names(pop_density)[names(pop_density) == "...2"] <- "popdensity19"


#Share of elderly
share_elderly <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\share_elderly.xlsx",sheet = "Blatt 1")
share_elderly <- share_elderly[9:21,1:2]
names(share_elderly)
names(share_elderly)[names(share_elderly) == "Daten abgefragt am 29/12/2022 15:52:45 von [ESTAT]"] <- "country"
names(share_elderly)[names(share_elderly) == "...2"] <- "elderly20"
share_elderly$elderly20 <- as.numeric(sub("%", "",share_elderly$elderly20,fixed=TRUE))/100


#Tertiary education
share_tertiary_ed <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\share_tertiary_ed.xlsx",
                                sheet = "Blatt 1")
share_tertiary_ed <- share_tertiary_ed[12:24,1:2]
names(share_tertiary_ed)
names(share_tertiary_ed)[names(share_tertiary_ed) == "Daten abgefragt am 29/12/2022 15:42:33 von [ESTAT]"] <- "country"
names(share_tertiary_ed)[names(share_tertiary_ed) == "...2"] <- "tertiary20"
share_tertiary_ed$tertiary20 <- as.numeric(sub("%", "",share_tertiary_ed$tertiary20,fixed=TRUE))/100


#Influenza
influenza_vacc_rates <- read_excel("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data\\influenza_vaccination_rates.xlsx",
                                sheet = "Blatt 1")
influenza_vacc_rates <- influenza_vacc_rates[9:21,1:2]
names(influenza_vacc_rates)
names(influenza_vacc_rates)[names(influenza_vacc_rates) == "Daten abgefragt am 29/12/2022 15:26:09 von [ESTAT]"] <- "country"
names(influenza_vacc_rates)[names(influenza_vacc_rates) == "...2"] <- "inflzvacc19"
influenza_vacc_rates$inflzvacc19 <- as.numeric(sub("%", "",influenza_vacc_rates$inflzvacc19,fixed=TRUE))/100
                  
                  
#Merging
df_list <- list(influenza_vacc_rates, per_capita_gdp, pop_density, share_elderly, share_tertiary_ed)
predictors <- df_list %>% reduce(full_join, by='country')
predictors$country <- c("Bulgaria", "Czechia", "Estonia", "Greece", "Croatia", "Latvia", "Lithuania", "Hungary", "Austria",
                                          "Poland", "Romania", "Slovenia", "Slovakia")


#Wellcome Global monitor
trst_science <- data.frame (country  = c("Bulgaria", "Czechia", "Estonia", "Greece", "Croatia", "Latvia", "Lithuania",
                                         "Hungary", "Austria", "Poland", "Romania", "Slovenia", "Slovakia"),
                            trstscinc20 = c(0.819,0.948,0.918,0.830,0.895,0.851,0.836,0.936,0.895,0.872,0.795,0.889,0.923))


#Merge again
sndmerge <- list(predictors, trst_science)
predictors <- sndmerge %>% reduce(full_join, by='country')
predictors$countryid <- c(1:13)



#Vaccination datset
vaccine <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
donor_countries <- "Bulgaria|Czechia|Estonia|Greece|Croatia|Latvia|Lithuania|Hungary|Austria|Poland|Romania|Slovenia|Slovakia"
donor_pool <- vaccine[grep(donor_countries, vaccine$location), ]


#Isolating for vaccinated per hundred
poland_lottery <- donor_pool[, c(1:3,11,12) ]
names(poland_lottery)[names(poland_lottery) == "location"] <- "country"
mrgefrst <- list(poland_lottery, predictors)
poland_lottery <- mrgefrst %>% reduce(full_join, by='country')

poland_lottery <- as.data.frame(poland_lottery)
poland_lottery$people_vaccinated_per_hundred <- as.numeric(sub("%", "",poland_lottery$people_vaccinated_per_hundred,fixed=TRUE))/100
names(poland_lottery)[names(poland_lottery) == "people_vaccinated_per_hundred"] <- "onedose"

poland_lottery$people_fully_vaccinated_per_hundred <- as.numeric(sub("%", "",poland_lottery$people_fully_vaccinated_per_hundred,fixed=TRUE))/100
names(poland_lottery)[names(poland_lottery) == "people_fully_vaccinated_per_hundred"] <- "twodoses"


#Dropping Croatia, Slovakia, Bulgaria and Greece (no data and intervention)
drop_list <- "Greece"
poland_lottery <- poland_lottery[!grepl(drop_list, poland_lottery$country), ]

#Remove 2020 and 2022
poland_lottery <- poland_lottery[!grepl("2022|2020", poland_lottery$date), ]

#experiment:
setwd("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data")
source("kalman.R")

poland <- poland_lottery[grep("Poland", poland_lottery$country), ]
poland <- na_interpolation(poland)
remaining <- poland_lottery[!grepl("Poland", poland_lottery$country), ]


estonia <- remaining[grep("Estonia", remaining$country), ]
estonia <- na_interpolation(estonia)
remaining <- remaining[!grepl("Estonia", remaining$country), ]


hungary <- remaining[grep("Hungary", remaining$country), ]
hungary <- na_interpolation(hungary)
remaining <- remaining[!grepl("Hungary", remaining$country), ]


lithuania <- remaining[grep("Lithuania", remaining$country), ]
lithuania <- na_interpolation(lithuania)
remaining <- remaining[!grepl("Lithuania", remaining$country), ]


romania <- remaining[grep("Romania", remaining$country), ]
romania <- na_interpolation(romania)
remaining <- remaining[!grepl("Romania", remaining$country), ]


slovenia <- remaining[grep("Slovenia", remaining$country), ]
slovenia <- na_interpolation(slovenia)
remaining <- remaining[!grepl("Slovenia", remaining$country), ]

slovakia <- remaining[grep("Slovakia", remaining$country), ]
slovakia <- na_interpolation(slovakia)
remaining <- remaining[!grepl("Slovakia", remaining$country), ]


croatia <- remaining[grep("Croatia", remaining$country), ]
croatia <- na_interpolation(croatia)
remaining <- remaining[!grepl("Croatia", remaining$country), ]


bulgaria <- remaining[grep("Bulgaria", remaining$country), ]
bulgaria <- na_interpolation(bulgaria)
remaining <- remaining[!grepl("Bulgaria", remaining$country), ]

mrgepol <- list(remaining, slovakia, bulgaria, croatia, slovenia, romania, lithuania, hungary, estonia, poland)

poland_lottery <- bind_rows(mrgepol)

poland_lottery <- subset(poland_lottery, date >= as.Date("2021-02-01") & date <= as.Date("2021-12-31"))


poland_lottery$date <- as.Date(poland_lottery$date)
poland_lottery$date2 <- as.numeric(poland_lottery$date)
poland_lottery$gdpcapita20 <- as.numeric(poland_lottery$gdpcapita20)
poland_lottery$popdensity19 <- as.numeric(poland_lottery$popdensity19)


#Removing Austria
first_dose_share <- first_dose_share[!grepl("Austria", first_dose_share$country), ]

#"trstscinc20"

#SCM
dataprep.out <- dataprep(foo = first_dose_share, 
         predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20"),
         predictors.op = "mean",
         dependent = "onedose", unit.variable = "countryid",
         time.variable = "date2", treatment.identifier = 10,
         controls.identifier = c(1,3,5:8),
         time.predictors.prior = c(18659:18772),
         time.optimize.ssr = c(18659:18772), time.plot = c(18659:18992),
         unit.names.variable = "country")

synth.out = synth(dataprep.out)

path.plot(dataprep.res = dataprep.out, synth.res = synth.out,Xlab="Date",Ylab="Share of Population fully vaccinated",Main="Comparison of Synth vs. Actual Vaccination Rate")
abline(v=18772,lty=2,col="red")
abline(v=18900,lty=2,col="red")


synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
# results tables:
print(synth.tables)

gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"))

tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 2)
## Test how extreme was the observed treatment effect given the placebos:
ratio <- mspe.test(tdf)
ratio$p.val
mspe.plot(tdf, discard.extreme = FALSE)
plot_placebos(
  tdf = tdf,
  discard.extreme = FALSE,
  mspe.limit = 20,
  alpha.placebos = 1)


# Let's pull out the data from the result, to make our own nicer plots in ggplot of course
synth_data_out = data.frame(dataprep.out$Y0plot%*%synth.out$solution.w) 
date = as.numeric(row.names(synth_data_out))
plot.df = data.frame(twodoses=first_dose_share$twodoses[first_dose_share$countryid==10 & first_dose_share$date2 %in% date])
plot.df$synth = synth_data_out$w.weight
plot.df$date <- first_dose_share$date[first_dose_share$countryid==10 & first_dose_share$date2 %in% date]


SCM_plot <- ggplot(plot.df,aes(y=twodoses,x=date,linetype="solid")) + geom_line(size=0.8) + 
  geom_line(aes(y=synth,x=date,linetype="dashed"),size=0.8) +
  geom_vline(xintercept=18772,color="red",linetype=2,size=1.1) + 
  geom_vline(xintercept=18900,color="red",linetype=2,size=1.1) + 
  xlab(expression(bold(paste("Date in 2021")))) +  
  ylab(expression(bold(paste("Share of Population fully vaccinated (in %)")))) + 
  scale_linetype_manual(name="Countries",values=c("dashed","solid"),labels=c("Synthetic Poland","Poland"))+
  zew_plotstyle() +
  theme(legend.position = c(0.85, 0.45),
        legend.background = element_rect(fill = "white", color = "black", size = 1))



# Let's put the two plots side-by-side.
SCM_plot



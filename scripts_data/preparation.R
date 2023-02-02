library(tidyverse)
library(readxl)
library(Synth)
library(SCtools)
library(augsynth)



library(extrafont) 

#RDD analysis
vaccine <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv")
vaccine$date <- as.Date(vaccine$date)
rdd_pol <- vaccine[grep("Poland", vaccine$location), ]
rdd_pol <- rdd_pol[, c(1:3,9) ]



# link www.fontsquirrel.com/fonts/latin-modern-roman

# execute once to add fonts:
library(showtext)
font_add(family = "Latin Modern Roman 10", regular = "lmroman10-regular-webfont.ttf")
showtext_auto()


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
vaccine$date <- as.Date(vaccine$date)
donor_countries <- "Bulgaria|Czechia|Estonia|Greece|Croatia|Latvia|Lithuania|Hungary|Austria|Poland|Romania|Slovenia|Slovakia"
donor_pool <- vaccine[grep(donor_countries, vaccine$location), ]


#Plotting vaccinations in Poland (descriptive)


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


#Remove 2020, 2022 and 2023
poland_lottery <- poland_lottery[!grepl("2023|2022|2020", poland_lottery$date), ]

#experiment:
setwd("C:\\Users\\bened\\Documents\\bachelor_thesis\\scripts_data")
source("kalman.R")

poland <- poland_lottery[grep("Poland", poland_lottery$country), ]
poland <- na_interpolation(poland)


##Plot poland (descriptive part)
source("ggplot_theme.R")
Pol_desc_plot <- ggplot(poland,aes(y=twodoses,x=date,group=1,linetype="solid")) + 
  geom_line(size=1, colour="#527ca4") + 
  xlab(expression(bold(paste("Date in 2021")))) +  
  ylab(expression(bold(paste("Share of Population fully vaccinated (in %)")))) + 
  zew_plotstyle() + theme(legend.position = "none")

#+
 # scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
  #             date_labels = "%B")

Pol_desc_plot

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


country <- c("Austria", "Czechia", "Greece", "Latvia","Slovakia","Bulgaria","Croatia",
                  "Slovenia","Romania","Lithuania","Hungary","Estonia","Poland")
entrydate <- c(1995,2004,1981,2004,2004,2007,2013,2004,2007,2004,2004,2004,2004)

eu_accession <- data.frame(country, entrydate)

mrgeacc <- list(poland_lottery, eu_accession)
poland_lottery <- mrgeacc %>% reduce(full_join, by='country')


#"trstscinc20"

dataprep.out <- dataprep(foo = poland_lottery, 
                         predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                         predictors.op = "mean",
                         dependent = "twodoses", unit.variable = "countryid",
                         time.variable = "date2", treatment.identifier = 10,
                         controls.identifier = c(1,2,3,6,12,13),
                         time.predictors.prior = c(18659:18771),
                         time.optimize.ssr = c(18659:18771), time.plot = c(18659:18961),
                         unit.names.variable = "country")

synth.out = synth(dataprep.out)

synth_data_out = data.frame(dataprep.out$Y0plot%*%synth.out$solution.w) 
date = as.numeric(row.names(synth_data_out))
plot.df = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df$synth = synth_data_out$w.weight
plot.df$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]

path.plot(dataprep.res = dataprep.out, synth.res = synth.out,Xlab="Date",Ylab="Cumulative COVID-19 Cases",Main="Comparison of Synth vs. Actual Cum. COVID-19 Cases in Jena, Germany")

# And we can add a vertical line where the treatment occurred.
abline(v=18771,lty=2,col="red")

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("gap in real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"))

gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
plotgap.df = data.frame(gap)
plotgap.df$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


ggplot(plotgap.df)
Pol_gap_plot <- ggplot(plotgap.df,aes(y=gap,x=date,group=1,linetype="solid")
)+ geom_line(size=1, colour="#527ca4") + zew_plotstyle() + ylim(-0.08, 0.08) +
  geom_hline(yintercept = 0, color = "black", size = 1) + 
  theme(axis.line.x = element_line(colour = "grey70", size = 0.2, linetype = 3)) +
  geom_vline(xintercept=18772,color="black",linetype=2,size=1) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=1)
  

Pol_gap_plot

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


year <- cont <- id <- Y1 <- synthetic.Y1 <- NULL
n<-tdf$n
t1 <- unique(tdf$df$year)[which(tdf$df$year == tdf$t1) - 1]
tr<-tdf$tr
names.and.numbers<-tdf$names.and.numbers
treated.name<-as.character(tdf$treated.name)
df.plot<-NULL

for(i in 1:n){
  a<-cbind(tdf$df$year,tdf$df[,i],tdf$df[,n+i],i)
  df.plot<-rbind(df.plot, a)
}
df.plot<-data.frame(df.plot)
df.plot$gap <- df.plot$V3-df.plot$V2
colnames(df.plot)<-c('date2','cont','tr','id','gap')
datedfplot <- rep(seq(as.Date("2021-02-01"),as.Date("2021-11-29"),1),6)
df.plot$date <- datedfplot

p.gaps <- ggplot(df.plot,mapping=aes(x=date, y=gap))+
geom_line(mapping=aes(group=id, color='2'),size=0.8)+
geom_line(plotgap.df, mapping = aes(color='1'),size=0.8)+ 
geom_hline(yintercept = 0, color = "black", size = 1) + 
geom_vline(xintercept=18772,color="black",linetype=2,size=1) + 
geom_vline(xintercept=18900,color="black",linetype=2,size=1) + 
ylim(-0.6, 0.4) +
xlab(expression(bold(paste("Date in 2021")))) +
scale_color_manual(name="Countries",
                   values = c( '1' = '#527ca4', '2' = '#b4be28'),
                   labels = c(tdf$treated.name, 'Controls')) +
zew_plotstyle() + theme(axis.title.y = element_blank(),
                        axis.line.x = element_line(colour = "grey70", size = 0.2, linetype = 3),
                        legend.position = c(0.15, 0.3),
                        legend.background = element_rect(fill = "white", color = "black", size = 1),) +
labs(caption = "The figure shows the gaps between the actual and synthetic unit 
     for Poland as well as all of the control units. The synthetic control 
     of the control units are placebos (constructed as if there was a treatment.")



p.gaps

test1 <- as.Date("2021-02-01")
test2 <- as.Date("2021-11-29")
test <- rep(seq(test1:test2,1),6)
test
date <- rep(seq(as.Date("2021-02-01"),as.Date("2021-11-29"),1),6)
date

# Let's pull out the data from the result, to make our own nicer plots in ggplot of course

synth_data_out = data.frame(dataprep.out$Y0plot%*%synth.out$solution.w) 
date = as.numeric(row.names(synth_data_out))
plot.df = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df$synth = synth_data_out$w.weight
plot.df$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


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


install.packages("ggtext",dependencies = TRUE)

SCM_plot <- ggplot(plot.df,aes(y=twodoses,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18772,color="black",linetype=2,size=1) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=1) + 
  xlab(expression(bold(paste("Date in 2021")))) +  
  ylab(expression(bold(paste("Share of Population fully vaccinated (in %)")))) + 
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  zew_plotstyle() +
  theme(legend.position = c(0.85, 0.45),
        legend.background = element_rect(fill = "white", color = "black", size = 1),
        axis.title.y = element_blank()) +
  labs(caption = "The two dashed lines refer to the announcement (25/05) and the end (30/09) of the lottery.")
SCM_plot


c(0.85, 0.45)


dataprep.out2 <- dataprep(foo = poland_lottery, 
                          predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20","entrydate"),
                          predictors.op = "mean",
                          dependent = "onedose", unit.variable = "countryid",
                          time.variable = "date2", treatment.identifier = 10,
                          controls.identifier = c(1,2,3,6,12,13),
                          time.predictors.prior = c(18659:18772),
                          time.optimize.ssr = c(18659:18772), time.plot = c(18659:18960),
                          unit.names.variable = "country")

synth.out2 = synth(dataprep.out2)


synth_data_out2 = data.frame(dataprep.out2$Y0plot%*%synth.out2$solution.w) 
date = as.numeric(row.names(synth_data_out2))
plot.df2 = data.frame(onedose=poland_lottery$onedose[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df2$synth = synth_data_out2$w.weight
plot.df2$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


SCM_plot2 <- ggplot(plot.df2,aes(y=onedose,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18772,color="black",linetype=2,size=1) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=1) + 
  xlab(expression(bold(paste("Date in 2021")))) +  
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  zew_plotstyle() +
  theme(legend.position = c(0.85, 0.45),
        legend.background = element_rect(fill = "white", color = "black", size = 1),
        axis.title.y = element_blank()) +
  labs(caption = "The two dashed lines refer to the announcement (25/05) and the end (30/09) of the lottery.")
SCM_plot2



dataprep.out.tc <- dataprep(foo = poland_lottery, 
                            predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                            predictors.op = "mean",
                            dependent = "twodoses", unit.variable = "countryid",
                            time.variable = "date2", treatment.identifier = 10,
                            controls.identifier = c(1,2,3,6,12,13),
                            time.predictors.prior = c(18659:18808),
                            time.optimize.ssr = c(18659:18808), time.plot = c(18659:18960),
                            unit.names.variable = "country")

synth.out.tc = synth(dataprep.out)

synth_data_out.tc = data.frame(dataprep.out.tc$Y0plot%*%synth.out.tc$solution.w) 
date = as.numeric(row.names(synth_data_out.tc))
plot.df.tc = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.tc$synth = synth_data_out.tc$w.weight
plot.df.tc$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


SCM_plot.tc <- ggplot(plot.df.tc,aes(y=twodoses,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18808,color="black",linetype=2,size=1) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=1) + 
  xlab(expression(bold(paste("Date in 2021")))) +  
  ylab(expression(bold(paste("Share of Population fully vaccinated (in %)")))) + 
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  zew_plotstyle() +
  theme(legend.position = c(0.85, 0.45),
        legend.background = element_rect(fill = "white", color = "black", size = 1),
        axis.title.y = element_blank(),axis.title.x = element_blank()) +
  annotate(geom="text", x=as.Date("2021-04-01"), y=0.4, label="The time of intervention is \nchanged to July 1",
           color="black", family= "Latin Modern Roman 10") +
  annotate("segment", x = as.Date("2021-05-31"), xend = as.Date("2021-06-25"), y = 0.4, 
           yend = 0.4, colour = "black", size = 0.2, arrow = arrow(angle = 25, 
           length = unit(.25,"cm")))
SCM_plot.tc

#SCM two months prior
dataprep.out.tcb <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,2,3,6,12,13),
                             time.predictors.prior = c(18659:18710),
                             time.optimize.ssr = c(18659:18710), time.plot = c(18659:18960),
                             unit.names.variable = "country")

synth.out.tcb = synth(dataprep.out.tcb)

synth_data_out.tcb = data.frame(dataprep.out.tcb$Y0plot%*%synth.out.tcb$solution.w) 
date = as.numeric(row.names(synth_data_out.tcb))
plot.df.tcb = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.tcb$synth = synth_data_out.tcb$w.weight
plot.df.tcb$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]

SCM_plot.tcb <- ggplot(plot.df.tcb,aes(y=twodoses,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18710,color="black",linetype=2,size=1) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=1) + 
  xlab(expression(bold(paste("Date in 2021")))) +  
  ylab(expression(bold(paste("Share of Population fully vaccinated (in %)")))) + 
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  zew_plotstyle() +
  theme(legend.position = c(0.85, 0.45),
        legend.background = element_rect(fill = "white", color = "black", size = 1),
        axis.title.y = element_blank(),axis.title.x = element_blank()) +
  annotate(geom="text", x=as.Date("2021-05-25"), y=0.4, label="The time of intervention is \nchanged to March 25",
           color="black", family= "Latin Modern Roman 10") +
  annotate("segment", x = as.Date("2021-04-15"), xend = as.Date("2021-03-28"), y = 0.4, 
           yend = 0.4, colour = "black", size = 0.2, arrow = arrow(angle = 25, 
                                                                   length = unit(.25,"cm")))
SCM_plot.tcb




#Leave one out analysis
#GDP
dataprep.out.gdp <- dataprep(foo = poland_lottery, 
                         predictors = c("inflzvacc19","popdensity19","tertiary20","trstscinc20","elderly20"),
                         predictors.op = "mean",
                         dependent = "twodoses", unit.variable = "countryid",
                         time.variable = "date2", treatment.identifier = 10,
                         controls.identifier = c(1,2,3,6,12,13),
                         time.predictors.prior = c(18659:18772),
                         time.optimize.ssr = c(18659:18772), time.plot = c(18659:18960),
                         unit.names.variable = "country")

synth.out.gdp = synth(dataprep.out.gdp)

synth_data_out.gdp = data.frame(dataprep.out.gdp$Y0plot%*%synth.out.gdp$solution.w) 
date = as.numeric(row.names(synth_data_out.gdp))
plot.df.gdp = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.gdp$synth = synth_data_out.gdp$w.weight
plot.df.gdp$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#influenza
dataprep.out.infz <- dataprep(foo = poland_lottery, 
                            predictors = c("gdpcapita20","popdensity19","tertiary20","trstscinc20","elderly20"),
                            predictors.op = "mean",
                            dependent = "twodoses", unit.variable = "countryid",
                            time.variable = "date2", treatment.identifier = 10,
                            controls.identifier = c(1,2,3,6,12,13),
                            time.predictors.prior = c(18659:18772),
                            time.optimize.ssr = c(18659:18772), time.plot = c(18659:18960),
                            unit.names.variable = "country")

synth.out.infz = synth(dataprep.out.infz)

synth_data_out.infz = data.frame(dataprep.out.infz$Y0plot%*%synth.out.infz$solution.w) 
date = as.numeric(row.names(synth_data_out.infz))
plot.df.infz = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.infz$synth = synth_data_out.infz$w.weight
plot.df.infz$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#popdensity
dataprep.out.pop <- dataprep(foo = poland_lottery, 
                            predictors = c("gdpcapita20","inflzvacc19","tertiary20","trstscinc20","elderly20"),
                            predictors.op = "mean",
                            dependent = "twodoses", unit.variable = "countryid",
                            time.variable = "date2", treatment.identifier = 10,
                            controls.identifier = c(1,2,3,6,12,13),
                            time.predictors.prior = c(18659:18772),
                            time.optimize.ssr = c(18659:18772), time.plot = c(18659:18960),
                            unit.names.variable = "country")

synth.out.pop = synth(dataprep.out.pop)

synth_data_out.pop = data.frame(dataprep.out.pop$Y0plot%*%synth.out.pop$solution.w) 
date = as.numeric(row.names(synth_data_out.pop))
plot.df.pop = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.pop$synth = synth_data_out.pop$w.weight
plot.df.pop$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#tertiary
dataprep.out.tert <- dataprep(foo = poland_lottery, 
                            predictors = c("gdpcapita20","inflzvacc19","popdensity19","trstscinc20","elderly20"),
                            predictors.op = "mean",
                            dependent = "twodoses", unit.variable = "countryid",
                            time.variable = "date2", treatment.identifier = 10,
                            controls.identifier = c(1,2,3,6,12,13),
                            time.predictors.prior = c(18659:18772),
                            time.optimize.ssr = c(18659:18772), time.plot = c(18659:18960),
                            unit.names.variable = "country")

synth.out.tert = synth(dataprep.out.tert)

synth_data_out.tert = data.frame(dataprep.out.tert$Y0plot%*%synth.out.tert$solution.w) 
date = as.numeric(row.names(synth_data_out.tert))
plot.df.tert = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.tert$synth = synth_data_out.tert$w.weight
plot.df.tert$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#trstscinc
dataprep.out.trst <- dataprep(foo = poland_lottery, 
                            predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20"),
                            predictors.op = "mean",
                            dependent = "twodoses", unit.variable = "countryid",
                            time.variable = "date2", treatment.identifier = 10,
                            controls.identifier = c(1,2,3,6,12,13),
                            time.predictors.prior = c(18659:18772),
                            time.optimize.ssr = c(18659:18772), time.plot = c(18659:18960),
                            unit.names.variable = "country")

synth.out.trst = synth(dataprep.out.trst)

synth_data_out.trst = data.frame(dataprep.out.trst$Y0plot%*%synth.out.trst$solution.w) 
date = as.numeric(row.names(synth_data_out.trst))
plot.df.trst = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.trst$synth = synth_data_out.trst$w.weight
plot.df.trst$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#elderly
dataprep.out.eld <- dataprep(foo = poland_lottery, 
                            predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","trstscinc20"),
                            predictors.op = "mean",
                            dependent = "twodoses", unit.variable = "countryid",
                            time.variable = "date2", treatment.identifier = 10,
                            controls.identifier = c(1,2,3,6,12,13),
                            time.predictors.prior = c(18659:18772),
                            time.optimize.ssr = c(18659:18772), time.plot = c(18659:18960),
                            unit.names.variable = "country")

synth.out.eld = synth(dataprep.out.eld)

synth_data_out.eld = data.frame(dataprep.out.eld$Y0plot%*%synth.out.eld$solution.w) 
date = as.numeric(row.names(synth_data_out.eld))
plot.df.eld = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.eld$synth = synth_data_out.eld$w.weight
plot.df.eld$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


loo.plot <- ggplot(plot.df,mapping=aes(x=date, y=twodoses,color="Poland"))+geom_line(linetype="solid",size=1) +
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=1) +
  geom_line(plot.df.gdp, mapping=aes(x=date,y=synth,color='Leave-one-out Synthetic Poland'),size=0.6)+
  geom_line(plot.df.infz, mapping=aes(x=date,y=synth,color='Leave-one-out Synthetic Poland'),size=0.6)+
  geom_line(plot.df.pop, mapping=aes(x=date,y=synth,color='Leave-one-out Synthetic Poland'),size=0.6)+
  geom_line(plot.df.tert, mapping=aes(x=date,y=synth,color='Leave-one-out Synthetic Poland'),size=0.6)+
  geom_line(plot.df.eld, mapping=aes(x=date,y=synth,color='Leave-one-out Synthetic Poland'),size=0.6)+
  geom_line(plot.df.trst, mapping=aes(x=date,y=synth,color='Leave-one-out Synthetic Poland'),size=0.6)+
  geom_vline(xintercept=18772,color="black",linetype=2,size=1) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=1) + 
  xlab(expression(bold(paste("Date in 2021")))) +
  scale_color_manual(name="Countries",
                     values = c( "Poland" = "#527ca4" , "Synthetic Poland"="#b4be28",
                                 'Leave-one-out Synthetic Poland' = 'grey')) +
  zew_plotstyle() + theme(axis.title.y = element_blank(),
                          legend.position = c(0.85, 0.3),
                          legend.background = element_rect(fill = "white", color = "black", size = 1),)

loo.plot

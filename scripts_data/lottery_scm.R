#Load necessary packages
library(tidyverse)
library(readxl)
#install.packages("Synth") --> main synthetic control analysis
library(Synth)
#install.packages("SCtools") --> synthetic control inference
library(SCtools)
#install.packages("scales") --> plots
library(scales)
#install.packages("rdrobust") --> rdd analysis
library(rdrobust)
#install.packages("imputeTS")
library(imputeTS)




#Import main dataset from Our World in Data (takes a few minutes)
#Commit from March 8, 2023
vaccine <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/bac6f96045857196fa439508492529d5b9e75d0e/public/data/vaccinations/vaccinations.csv")

#Define zew_plotstyle
zew_plotstyle <- function() {
  font <- "Latin Modern Roman 10"
  
  ggplot2::theme(
    text=element_text(family = font),
    plot.title = element_text(face = "bold", size = 12, colour = "black"),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.position = "bottom",
    legend.title = element_text(family = font, colour = "black", size = 12),
    legend.text = element_text(family = font, colour = "black", size = 12),
    legend.key = element_rect(fill = "white"),
    axis.title.y = element_text(family = font, colour = "black", size = 12),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(colour = "black", size = 0.5), 
    axis.text = element_text(family = font, colour = "black", size= 12),
    axis.line.x = element_line(colour = "black", size = 1),
    panel.grid.major.y = element_line(colour = "grey70", size = 0.2, linetype = 3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.subtitle = ggplot2::element_text(size = 12, colour = "black", margin=ggplot2::margin(3,0,9,0)),
    panel.background = element_rect(fill = "white"),
    axis.title.x = element_text(family = font, colour = "black", size = 12),
    plot.title.position = "plot"
  )
}



#RDD analysis of daily vaccinations
##Transform date into date class
vaccine$date <- as.Date(vaccine$date)

##Isolate Poland
rdd_pol <- vaccine[grep("Poland", vaccine$location), ]

##Isolate daily vaccinations
rdd_pol <- rdd_pol[, c(1:3,9) ]

##Select data subset (date)
rdd_pol <- subset(rdd_pol, date >= as.Date("2021-04-25") & date <= as.Date("2021-06-23"))

##Create additional numeric date variable
rdd_pol$date2 <- as.numeric(rdd_pol$date)

##Set x and y for RDD analysis
Y <- rdd_pol$daily_vaccinations
X <- rdd_pol$date2

##Perform RDD analysis

###Uniform Kernel
rd_uniform <- rdrobust(y=Y, x=X, c = 18772,p=4, kernel= "uniform")
####Regression results of uniform kernel 
summary(rd_uniform)

###Triangular kernel
rd_triangular <- rdrobust(y=Y, x=X, c = 18772,p=4)
####Regression results of triangular kernel
summary(rd_triangular)

###Plot RDD with uniform kernel
plot_rd <- rdplot(y=Y, x=X, c = 18772)

rdplot_mean_bin = plot_rd$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot_rd$vars_bins[,"rdplot_mean_y"]
y_hat           = plot_rd$vars_poly[,"rdplot_y"]
x_plot          = plot_rd$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot_rd$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot_rd$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot_rd$vars_bins[,"rdplot_mean_bin"]

rdd_plot <- ggplot() + zew_plotstyle() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "#527ca4", na.rm = TRUE,size=0.8) +
  geom_line(aes(x = x_plot, y = y_hat), col = "#9c2424", na.rm = TRUE, size=0.8) +
  theme(legend.position = "None", axis.line.x = element_line(colour = "black", size = 0.5)) +
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  scale_x_continuous(breaks=c(18747.98,18772,18793),
                     labels=c("2021-05-01", "2021-05-25","2021-06-15")) +
  xlab(expression(bold(paste("Date")))) + 
  ylab(expression(bold(paste("Daily vaccinations")))) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ',',
                                                   decimal.mark = '.'),
                     limits = c(200000,350000))

rdd_plot



#Synthetic control analysis

#Import additional data from Eurostat (necessary to change paths):

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


#Add data from Wellcome Global monitor (https://ourworldindata.org/grapher/share-people-trust-science?tab=table)
trst_science <- data.frame (country  = c("Bulgaria", "Czechia", "Estonia", "Greece", "Croatia", "Latvia", "Lithuania",
                                         "Hungary", "Austria", "Poland", "Romania", "Slovenia", "Slovakia"),
                            trstscinc20 = c(0.819,0.948,0.918,0.830,0.895,0.851,0.836,0.936,0.895,0.872,0.795,0.889,0.923))


#Merge with other covariates and add country ids
sndmerge <- list(predictors, trst_science)
predictors <- sndmerge %>% reduce(full_join, by='country')
predictors$countryid <- c(1:13)



#Select relevant countries from main dataset
donor_countries <- "Bulgaria|Czechia|Estonia|Greece|Croatia|Latvia|Lithuania|Hungary|Austria|Poland|Romania|Slovenia|Slovakia"
donor_pool <- vaccine[grep(donor_countries, vaccine$location), ]


#Isolating for vaccinated per hundred (removing the rest of the dataset)
poland_lottery <- donor_pool[, c(1:3,11,12) ]
names(poland_lottery)[names(poland_lottery) == "location"] <- "country"


#Merge additional variables and main vaccination data
mrgefrst <- list(poland_lottery, predictors)
poland_lottery <- mrgefrst %>% reduce(full_join, by='country')


#Transform percentage values into decimal values and name variables properly
poland_lottery <- as.data.frame(poland_lottery)
poland_lottery$people_vaccinated_per_hundred <- as.numeric(sub("%", "",poland_lottery$people_vaccinated_per_hundred,fixed=TRUE))/100
names(poland_lottery)[names(poland_lottery) == "people_vaccinated_per_hundred"] <- "onedose"

poland_lottery$people_fully_vaccinated_per_hundred <- as.numeric(sub("%", "",poland_lottery$people_fully_vaccinated_per_hundred,fixed=TRUE))/100
names(poland_lottery)[names(poland_lottery) == "people_fully_vaccinated_per_hundred"] <- "twodoses"


#Impute missing values (TSimpute package)

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


#Select time window
poland_lottery <- subset(poland_lottery, date >= as.Date("2021-02-01") & date <= as.Date("2021-09-30"))


#Transform variables to numerical variables and add numeric date for synth package
poland_lottery$date <- as.Date(poland_lottery$date)
poland_lottery$date2 <- as.numeric(poland_lottery$date)
poland_lottery$gdpcapita20 <- as.numeric(poland_lottery$gdpcapita20)
poland_lottery$popdensity19 <- as.numeric(poland_lottery$popdensity19)

#Remove Austria


#Preparation done
#Synthetic control analysis-Standard, share of the fully vaccinated (intervention date: 25/05/2021)
dataprep.out <- dataprep(foo = poland_lottery, 
                         predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                         predictors.op = "mean",
                         dependent = "twodoses", unit.variable = "countryid",
                         time.variable = "date2", treatment.identifier = 10,
                         controls.identifier = c(1,2,3,6,7,12),
                         time.predictors.prior = c(18659:18771),
                         time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                         unit.names.variable = "country")

synth.out = synth(dataprep.out)

##Inspect weights and predictors
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)


##Plotting the results
synth_data_out = data.frame(dataprep.out$Y0plot%*%synth.out$solution.w) 
date = as.numeric(row.names(synth_data_out))
plot.df = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df$synth = synth_data_out$w.weight
plot.df$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]

###Standard SCM plot
SCM_plot <- ggplot(plot.df,aes(y=twodoses,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  xlab(expression(bold(paste("Date")))) +  
  ylab(expression(bold(paste("Share fully vaccinated")))) + 
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  zew_plotstyle() +
  theme(legend.position = c(0.8, 0.3),
        axis.line.x = element_line(colour = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5)) +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d") +
  annotate(geom="text", x=as.Date("2021-03-15"), y=0.4, label="Lottery was announced \non May 25",
           color="black", family= "Latin Modern Roman 10") +
  annotate("segment", x = as.Date("2021-04-30"), xend = as.Date("2021-05-20"), y = 0.4, 
           yend = 0.4, colour = "black", size = 0.2, arrow = arrow(angle = 25, 
                                                                   length = unit(.25,"cm")))
SCM_plot


###Plotting the gap/estimated difference
gap <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
plotgap.df = data.frame(gap)
plotgap.df$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


Pol_gap_plot <- ggplot(plotgap.df,aes(y=gap,x=date,group=1,linetype="solid")
)+ geom_line(size=0.8, colour="#9c2424") + zew_plotstyle() + ylim(-0.08, 0.08) +
  xlab(expression(bold(paste("Date")))) + 
  ylab(expression(bold(paste("Estimated Difference")))) +  
  theme(axis.line.x = element_line(colour = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")
Pol_gap_plot

##Inference: Generating placebos
tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 2)

###If there are any problems with the plot, here's the simple version of plotting the placebos:
#plot_placebos(
 # tdf = tdf,
 # discard.extreme = FALSE,
 # mspe.limit = 20,
 # alpha.placebos = 1)


##Inference: Plotting the placebos (SCtools package)
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
datedfplot <- rep(seq(as.Date("2021-02-01"),as.Date("2021-09-30"),1),6)
df.plot$date <- datedfplot


p.gaps <- ggplot(df.plot,mapping=aes(x=date, y=gap))+
  geom_line(mapping=aes(group=id, color='2'),size=0.8)+
  geom_line(plotgap.df, mapping = aes(color='1'),size=0.8)+ 
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  ylim(-0.3, 0.3) +
  xlab(expression(bold(paste("Date")))) +
  ylab(expression(bold(paste("Estimated Difference")))) +
  scale_color_manual(name="Countries",
                     values = c( '1' = '#9c2424', '2' = '#c7a92f'),
                     labels = c(tdf$treated.name, 'Donor pool')) +
  zew_plotstyle() + theme(axis.line.x = element_line(colour = "black", size = 0.5),
                          legend.position = c(0.15, 0.23),
                          legend.background = element_rect(fill = "white", color = "black", size = 0.5),) + scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")


p.gaps


##Inference: Test and resulting p-value(SCtools package)
ratio <- mspe.test(tdf)
ratio$p.val




##Main analysis done!

#Synthetic control analysis-Standard, share with at least one dose


dataprep.out2 <- dataprep(foo = poland_lottery, 
                          predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                          predictors.op = "mean",
                          dependent = "onedose", unit.variable = "countryid",
                          time.variable = "date2", treatment.identifier = 10,
                          controls.identifier = c(1,2,3,6,7,12),
                          time.predictors.prior = c(18659:18771),
                          time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                          unit.names.variable = "country")

synth.out2 = synth(dataprep.out2)


##Inspect weights and predictors
synth.tables2 <- synth.tab(
  dataprep.res = dataprep.out2,
  synth.res = synth.out2)
print(synth.tables2)

##Plotting the results
synth_data_out2 = data.frame(dataprep.out2$Y0plot%*%synth.out2$solution.w) 
date = as.numeric(row.names(synth_data_out2))
plot.df2 = data.frame(onedose=poland_lottery$onedose[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df2$synth = synth_data_out2$w.weight
plot.df2$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


###Standard SCM Plot
SCM_plot2 <- ggplot(plot.df2,aes(y=onedose,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  xlab(expression(bold(paste("Date")))) +  
  ylab(expression(bold(paste("Share at least one dose")))) + 
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  zew_plotstyle() +
  theme(legend.position = c(0.8, 0.3),
        axis.line.x = element_line(colour = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5)) +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")
SCM_plot2


###Plotting the gap/estimated difference
gap2 <- dataprep.out2$Y1plot - (dataprep.out2$Y0plot %*% synth.out2$solution.w)
plotgap.df2 = data.frame(gap2)
plotgap.df2$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


Pol_gap_plot2 <- ggplot(plotgap.df2,aes(y=gap2,x=date,group=1,linetype="solid")
)+ geom_line(size=0.8, colour="#9c2424") + zew_plotstyle() + ylim(-0.08, 0.08) +
  xlab(expression(bold(paste("Date")))) + 
  ylab(expression(bold(paste("Estimated Difference")))) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  theme(axis.line.x = element_line(colour = "black", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")
Pol_gap_plot2

##Inference: Generating placebos
tdf2 <- generate.placebos(dataprep.out2,synth.out2, Sigf.ipop = 2)



##Inference: Plotting the placebos (SCtools package)
year <- cont <- id <- Y1 <- synthetic.Y1 <- NULL
n<-tdf2$n
t1 <- unique(tdf2$df$year)[which(tdf2$df$year == tdf2$t1) - 1]
tr<-tdf2$tr
names.and.numbers<-tdf2$names.and.numbers
treated.name<-as.character(tdf2$treated.name)
df.plot2<-NULL

for(i in 1:n){
  a<-cbind(tdf2$df$year,tdf2$df[,i],tdf2$df[,n+i],i)
  df.plot2<-rbind(df.plot2, a)
}
df.plot2<-data.frame(df.plot2)
df.plot2$gap2 <- df.plot2$V3-df.plot2$V2
colnames(df.plot2)<-c('date2','cont','tr','id','gap2')
datedfplot2 <- rep(seq(as.Date("2021-02-01"),as.Date("2021-09-30"),1),6)
df.plot2$date <- datedfplot2

p.gaps2 <- ggplot(df.plot2,mapping=aes(x=date, y=gap2))+
  geom_line(mapping=aes(group=id, color='2'),size=0.8)+
  geom_line(plotgap.df2, mapping = aes(color='1'),size=0.8)+ 
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) + 
  ylim(-0.3, 0.3) +
  xlab(expression(bold(paste("Date")))) +
  ylab(expression(bold(paste("Estimated Difference")))) +
  scale_color_manual(name="Countries",
                     values = c( '1' = '#9c2424', '2' = '#c7a92f'),
                     labels = c(tdf2$treated.name, 'Donor pool')) +
  zew_plotstyle() + theme(axis.line.x = element_line(colour = "black", size = 0.5),
                          legend.position = c(0.15, 0.23),
                          legend.background = element_rect(fill = "white", color = "black", size = 0.5),) + scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")


p.gaps2


###If there are any problems with the plot, here's the simple version of plotting the placebos:
#plot_placebos(
# tdf = tdf2,
# discard.extreme = FALSE,
# mspe.limit = 20,
# alpha.placebos = 1)

##Inference: Test and resulting p-value(SCtools package)
ratio2 <- mspe.test(tdf2)
ratio2$p.val


#Analysis done!


#Next: Robustness checks (share of the fully vaccinated)

#First: Changing the time of intervention to July 1, 2021

#Synthetic control analysis-Robustness check, share of the fully vaccinated (intervention date: 01/07/2021)
dataprep.out.tc <- dataprep(foo = poland_lottery, 
                            predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                            predictors.op = "mean",
                            dependent = "twodoses", unit.variable = "countryid",
                            time.variable = "date2", treatment.identifier = 10,
                            controls.identifier = c(1,2,3,6,7,12),
                            time.predictors.prior = c(18659:18808),
                            time.optimize.ssr = c(18659:18808), time.plot = c(18659:18900),
                            unit.names.variable = "country")

synth.out.tc = synth(dataprep.out.tc)


##Plotting the results
synth_data_out.tc = data.frame(dataprep.out.tc$Y0plot%*%synth.out.tc$solution.w) 
date = as.numeric(row.names(synth_data_out.tc))
plot.df.tc = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.tc$synth = synth_data_out.tc$w.weight
plot.df.tc$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]

###Standard SCM plot
SCM_plot.tc <- ggplot(plot.df.tc,aes(y=twodoses,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18809,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  xlab(expression(bold(paste("Date")))) +  
  ylab(expression(bold(paste("Share fully vaccinated")))) + 
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  zew_plotstyle() +
  ylim(0, 0.6) +
  theme(legend.position = c(0.8, 0.3),
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "white", color = "black", size = 0.5),
        axis.line.x = element_line(colour = "black", size = 0.5)) +
  annotate(geom="text", x=as.Date("2021-04-01"), y=0.4, label="The time of intervention is \nchanged to July 1",
           color="black", family= "Latin Modern Roman 10") +
  annotate("segment", x = as.Date("2021-05-31"), xend = as.Date("2021-06-25"), y = 0.4, 
           yend = 0.4, colour = "black", size = 0.2, arrow = arrow(angle = 25, 
                                                                   length = unit(.25,"cm"))) +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")
SCM_plot.tc

##Inference: Generating placebos
tdf.tc <- generate.placebos(dataprep.out.tc,synth.out.tc, Sigf.ipop = 2)

##Inference: Test and resulting p-value(SCtools package)
ratio.tc <- mspe.test(tdf.tc)
ratio.tc$p.val


#Analysis done!

#Next: Changing the time of intervention to April 25, 2021

#Synthetic control analysis-Robustness check, share of the fully vaccinated (intervention date: 25/04/2021)
dataprep.out.tcb <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,2,3,6,7,12),
                             time.predictors.prior = c(18659:18741),
                             time.optimize.ssr = c(18659:18741), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.tcb = synth(dataprep.out.tcb)


##Plotting the results
synth_data_out.tcb = data.frame(dataprep.out.tcb$Y0plot%*%synth.out.tcb$solution.w) 
date = as.numeric(row.names(synth_data_out.tcb))
plot.df.tcb = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.tcb$synth = synth_data_out.tcb$w.weight
plot.df.tcb$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


###Standard SCM plot
SCM_plot.tcb <- ggplot(plot.df.tcb,aes(y=twodoses,x=date,color="Poland")) + geom_line(linetype="solid",size=0.8) + 
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  geom_vline(xintercept=18741,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  xlab(expression(bold(paste("Date")))) +  
  ylab(expression(bold(paste("Share fully vaccinated")))) + 
  scale_color_manual(name="Countries",values=c("Poland"="#527ca4","Synthetic Poland"="#b4be28")) +
  ylim(0, 0.6) +
  zew_plotstyle() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        legend.background = element_rect(fill = "white", color = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 0.5)) +
  annotate(geom="text", x=as.Date("2021-08-01"), y=0.1, label="The time of intervention is \nchanged to April 25",
           color="black", family= "Latin Modern Roman 10") +
  annotate("segment", x = as.Date("2021-05-31"), xend = as.Date("2021-04-29"), y = 0.1, 
           yend = 0.1, colour = "black", size = 0.2, arrow = arrow(angle = 25, 
                                                                   length = unit(.25,"cm"))) +
  scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")
SCM_plot.tcb

##Inference: Generating placebos
tdf.tcb <- generate.placebos(dataprep.out.tcb,synth.out.tcb, Sigf.ipop = 2)

##Inference: Test and resulting p-value(SCtools package)
ratio.tcb <- mspe.test(tdf.tcb)
ratio.tcb$p.val


#Analysis done


#Next: Leave-one-out Analysis

#First: Leave-one-out with predictors


#Synthetic control analyses-Robustness check, share of the fully vaccinated, leaving out each predictor once
#Will probably take a few seconds/minutes
#Preparation for plotting is also carried out

#Leaving out GDP
dataprep.out.gdp <- dataprep(foo = poland_lottery, 
                             predictors = c("inflzvacc19","popdensity19","tertiary20","trstscinc20","elderly20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,2,3,6,7,12),
                             time.predictors.prior = c(18659:18771),
                             time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.gdp = synth(dataprep.out.gdp)

synth_data_out.gdp = data.frame(dataprep.out.gdp$Y0plot%*%synth.out.gdp$solution.w) 
date = as.numeric(row.names(synth_data_out.gdp))
plot.df.gdp = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.gdp$synth = synth_data_out.gdp$w.weight
plot.df.gdp$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#Leaving out influenza
dataprep.out.infz <- dataprep(foo = poland_lottery, 
                              predictors = c("gdpcapita20","popdensity19","tertiary20","trstscinc20","elderly20"),
                              predictors.op = "mean",
                              dependent = "twodoses", unit.variable = "countryid",
                              time.variable = "date2", treatment.identifier = 10,
                              controls.identifier = c(1,2,3,6,7,12),
                              time.predictors.prior = c(18659:18771),
                              time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                              unit.names.variable = "country")

synth.out.infz = synth(dataprep.out.infz)

synth_data_out.infz = data.frame(dataprep.out.infz$Y0plot%*%synth.out.infz$solution.w) 
date = as.numeric(row.names(synth_data_out.infz))
plot.df.infz = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.infz$synth = synth_data_out.infz$w.weight
plot.df.infz$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#Leaving out popdensity
dataprep.out.pop <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","tertiary20","trstscinc20","elderly20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,2,3,6,7,12),
                             time.predictors.prior = c(18659:18771),
                             time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.pop = synth(dataprep.out.pop)

synth_data_out.pop = data.frame(dataprep.out.pop$Y0plot%*%synth.out.pop$solution.w) 
date = as.numeric(row.names(synth_data_out.pop))
plot.df.pop = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.pop$synth = synth_data_out.pop$w.weight
plot.df.pop$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#Leaving out tertiary
dataprep.out.tert <- dataprep(foo = poland_lottery, 
                              predictors = c("gdpcapita20","inflzvacc19","popdensity19","trstscinc20","elderly20"),
                              predictors.op = "mean",
                              dependent = "twodoses", unit.variable = "countryid",
                              time.variable = "date2", treatment.identifier = 10,
                              controls.identifier = c(1,2,3,6,7,12),
                              time.predictors.prior = c(18659:18771),
                              time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                              unit.names.variable = "country")

synth.out.tert = synth(dataprep.out.tert)

synth_data_out.tert = data.frame(dataprep.out.tert$Y0plot%*%synth.out.tert$solution.w) 
date = as.numeric(row.names(synth_data_out.tert))
plot.df.tert = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.tert$synth = synth_data_out.tert$w.weight
plot.df.tert$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#Leaving out trstscinc
dataprep.out.trst <- dataprep(foo = poland_lottery, 
                              predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20"),
                              predictors.op = "mean",
                              dependent = "twodoses", unit.variable = "countryid",
                              time.variable = "date2", treatment.identifier = 10,
                              controls.identifier = c(1,2,3,6,7,12),
                              time.predictors.prior = c(18659:18771),
                              time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                              unit.names.variable = "country")

synth.out.trst = synth(dataprep.out.trst)

synth_data_out.trst = data.frame(dataprep.out.trst$Y0plot%*%synth.out.trst$solution.w) 
date = as.numeric(row.names(synth_data_out.trst))
plot.df.trst = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.trst$synth = synth_data_out.trst$w.weight
plot.df.trst$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


#Leaving out elderly
dataprep.out.eld <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","trstscinc20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,2,3,6,7,12),
                             time.predictors.prior = c(18659:18771),
                             time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.eld = synth(dataprep.out.eld)

synth_data_out.eld = data.frame(dataprep.out.eld$Y0plot%*%synth.out.eld$solution.w) 
date = as.numeric(row.names(synth_data_out.eld))
plot.df.eld = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.eld$synth = synth_data_out.eld$w.weight
plot.df.eld$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]

##Plotting the results
###Standard SCM plot
loo.plot <- ggplot(plot.df,mapping=aes(x=date, y=twodoses,color="Poland"))+geom_line(linetype="solid",size=0.8) +
  geom_line(plot.df.gdp, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.infz, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.pop, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.tert, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.eld, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.trst, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=0.8) +
  ylim(0, 0.6) +
  xlab(expression(bold(paste("Date")))) +
  ylab(expression(bold(paste("Share fully vaccinated")))) + 
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  scale_color_manual(name="Countries",
                     values = c( "Poland" = "#527ca4" , "Synthetic Poland"="#b4be28",
                                 "Leave-one-out" = 'grey')) +
  zew_plotstyle() + theme(axis.line.x = element_line(colour = "black", size = 0.5),
                          plot.title = element_text(hjust = 0.5),
                          legend.position = c(0.2, 0.6),
                          legend.background = element_rect(fill = "white", color = "black", size = 0.5)) + scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")

loo.plot


#Analysis done!



#Lastly: Leave-one out with countries
#Synthetic control analyses-Robustness check, share of the fully vaccinated, leaving out each country once
#Will probably take a few seconds/minutes
#Preparation for plotting is also carried out


dataprep.out.co1 <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(2,3,6,7,12),
                             time.predictors.prior = c(18659:18771),
                             time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.co1 = synth(dataprep.out.co1)

synth_data_out.co1 = data.frame(dataprep.out.co1$Y0plot%*%synth.out.co1$solution.w) 
date = as.numeric(row.names(synth_data_out.co1))
plot.df.co1 = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.co1$synth = synth_data_out.co1$w.weight
plot.df.co1$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]



dataprep.out.co2 <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,3,6,7,12),
                             time.predictors.prior = c(18659:18771),
                             time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.co2 = synth(dataprep.out.co2)


synth_data_out.co2 = data.frame(dataprep.out.co2$Y0plot%*%synth.out.co2$solution.w) 
date = as.numeric(row.names(synth_data_out.co2))
plot.df.co2 = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.co2$synth = synth_data_out.co2$w.weight
plot.df.co2$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


dataprep.out.co3 <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,2,6,7,12),
                             time.predictors.prior = c(18659:18771),
                             time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.co3 = synth(dataprep.out.co3)

synth_data_out.co3 = data.frame(dataprep.out.co3$Y0plot%*%synth.out.co3$solution.w) 
date = as.numeric(row.names(synth_data_out.co3))
plot.df.co3 = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.co3$synth = synth_data_out.co3$w.weight
plot.df.co3$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]



dataprep.out.co6 <- dataprep(foo = poland_lottery, 
                             predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                             predictors.op = "mean",
                             dependent = "twodoses", unit.variable = "countryid",
                             time.variable = "date2", treatment.identifier = 10,
                             controls.identifier = c(1,2,3,7,12),
                             time.predictors.prior = c(18659:18771),
                             time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                             unit.names.variable = "country")

synth.out.co6 = synth(dataprep.out.co6)


synth_data_out.co6 = data.frame(dataprep.out.co6$Y0plot%*%synth.out.co6$solution.w) 
date = as.numeric(row.names(synth_data_out.co6))
plot.df.co6 = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.co6$synth = synth_data_out.co6$w.weight
plot.df.co6$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]



dataprep.out.co7 <- dataprep(foo = poland_lottery, 
                              predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                              predictors.op = "mean",
                              dependent = "twodoses", unit.variable = "countryid",
                              time.variable = "date2", treatment.identifier = 10,
                              controls.identifier = c(1,2,3,6,12),
                              time.predictors.prior = c(18659:18771),
                              time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                              unit.names.variable = "country")

synth.out.co7 = synth(dataprep.out.co7)

synth_data_out.co7 = data.frame(dataprep.out.co7$Y0plot%*%synth.out.co7$solution.w) 
date = as.numeric(row.names(synth_data_out.co7))
plot.df.co7 = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.co7$synth = synth_data_out.co7$w.weight
plot.df.co7$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]


dataprep.out.co12 <- dataprep(foo = poland_lottery, 
                              predictors = c("gdpcapita20","inflzvacc19","popdensity19","tertiary20","elderly20","trstscinc20"),
                              predictors.op = "mean",
                              dependent = "twodoses", unit.variable = "countryid",
                              time.variable = "date2", treatment.identifier = 10,
                              controls.identifier = c(1,2,3,6,7),
                              time.predictors.prior = c(18659:18771),
                              time.optimize.ssr = c(18659:18771), time.plot = c(18659:18900),
                              unit.names.variable = "country")

synth.out.co12 = synth(dataprep.out.co12)


synth_data_out.co12 = data.frame(dataprep.out.co12$Y0plot%*%synth.out.co12$solution.w) 
date = as.numeric(row.names(synth_data_out.co12))
plot.df.co12 = data.frame(twodoses=poland_lottery$twodoses[poland_lottery$countryid==10 & poland_lottery$date2 %in% date])
plot.df.co12$synth = synth_data_out.co12$w.weight
plot.df.co12$date <- poland_lottery$date[poland_lottery$countryid==10 & poland_lottery$date2 %in% date]



##Plotting the results
###Standard SCM plot
looc.plot <- ggplot(plot.df,mapping=aes(x=date, y=twodoses,color="Poland"))+geom_line(linetype="solid",size=1) +
  geom_line(plot.df.co1, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.co2, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.co3, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.co6, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.co7, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(plot.df.co12, mapping=aes(x=date,y=synth,color='Leave-one-out'),size=0.6)+
  geom_line(aes(y=synth,x=date,color="Synthetic Poland"),linetype="solid",size=1) +
  geom_vline(xintercept=18772,color="black",linetype=2,size=0.5) + 
  geom_vline(xintercept=18900,color="black",linetype=2,size=0.5) + 
  xlab(expression(bold(paste("Date")))) +
  ylab(expression(bold(paste("Share fully vaccinated")))) + 
  ylim(0, 0.6) +
  scale_color_manual(name="Countries",
                     values = c( "Poland" = "#527ca4" , "Synthetic Poland"="#b4be28",
                                 'Leave-one-out' = 'grey')) +
  zew_plotstyle() + theme(plot.title = element_text(hjust = 0.5),
                          axis.line.x = element_line(colour = "black", size = 0.5),
                          legend.position = "none",
                          legend.background = element_rect(fill = "white", color = "black", size = 1)) + scale_x_date(date_breaks = "2 months", date_minor_breaks = "1 month", date_labels = "%Y-%m-%d")

looc.plot


#Everything done!
setwd("/path/to/working/directory/")

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(cowplot)
library(zoo)
library(olsrr)
library(utils)
library(nnet)

#library(directlabels)
#library(scales)
#library(plyr)
#library(ggpubr)

# Loading Rdata file containing saved script environment
load ("Manuscript1.Rdata")

hdata_CDC <- read.csv("https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD", header = T)

hdata_CDC$date <- format(as.Date(hdata_CDC$date, format = "%Y/%m/%d"), "%Y-%m-%d")

hdata_CDC <- replace(hdata_CDC, hdata_CDC<0,0)
as.numeric(hdata_CDC[is.na(hdata_CDC)]<-0)

hdata_covidproject <- read.csv("https://covidtracking.com/data/download/all-states-history.csv", header=TRUE)
hdata_covidproject <- replace(hdata_covidproject, hdata_covidproject<0,0)
as.numeric(hdata_covidproject[is.na(hdata_covidproject)]<-0)
hdata_covidproject <- subset(hdata_covidproject, as.Date(hdata_covidproject$date, format = "%Y-%m-%d") < "2020-3-28")

census <- read.csv(file='CENSUS.csv',header=T)
censusage <- read.csv(file='CensusAge.csv',header=T)
rownames(censusage) <- censusage$Age
censusage <- censusage[,-1]
censusage <- data.frame(t(censusage))

STATES <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
            "Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
            "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
            "Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
            "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
            "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
            "Virginia","Washington","West Virginia","Wisconsin","Wyoming")

hdata_total <- hdata_CDC$total_adult_patients_hospitalized_confirmed_covid + 
  hdata_CDC$total_pediatric_patients_hospitalized_confirmed_covid

hdata1 <- data.frame(cbind(hdata_CDC, hdata_total))
as.numeric(hdata1[is.na(hdata1)] <- 0)
hdata2 <- subset(hdata1, as.Date(hdata1$date, format = "%Y-%m-%d") > "2020-3-27")

hdata_date_covproj <- data.frame(with(hdata_covidproject,tapply(hdata_covidproject$hospitalizedIncrease,hdata_covidproject$date,sum)))
hdata_date_covproj <- cbind(rownames(hdata_date_covproj),hdata_date_covproj)
rownames(hdata_date_covproj) <- NULL
colnames(hdata_date_covproj) <- c('Date','Daily Hospital Admissions')

hdata_date <- data.frame(with(hdata2,tapply(hdata2$hdata_total,hdata2$date,sum)))
hdata_date <- cbind(rownames(hdata_date),hdata_date)
rownames(hdata_date) <- NULL
colnames(hdata_date) <- c('Date','Daily Hospital Admissions')
hdata_date <- rbind(hdata_date_covproj,hdata_date)

hdata_state_covproj <- data.frame(with(hdata_covidproject,tapply(hdata_covidproject$hospitalizedIncrease,
                                                                 hdata_covidproject$state,sum)))
hdata_state_covproj <- cbind(rownames(hdata_state_covproj),hdata_state_covproj)
rownames(hdata_state_covproj) <- NULL
colnames(hdata_state_covproj) <- c('State','Daily Hospital Admissions')

remove_h_covproj = c("AS", "DC", "GU", "MP", "PR", "VI") 
rows_to_remove_h_covproj <- which(hdata_state_covproj$State %in% remove_h_covproj)
hdata_state_covproj <- hdata_state_covproj[-rows_to_remove_h_covproj,]


hdata_state <- data.frame(with(hdata2,tapply(hdata2$total_adult_patients_hospitalized_confirmed_covid,
                                             hdata2$state,sum)))
hdata_state <- cbind(rownames(hdata_state),
                     hdata_state)
rownames(hdata_state) <- NULL
colnames(hdata_state) <- c('State','Daily Hospital Admissions')

remove_h = c("AS","BP2", "DC","DD2", "FM", "GU","IH2", "LTC", "MH", "MP", "PR", "RP", "US", "VA2", "VI") 
rows_to_remove_h <- which(hdata_state$State %in% remove_h)
hdata_state <- hdata_state[-rows_to_remove_h,]

hdata_state <- cbind(hdata_state$State,hdata_state$`Daily Hospital Admissions`+
                       hdata_state_covproj$`Daily Hospital Admissions`)
colnames(hdata_state) <- c('State','Daily Hospital Admissions')

state_name <- c("Alaska","Alabama","Arkansas","Arizona","California","Colorado","Connecticut",
                "Delaware","Florida","Georgia","Hawaii","Iowa","Idaho","Illinois","Indiana",
                "Kansas","Kentucky","Louisiana","Massachusetts","Maryland","Maine","Michigan",
                "Minnesota","Missouri","Mississippi","Montana","North Carolina","North Dakota",
                "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York",
                "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                "South Carolina","South Dakota","Tennessee","Texas","Utah",
                "Virginia","Vermont","Washington","Wisconsin","West Virginia","Wyoming")

hdata_state <- data.frame(cbind(state_name,hdata_state))
hdata_state <- hdata_state[order(hdata_state$state_name),]
hdata_state <- cbind(hdata_state,(as.numeric(hdata_state$Daily.Hospital.Admissions)/censusage$Total)*1000)

colnames(hdata_state) <- c('state_name','State','Daily Hospitalization Count','Daily Hospitalization Rate')
col = hcl(c(15, 15+180), 100, 65)



####### VACCINE DATA ########
vaccine_dt <- read.csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD",fileEncoding = 'UTF-8-BOM')

vaccine_dt$Date <- format(as.Date(vaccine_dt$Date, format = "%m/%d/%Y"), "%Y-%m-%d")

remove4 = c("AS","BP2", "DC","DD2", "FM", "GU","IH2", "LTC", "MH", "MP", "PR", "RP", "US", "VA2", "VI","PW") 
rows_to_remove4 <- which(vaccine_dt$Location %in% remove4)
vaccine <- vaccine_dt[-rows_to_remove4,]

state_name <- c("Alaska","Alabama","Arkansas","Arizona","California","Colorado","Connecticut",
                "Delaware","Florida","Georgia","Hawaii","Iowa","Idaho","Illinois","Indiana",
                "Kansas","Kentucky","Louisiana","Massachusetts","Maryland","Maine","Michigan",
                "Minnesota","Missouri","Mississippi","Montana","North Carolina","North Dakota",
                "Nebraska","New Hampshire","New Jersey","New Mexico","Nevada","New York",
                "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                "South Carolina","South Dakota","Tennessee","Texas","Utah",
                "Virginia","Vermont","Washington","Wisconsin","West Virginia","Wyoming")


# two conditions sum test
vaccine1 <- data.frame(with(vaccine, tapply(vaccine$Series_Complete_Yes,list(vaccine$Date,vaccine$Location), sum)))
vaccine1 <- ceiling(vaccine1)

# transpose rows columns of vaccine1 table
vaccine2 <- data.frame(t(vaccine1))
names(vaccine2) <- sub("^X", "", names(vaccine2))
vaccine_date<- data.frame(with(vaccine, tapply(Administered, Date, sum)))
vaccine_date <- cbind(rownames(vaccine_date),vaccine_date)
rownames(vaccine_date) <- NULL
colnames(vaccine_date) <- c('Date','Cumulative Vaccine Count')


vaccine2 <- cbind(state_name,vaccine2)

last_data_vax <- vaccine2[ , ncol(vaccine2), drop = FALSE]
vaccine_state <- cbind(vaccine2$state_name,last_data_vax)

colnames(vaccine_state) <- c("State","Cumulative Vaccine Administration")

censusstate <- read.csv("CensusState.csv",header=T)

# dplyr ordering alphabetically

vaccine_state <- vaccine_state[order(vaccine_state$State),]
cumulative_vaxrate <- (vaccine_state$`Cumulative Vaccine Administration`/censusage$X10.years.and.over )*1000
vaccine_state <- cbind(vaccine_state,cumulative_vaxrate)

scalevstate <- max((vaccine_state$`Cumulative Vaccine Administration`/censusage$X10.years.and.over)*1000) /
  max(hdata_state$`Daily Hospitalization Rate`)





#############Distribution of disease based on vaccine type ############

########PFIZER 
vaccine_pfizer <- data.frame(with(vaccine, tapply(Series_Complete_Pfizer, list(Date,Location), sum)))
vaccine_pfizer <- ceiling(vaccine_pfizer)
vaccine_pfizer2 <- data.frame(t(vaccine_pfizer))
names(vaccine_pfizer2) <- sub("^X", "", names(vaccine_pfizer2))
vaccine_pfizer2 <- cbind(state_name,vaccine_pfizer2)

########MODERNA 
vaccine_moderna <- data.frame(with(vaccine, tapply(Series_Complete_Moderna, list(Date,Location), sum)))
vaccine_moderna <- ceiling(vaccine_moderna)
vaccine_moderna2 <- data.frame(t(vaccine_moderna))
names(vaccine_moderna2) <- sub("^X", "", names(vaccine_moderna2))
vaccine_moderna2 <- cbind(state_name,vaccine_moderna2)

########JANSSEN 
vaccine_janssen <- data.frame(with(vaccine, tapply(Series_Complete_Janssen, list(Date,Location), sum)))
vaccine_janssen <- ceiling(vaccine_janssen)
vaccine_janssen2 <- data.frame(t(vaccine_janssen))
names(vaccine_janssen2) <- sub("^X", "", names(vaccine_janssen2))
vaccine_janssen2 <- cbind(state_name,vaccine_janssen2)

########NOVAVAX 
vaccine_novavax <- data.frame(with(vaccine, tapply(Series_Complete_Novavax, list(Date,Location), sum)))
vaccine_novavax <- ceiling(vaccine_novavax)
vaccine_novavax2 <- data.frame(t(vaccine_novavax))
names(vaccine_novavax2) <- sub("^X", "", names(vaccine_novavax2))
vaccine_novavax2 <- cbind(state_name,vaccine_novavax2)


last_data_pfizer <- vaccine_pfizer2[ , ncol(vaccine_pfizer2), drop = FALSE]
last_data_moderna <- vaccine_moderna2[ , ncol(vaccine_moderna2), drop = FALSE]
last_data_janssen <- vaccine_janssen2[ , ncol(vaccine_janssen2), drop = FALSE]
last_data_novavax <- vaccine_novavax2[ , ncol(vaccine_novavax2), drop = FALSE]


vaccine_type_state <- cbind(vaccine_pfizer2$state_name,last_data_pfizer,last_data_moderna,last_data_janssen,last_data_novavax)
colnames(vaccine_type_state) <- c("State","Cumulative Pfizer","Cumulative Moderna","Cumulative Janssen","Cumulative Novavax")                            
vaccine_type_state <- vaccine_type_state[order(vaccine_type_state$State),]




########Combined graph for all 3 vax and cumulative ########

linetype1 <- c("Cumulative"="solid","Pfizer"="dashed","Moderna"="dotted","Janssen"="dotdash","Novavax"="longdash")
ggplot() + 
  geom_line(aes(x=reorder(vaccine_state$State, (vaccine_state$`Cumulative Vaccine Administration`/
                                                  censusage$X10.years.and.over)*1000, FUN=mean),
                y=(vaccine_state$`Cumulative Vaccine Administration`/
                     censusage$X10.years.and.over)*1000,linetype="Cumulative"), color=col[2], group=1,size=0.9) +
  geom_line(aes(x=reorder(vaccine_type_state$State, 
                          (vaccine_state$`Cumulative Vaccine Administration`/
                             censusage$X10.years.and.over)*1000, FUN=mean),
                y=(vaccine_type_state$`Cumulative Pfizer`/
                     censusage$X10.years.and.over)*1000,linetype="Pfizer"), color=col[2], group=3,size=0.9) + 
  geom_line(aes(x=reorder(vaccine_type_state$State, 
                          (vaccine_state$`Cumulative Vaccine Administration`/
                             censusage$X10.years.and.over)*1000, FUN=mean),
                y=(vaccine_type_state$`Cumulative Moderna`/
                     censusage$X10.years.and.over)*1000,linetype="Moderna"), color=col[2],group=4,size=0.9) +
  geom_line(aes(x=reorder(vaccine_type_state$State, 
                          (vaccine_state$`Cumulative Vaccine Administration`/
                             censusage$X10.years.and.over)*1000, FUN=mean),
                y=(vaccine_type_state$`Cumulative Janssen`/
                     censusage$X10.years.and.over)*1000,linetype="Janssen"), color=col[2],group=5,size=0.9) +
  geom_line(aes(x=reorder(vaccine_type_state$State, 
                          (vaccine_state$`Cumulative Vaccine Administration`/
                             censusage$X10.years.and.over)*1000, FUN=mean),
                y=(vaccine_type_state$`Cumulative Novavax`/
                     censusage$X10.years.and.over)*1000,linetype="Novavax"), color=col[2],group=6,size=0.9) +
  geom_line(aes(x=reorder(hdata_state$state_name, (vaccine_state$`Cumulative Vaccine Administration`/
                                                     censusage$X10.years.and.over)*1000, FUN=mean),
                y=hdata_state$`Daily Hospitalization Rate`* scalevstate), color=col[1],group=2,size=0.9) +
  
  scale_y_continuous(sec.axis = sec_axis (~./scalevstate, 
                                          name = "Cumulative Hospital adimissions 
  per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0,size=13),
        axis.text.y.right=element_text(colour=col[1]),
        axis.ticks.y.right=element_line(colour=col[1]),
        axis.title.y.right=element_text(colour=col[1],size=12,face="bold"),
        axis.text.y=element_text(colour=col[2]),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2],size=12,face="bold"),
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(face='bold',size=12),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(1, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black")) +
  scale_linetype_manual(values = linetype1) + 
  labs(linetype="Vaccine Type") + ylab('Cumulative vaccination rate 
  per 1000') + xlab('States') +
  ggtitle("Statewise Cumulative Vaccination & COVID-19 Hospitalization")

##### Supplementary Figure 1. State wise comparison of vaccination rate and hospital admissions with confirmed COVID-19 across al 50 states; X-axis organized in acending order of total vaccine uptake for 50 states


# making subset of original data for correlation based on dates in second table

hdata_dec20_corr <- hdata_date[which(hdata_date$Date %in% vaccine_date$Date),]

vaccine_hosp_corr <- cbind(hdata_dec20_corr,vaccine_date$`Cumulative Vaccine Count`)
colnames(vaccine_hosp_corr) <- c('Date','Hospitalization','Vaccination')
NonCumulative_Vax <- vaccine_hosp_corr$Vaccination-lag(vaccine_hosp_corr$Vaccination)
vaccine_hosp_corr <- cbind(vaccine_hosp_corr,NonCumulative_Vax)
colnames(vaccine_hosp_corr) <- c('Date','Hospitalization','Cumulative_Vax','NonCumulative_Vax')
as.numeric(vaccine_hosp_corr[is.na(vaccine_hosp_corr)] <- 0)



##########Vaccinated & Unvaccinated hospitalized (RATE is per 100,000) ########
vax_unvax_hosp_file <- read.csv("https://data.cdc.gov/api/views/k3na-u7xf/rows.csv?accessType=DOWNLOAD",fileEncoding = 'UTF-8-BOM')

#vax_unvax_hosp1 <- read.csv("CDC_hosp_vax_unvax.csv",fileEncoding = 'UTF-8-BOM')
#vaxhosp1 <- with(vax_unvax_hosp, tapply(Rate.in.fully.vaccinated, Week.ending, mean))
#unvaxhosp1 <- with(vax_unvax_hosp, tapply(Rate.in.unvaccinated, Week.ending, mean))


## Using zoo package's as.yearmon to convert date format 
vax_unvax_hosp_file$Month <- as.Date(as.yearmon(vax_unvax_hosp_file$Month), format = "%Y-%m")
unvax_hosp <- subset(vax_unvax_hosp_file, Vaccine.status %in% 'Unvaccinated')
vax_hosp <- subset(vax_unvax_hosp_file, Vaccine.status %in% c('Primary series','Vaccinated, no bivalent booster',   
                                                              'Primary series & 1 or more booster',
                                                              'Updated (bivalent) booster',
                                                              'Primary series & 2 or more boosters'))
vax_hosp <- data.frame(with(vax_hosp, tapply(Rate, Month, mean)))
colnames(vax_hosp) <- c('Vaccinated Rates')
unvax_hosp <- data.frame(with(unvax_hosp, tapply(Rate, Month, mean)))
colnames(unvax_hosp) <- c('Unvaccinated Rates')
vax_unvax_hosp <- data.frame(cbind(vax_hosp,unvax_hosp$`Unvaccinated Rates`))
colnames(vax_unvax_hosp) <- c('Vaccinated Rates','Unvaccinated Rates')


scalevax <-  max(vaccine_date$`Cumulative Vaccine Count`) / max(hdata_date$`Daily Hospital Admissions`)

gg1a <- ggplot()+
  geom_line(aes(as.Date(vaccine_date$Date, format = "%Y-%m-%d"), 
                vaccine_date$`Cumulative Vaccine Count`), 
            color=col[2],size=0.9) +
  geom_line(aes(as.Date(hdata_date$Date, format = "%Y-%m-%d"), 
                hdata_date$`Daily Hospital Admissions`*scalevax), 
            color=col[1],size=0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2020-12-14")),
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-01")),color = "purple",
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-01")),color = "red",
             linetype="dotted",size=0.9)+
  annotate("text", x = as.Date("2020-12-14"), y = 510000000, hjust = 0, vjust = 1, label = "Alpha") +
  annotate("text", x = as.Date("2021-07-03"), y = 510000000, hjust = 0, vjust = 1, label = "Delta", color="purple") +
  annotate("text", x = as.Date("2021-12-04"), y = 510000000, hjust = 0, vjust = 1, label = "Omicron", color="red") +
  scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y") +
  scale_y_continuous(sec.axis = sec_axis (~./scalevax, 
                                          name = "National daily hospital 
               adimissions with confirmed COVID-19"))+
  theme(axis.text.x=element_text(angle=-20,hjust=0,size=12),
        axis.text.y.right=element_text(colour=col[1],size=12),
        axis.ticks.y.right=element_line(colour=col[1]),
        axis.title.y.right=element_text(colour=col[1],size=13,face='bold'),
        axis.text.y=element_text(colour=col[2],size=12),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2],size=13,face='bold'),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black")) +
  scale_colour_manual(values=col) +
  labs(colour="")+
  ylab('National Cumulative 
       Vaccination Count') + 
  xlab('') + ggtitle("Timeline of National Cumulative Vaccination & COVID-19 Hospitalization")

shapiro.test(vaccine_hosp_corr$Hospitalization)
shapiro.test(vaccine_hosp_corr$NonCumulative_Vax)

cor.test(vaccine_hosp_corr$NonCumulative_Vax,vaccine_hosp_corr$Hospitalization,
         alternative = "two.sided",method = "pearson")

gg2a <- ggplot(vaccine_hosp_corr, aes(NonCumulative_Vax,Hospitalization))+
  geom_point(alpha=0.6)+
  xlab('National daily Vaccination')+ylab('National Hospitalization 
  count with lab confirmed COVID-19')+
  geom_smooth(method = "lm", formula= y~x, se = FALSE)+
  theme(axis.text.x=element_text(colour=col[2],angle=-90,hjust=0,size=13),
        axis.text.y=element_text(colour=col[1]),
        axis.ticks.y=element_line(colour=col[1]),
        axis.title.y=element_text(colour=col[1],size=13,face="bold"),
        axis.title.x=element_text(colour=col[2],size=15,face="bold"),
        legend.position="bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(face='bold',size=12),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black")) +
  ggtitle("Pearson's correlation between 
  National daily Vaccination & 
  COVID-19 Hospitalization counts")

colorvaxunvax <- c('UNVACCINATED'='orangered','VACCINATED'='green4')

gg3a <- ggplot()+
  geom_line(aes(as.Date(x=rownames(vax_unvax_hosp), format = "%Y-%m-%d"),
                y=vax_unvax_hosp$`Unvaccinated Rates`, color='UNVACCINATED'),group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(vax_unvax_hosp), format = "%Y-%m-%d"),
                y=vax_unvax_hosp$`Vaccinated Rates`, color='VACCINATED'),group=1,size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-01")),
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-01")),color = "red",
             linetype="dotted",size=0.9)+
  annotate("text", x = as.Date("2021-07-03"), y = 140, hjust = 0, vjust = 1, label = "Delta") +
  annotate("text", x = as.Date("2021-12-04"), y = 140, hjust = 0, vjust = 1, label = "Omicron", color="red") +
  theme(axis.text.x=element_text(angle=-30,hjust=0,size=10),
        axis.text.y=element_text(colour=col[1]),
        axis.ticks.y=element_line(colour=col[1]),
        axis.title.y=element_text(colour=col[1],size=13,face="bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.key = element_rect(fill = NA))+
  scale_color_manual(values = colorvaxunvax) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  scale_y_continuous(limits = c(0,560), breaks=seq(0,560,50)) +
  xlab('Dates') + ylab('Hospitalization Rates 
  with lab confirmed 
  COVID-19 per 100,000')+
  labs(color="")+
  ggtitle("Hospitalization trends in Vaccinated & Unvaccinated populations")


#ggarrange(gg1a, ggarrange(gg2a,gg3a, ncol = 2, labels = c("b","c"), widths = c(5,7), heights = 4),nrow = 2, labels = "a", heights = 4)

gg2a3a <- plot_grid(gg2a,gg3a, nrow = 1, ncol = 2, labels = c("b","c"),align = "v",rel_widths = c(2,3),label_size = 15)
plot_grid(gg1a,gg2a3a, nrow = 2, ncol = 1,rel_heights = c(3,3), labels = c("a",""),label_size = 15)


##### Figure 3a. Comparison of daily COVID-19 hospital admissions against national cumulative vaccination event
##### Figure 3b. Pearson’s correlation between National COVID-19 hospital admission and National Cumulative vaccination (Moderna, Pfizer, Janssen)
##### Figure 3c. Weekly national hospitalization trend among vaccinated and unvaccinated US populations


  
  
  
########################Vaccine Hesitancy ###############################

vax_hesi <- read.csv('Vaccine_Hesitancy.csv')
statewise_hesitant <- with(vax_hesi, tapply(Estimated.hesitant,State, sum))

# counts repetitions in a table
county_count <- table(vax_hesi$State)



###########vaccine hesitancy graph ###########
state_avg <- statewise_hesitant/county_count
state_hesitant <- data.frame(statewise_hesitant)
state_hesitant <- cbind(rownames(state_hesitant),state_hesitant,county_count,state_avg)
rownames(state_hesitant) <- NULL
colnames(state_hesitant) <- c("State","Vaccine hesitancy sum from Counties","Var1.1","# Counties",
                              "Var1.2","State Avg Vaccine hesitancy")
state_hesitant <- subset(state_hesitant,select=-c(Var1.1,Var1.2))


remove2 = c("District Of Columbia") 
row_to_remove2 <- which(state_hesitant$State %in% remove2)
state_hesitant <- state_hesitant[-row_to_remove2,]
state_hesitant <- cbind(state_hesitant,log(state_hesitant$`State Avg Vaccine hesitancy`+1)) 
colnames(state_hesitant) <- c("State","Vaccine hesitancy sum from Counties","# Counties",
                              "State Avg Vaccine hesitancy","Log Avg hesitancy")


scale4 <- max(state_hesitant$`Log Avg hesitancy`)/
  max(hdata_state$`Daily Hospitalization Rate`)

ggplot() + 
  geom_line(aes(x=reorder(state_hesitant$State, state_hesitant$`Log Avg hesitancy`, FUN=mean),
                y= state_hesitant$`Log Avg hesitancy`),
            color='darkslategrey',group=1,size=0.9) + 
  geom_line(aes(x= reorder(hdata_state$state_name, state_hesitant$`Log Avg hesitancy`, FUN=mean), 
                y= (hdata_state$`Daily Hospitalization Rate`)*scale4),
            color=col[1],group=2,size=0.9) +
  scale_y_continuous(sec.axis = sec_axis (~./scale4, name = "Hospitalizations per 1000 
   with confirmed COVID-19"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0,size=14),
        axis.text.y.right=element_text(colour=col[1]),
        axis.ticks.y.right=element_line(colour=col[1]),
        axis.title.y.right=element_text(colour=col[1],size=15,face="bold"),
        axis.text.y=element_text(colour='darkslategrey'),
        axis.ticks.y=element_line(colour='darkslategrey'),
        axis.title.y=element_text(colour='darkslategrey',size=15,face="bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black")) +
  scale_colour_manual(values=col) +
  labs(colour="") + ylab('Log of Vaccine Hesitancy %') + xlab('STATES')


##### Supplementary Figure 4. Comparison of vaccine hesitancy for 50 states with COVID-19 hospital admissions for respective states; X-axis organized in acending order of vaccine hesitancy for 50 states


shapiro.test(state_hesitant$`State Avg Vaccine hesitancy`)

cor.test(state_hesitant$`State Avg Vaccine hesitancy`,
         hdata_state$`Daily Hospitalization Rate`,
         alternative = "two.sided",method = "spearman")


ggplot()+
  geom_point(aes(state_hesitant$`State Avg Vaccine hesitancy`,hdata_state$`Daily Hospitalization Rate`))+
  geom_smooth(aes(state_hesitant$`State Avg Vaccine hesitancy`,hdata_state$`Daily Hospitalization Rate`),
              method='lm', formula= y~x, se = FALSE) +
  xlab('Vaccine hesitancy for respective state')+
  ylab('Hospital Admissions per 1000 
  with lab confirmed COVID-19')+
  theme(legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text.y=element_text(colour='orangered3',size=12),
        axis.ticks.y=element_line(colour='orangered3'),
        axis.title.y=element_text(colour='orangered3',size=15,face="bold"),
        axis.text.x=element_text(colour='darkslategrey',size=12),
        axis.ticks.x=element_line(colour='darkslategrey'),
        axis.title.x=element_text(colour='darkslategrey',size=15,face="bold"))


##### Supplementary Figure 2. Pearson's correlation test between vaccine hesitancy for 50 states and COVID-19 hospital admissions for respective states 


  
  
  
# REGIONAL vaccination hesitancy & COVID19 hospitalization

western <- c("Alaska","Arizona","California","Colorado",
             "Hawaii","Idaho","Montana","Nevada","New Mexico",
             "Oregon","Utah","Washington","Wyoming")
midwestern <- c("Illinois","Indiana","Iowa","Kansas","Michigan",
                "Minnesota","Missouri","Nebraska","North Dakota",
                "Ohio","South Dakota","Wisconsin")
southern <- c("Alabama","Arkansas","Delaware","Florida","Georgia",
              "Kentucky","Louisiana","Maryland","Mississippi",
              "North Carolina","Oklahoma","South Carolina",
              "Tennessee","Texas","Virginia","West Virginia")
northeastern <- c("Connecticut","Maine","Massachusetts","New Hampshire",
                  "New Jersey","New York","Pennsylvania","Rhode Island","Vermont")


region1 <- "west"
region2 <- "midwest"
region3 <- "south"
region4 <- "northeast"

west <- state_hesitant[which(state_hesitant$State %in% western),]
westh <- hdata_state[which(hdata_state$state_name %in% western),]
westv <- vaccine_state[which(vaccine_state$State %in% western),]
west <- cbind(west,westh$`Daily Hospitalization Rate`,westv$cumulative_vaxrate,region1)
colnames(west) <- c('State','Vaccine hesitancy sum','# Counties',
                    'State Avg Vaccine hesitancy','Log Avg hesitancy',
                    'Hospitalization rate','Cumulative Vax rate','Region')

midwest <- state_hesitant[which(state_hesitant$State %in% midwestern),]
midwesth <- hdata_state[which(hdata_state$state_name %in% midwestern),]
midwestv <- vaccine_state[which(vaccine_state$State %in% midwestern),]
midwest <- cbind(midwest,midwesth$`Daily Hospitalization Rate`,midwestv$cumulative_vaxrate,region2)
colnames(midwest) <- c('State','Vaccine hesitancy sum','# Counties',
                       'State Avg Vaccine hesitancy','Log Avg hesitancy',
                       'Hospitalization rate','Cumulative Vax rate','Region')

south <- state_hesitant[which(state_hesitant$State %in% southern),]
southh <- hdata_state[which(hdata_state$state_name %in% southern),]
southv <- vaccine_state[which(vaccine_state$State %in% southern),]
south <- cbind(south,southh$`Daily Hospitalization Rate`,southv$cumulative_vaxrate,region3)
colnames(south) <- c('State','Vaccine hesitancy sum','# Counties',
                     'State Avg Vaccine hesitancy','Log Avg hesitancy',
                     'Hospitalization rate','Cumulative Vax rate','Region')

northeast <- state_hesitant[which(state_hesitant$State %in% northeastern),]
northeasth <- hdata_state[which(hdata_state$state_name %in% northeastern),]
northeastv <- vaccine_state[which(vaccine_state$State %in% northeastern),]
northeast <- cbind(northeast,northeasth$`Daily Hospitalization Rate`,northeastv$cumulative_vaxrate,region4)
colnames(northeast) <- c('State','Vaccine hesitancy sum','# Counties',
                         'State Avg Vaccine hesitancy','Log Avg hesitancy',
                         'Hospitalization rate','Cumulative Vax rate','Region')

regional_hesitancy <- rbindlist(list(west,midwest,south,northeast), fill = TRUE)




#######Regional Vaccine hesitancy graph ######
colorNATIONAL <-  c("NATIONAL AVG"="black","WEST"="red","MIDWEST"="darkgoldenrod",
                    "SOUTH"="purple","NORTHEAST"="dodgerblue") 
gghosphesi <- ggplot() + 
  geom_point(aes(x=west$`State Avg Vaccine hesitancy`,y=west$`Hospitalization rate`, 
                 color='WEST'), shape=8, size=2, group = 1) + 
  geom_smooth(aes(x=west$`State Avg Vaccine hesitancy`,y=west$`Hospitalization rate`, 
                  color='WEST'), method=lm,formula = y ~ x, se=FALSE)+
  geom_point(aes(x=midwest$`State Avg Vaccine hesitancy`,y=midwest$`Hospitalization rate`, 
                 color='MIDWEST'), shape=9, size=2, group = 2) +
  geom_smooth(aes(x=midwest$`State Avg Vaccine hesitancy`,y=midwest$`Hospitalization rate`, 
                  color='MIDWEST'), method=lm,formula = y ~ x, se=FALSE)+
  geom_point(aes(x=south$`State Avg Vaccine hesitancy`,y=south$`Hospitalization rate`, 
                 color='SOUTH'), shape=12, size=2, group = 3) +
  geom_smooth(aes(x=south$`State Avg Vaccine hesitancy`,y=south$`Hospitalization rate`, 
                  color='SOUTH'), method=lm,formula = y ~ x, se=FALSE)+
  geom_point(aes(x=northeast$`State Avg Vaccine hesitancy`,y=northeast$`Hospitalization rate`, 
                 color='NORTHEAST'), shape=11, size=2, group = 4) +
  geom_smooth(aes(x=northeast$`State Avg Vaccine hesitancy`,y=northeast$`Hospitalization rate`, 
                  color='NORTHEAST'), method=lm,formula = y ~ x, se=FALSE)+
  geom_smooth(aes(state_hesitant$`State Avg Vaccine hesitancy`,
                  hdata_state$`Daily Hospitalization Rate`, color="NATIONAL AVG"),
              method='lm', formula= y~x, se = FALSE) +
  theme(axis.text.x=element_text(angle=-90,hjust=0,size=14),
        axis.text.y=element_text(colour='darkslategrey'),
        axis.ticks.y=element_line(colour='darkslategrey'),
        axis.title.y=element_text(colour='darkslategrey',size=15),
        axis.title.x=element_text(colour='darkslategrey',size=15),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black"))+
  scale_color_manual(values = colorNATIONAL) +
  labs(x = "Vaccine hesitancy",y = "COVID-19 hospitalizations per 1000",
       title="Regional vaccine hesitancy & Hospitalizations per 1000
  with lab confirmed COVID-19", color=NULL)


###############Vaccine & Vaccine hesitancy correlation ###############
cor.test(state_hesitant$`State Avg Vaccine hesitancy`,
         vaccine_state$cumulative_vaxrate,
         alternative = "two.sided",method = "spearman")


ggvaxhesi <- ggplot() + 
  geom_point(aes(x=west$`State Avg Vaccine hesitancy`,y=west$`Cumulative Vax rate`, color='WEST'), 
             shape=8, size=2, group = 1) + 
  geom_smooth(aes(x=west$`State Avg Vaccine hesitancy`,y=west$`Cumulative Vax rate`, 
                  color='WEST'), method=lm, formula = y ~ x, se=FALSE)+
  geom_point(aes(x=midwest$`State Avg Vaccine hesitancy`,y=midwest$`Cumulative Vax rate`, color='MIDWEST'),
             shape=9, size=2, group = 2) +
  geom_smooth(aes(x=midwest$`State Avg Vaccine hesitancy`,y=midwest$`Cumulative Vax rate`, 
                  color='MIDWEST'), method=lm, formula = y ~ x, se=FALSE)+
  geom_point(aes(x=south$`State Avg Vaccine hesitancy`,y=south$`Cumulative Vax rate`, color='SOUTH'),
             shape=12, size=2, group = 3) +
  geom_smooth(aes(x=south$`State Avg Vaccine hesitancy`,y=south$`Cumulative Vax rate`, 
                  color='SOUTH'), method=lm,formula = y ~ x, se=FALSE)+
  geom_point(aes(x=northeast$`State Avg Vaccine hesitancy`,y=northeast$`Cumulative Vax rate`, color='NORTHEAST'), 
             shape=11, size=2, group = 4) +
  geom_smooth(aes(x=northeast$`State Avg Vaccine hesitancy`,y=northeast$`Cumulative Vax rate`, 
                  color='NORTHEAST'), method=lm, formula = y ~ x,se=FALSE)+
  geom_smooth(aes(state_hesitant$`State Avg Vaccine hesitancy`,
                  vaccine_state$cumulative_vaxrate, color="NATIONAL AVG"),
              method='lm', formula= y~x, se = FALSE) +
  theme(axis.text.x=element_text(angle=-90,hjust=0,size=14),
        axis.text.y=element_text(colour='darkslategrey'),
        axis.ticks.y=element_line(colour='darkslategrey'),
        axis.title.y=element_text(colour='darkslategrey',size=15),
        axis.title.x=element_text(colour='darkslategrey',size=15),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black"))+
  scale_color_manual(values = colorNATIONAL) +
  labs(x = "Vaccine hesitancy",y = "COVID-19 vaccination rates per 1000",
       title="Regional vaccine hesitancy & 
  COVID19 vaccine coverage",color=NULL)

#ggarrange(gghosphesi, ggvaxhesi,labels = c("a","b"),ncol = 2, nrow = 1)

plot_grid(gghosphesi,ggvaxhesi, nrow = 1, ncol = 2,align = "h", labels = c("a","b"),label_size = 16)




##### Supplementary Figure 5a. Correlation between vaccine hesitancy and hospital admissions with lab confirmed COVID-19 per 1000 [national avg. and national regional states]
##### Supplementary Figure 5b. Correlation between vaccine hesitancy and vaccination rates per 1000 [national avg. and national regional states]


  
  
  

#######################Cases and Deaths pre/post vax ###########################
#covid_cases_deaths <- read.csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD",fileEncoding = 'UTF-8-BOM')

covid_cases_deaths <- read.csv("cases_and_deaths_by_states.csv",fileEncoding = 'UTF-8-BOM')

# Rename the column
names(covid_cases_deaths)[names(covid_cases_deaths) == "date_updated"] <- "submission_date"
names(covid_cases_deaths)[names(covid_cases_deaths) == "new_cases"] <- "new_case"
names(covid_cases_deaths)[names(covid_cases_deaths) == "new_deaths"] <- "new_death"

covid_cases_deaths$submission_date <- format(as.Date(covid_cases_deaths$submission_date, format = "%m/%d/%Y"), "%Y-%m-%d")

as.numeric(covid_cases_deaths$new_case[is.na(covid_cases_deaths$new_case)] <- 0)
state_cases <- subset(covid_cases_deaths,select=c('submission_date','state','new_case'),
                      as.Date(submission_date, format = "%Y-%m-%d") > "2020-12-13")
state_cases <- with(state_cases, tapply(new_case,state, sum))
state_cases <- data.frame(state_cases)

state_cases <- cbind(State=rownames(state_cases),state_cases)
rownames(state_cases) <- NULL

remove3 = c("AS","FSM","GU","NYC","PW","RMI","DC","MP","VI","PR") 
row_to_remove3 <- which(state_cases$State %in% remove3)
state_cases <- state_cases[-row_to_remove3,]

state_cases <- cbind(state_name,state_cases)
state_cases <- select(state_cases,-State)
colnames(state_cases) <- c("State","COVID19 cases")

state_cases <- state_cases %>% arrange(State,`COVID19 cases`)
case_rate <- (state_cases$`COVID19 cases`/censusage$Total)*1000
as.numeric(covid_cases_deaths[is.na(covid_cases_deaths)] <- 0)

remove1 = c("AS", "DC", "FSM", "GU", "MP", "NYC", "PR", "PW", "RMI", "VI") 
rows_to_remove1 <- which(covid_cases_deaths$state %in% remove1)
casesndeaths <- covid_cases_deaths[-rows_to_remove1,]
cases1 <- data.frame(with(casesndeaths, tapply(new_case, list(submission_date,state), sum)))
deaths1 <- data.frame(with(casesndeaths, tapply(new_death, list(submission_date,state), sum)))



death_statewise <- data.frame(cbind(state_name, colSums(deaths1)))
death_statewise <- death_statewise[order(death_statewise$state_name),]
deathrate1 <- (as.numeric(death_statewise$V2)/censusage$Total)*1000
death_statewise <- cbind(death_statewise,deathrate1)



################Hosp & Cases state plot ##################
case_total <- data.frame(colSums(cases1))
case_total <- cbind(state_name,case_total)
case_total <- case_total[order(case_total$state_name),]
case_total <- cbind(case_total, (case_total$colSums.cases1./censusage$Total)*1000)
colnames(case_total) <- c('State','Cases','Cases per 1000')

death_total <- data.frame(colSums(deaths1))
death_total <- cbind(state_name,death_total)
death_total <- death_total[order(death_total$state_name),]
death_total <- cbind(death_total, (death_total$colSums.deaths1./censusage$Total)*1000)
colnames(death_total) <- c('State','Deaths','Deaths per 1000')

# Top rates of cases, deaths and vaccines

cases2 <- data.frame(t(cases1))
names(cases2) <- sub("^X", "", names(cases2))
cases_rate <- cbind(state_name,cases2)
cases_rate <- cases_rate[order(cases_rate$state_name),]
cases_rate <- (cases_rate[,-1]/censusage$Total)*1000
cases_rate <- data.frame(t(cases_rate))


deaths2 <- data.frame(t(deaths1))
names(deaths2) <- sub("^X", "", names(deaths2))
deaths_rate <- cbind(state_name,deaths2)
deaths_rate <- deaths_rate[order(deaths_rate$state_name),]
deaths_rate <- (deaths_rate[,-1]/censusage$Total)*1000
deaths_rate <- data.frame(t(deaths_rate))

vaccine_rate <- vaccine2[order(vaccine2$state_name),]
vaccine_rate <- (vaccine_rate[,-1]/censusage$X10.years.and.over)*1000
names(vaccine_rate) <- sub("^X", "", names(vaccine_rate))
vaccine_rate <- data.frame(t(vaccine_rate))


#############################################################################################################
### Pre_omicron cases deaths
cases_preomicron <- subset(cases1, as.Date(rownames(cases1), format = "%Y-%m-%d") < "2021-12-4")
deaths_predelta <- subset(deaths1, as.Date(rownames(deaths1), format = "%Y-%m-%d") < "2021-7-3")


case_total_PO <- data.frame(colSums(cases_preomicron))
case_total_PO <- cbind(state_name,case_total_PO)
case_total_PO <- case_total_PO[order(case_total_PO$state_name),]
colnames(case_total_PO) <- c('State','Cases pre-omicron')
case_total_PO <- cbind(case_total_PO,(case_total_PO$`Cases pre-omicron`/censusage$Total)*1000)
colnames(case_total_PO) <- c('State','Cases pre-omicron','Caserate pre-omicron')

death_total_PD <- data.frame(colSums(deaths_predelta))
death_total_PD <- cbind(state_name,death_total_PD)
death_total_PD <- death_total_PD[order(death_total_PD$state_name),]
colnames(death_total_PD) <- c('Death','Deaths pre-delta')
death_total_PD <- cbind(death_total_PD,(death_total_PD$`Deaths pre-delta`/censusage$Total)*1000)
colnames(death_total_PD) <- c('State','Deaths pre-delta','Deathrate pre-delta')




colorCH <- c("Hospitalization rate"="orangered1","Total Case rate"="darkblue","Case rate Pre-Omicron"="blueviolet")
scaleCH <- max(hdata_state$`Daily Hospitalization Rate`)/max(case_total$`Cases per 1000`)
ggCH <- ggplot()+
  geom_point(aes(x=reorder(hdata_state$state_name, hdata_state$`Daily Hospitalization Rate`, FUN=mean),
                y=hdata_state$`Daily Hospitalization Rate`, 
                color="Hospitalization rate"), group = 1, size=2) +
  geom_point(aes(x=reorder(case_total$State, hdata_state$`Daily Hospitalization Rate`, FUN=mean), 
                y=case_total$`Cases per 1000`*scaleCH, 
                color="Total Case rate"), group= 2 , size=2) +
  geom_point(aes(x=reorder(case_total_PO$State, hdata_state$`Daily Hospitalization Rate`, FUN=mean), 
                y=case_total_PO$`Caserate pre-omicron`*scaleCH, 
                color="Case rate Pre-Omicron"), group= 3 , size=2) +
  scale_y_continuous(sec.axis = sec_axis (~./scaleCH, name = "COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue"),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 12, face = "bold"),
        axis.text.y=element_text(colour=col[1]),
        axis.ticks.y=element_line(colour=col[1]),
        axis.title.y=element_text(colour=col[1], size = 12, face = "bold"),
        axis.title.x=element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = colorCH) +
  labs(colour="") + ylab('COVID-19 Hospitalizations per 1000') + xlab('STATES') +
  ggtitle('Statewise Hospitalization rate with lab confirmed COVID-19
  & COVID-19 case rate')


colorDH <- c("Hospitalization rate"="orangered1","Total Death rate"="goldenrod3","Death rate Pre-Delta"="yellowgreen")
scaleDH <- max(hdata_state$`Daily Hospitalization Rate`)/max(death_total$`Deaths per 1000`)
ggDH <- ggplot()+
  geom_point(aes(x=reorder(hdata_state$state_name, hdata_state$`Daily Hospitalization Rate`, FUN=mean),
                y=hdata_state$`Daily Hospitalization Rate`, 
                color="Hospitalization rate"), group = 1, size=2) +
  geom_point(aes(x=reorder(death_total$State, hdata_state$`Daily Hospitalization Rate`, FUN=mean), 
                y=death_total$`Deaths per 1000`*scaleDH, 
                color="Total Death rate"), group= 2, size=2) +
  geom_point(aes(x=reorder(death_total_PD$State, hdata_state$`Daily Hospitalization Rate`, FUN=mean), 
                y=death_total_PD$`Deathrate pre-delta`*scaleDH, 
                color="Death rate Pre-Delta"), group= 3, size=2) +
  scale_y_continuous(sec.axis = sec_axis (~./scaleDH, name = "COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="goldenrod3"),
        axis.ticks.y.right=element_line(colour="goldenrod3"),
        axis.title.y.right=element_text(colour="goldenrod3", size = 12, face = "bold"),
        axis.text.y=element_text(colour=col[1]),
        axis.ticks.y=element_line(colour=col[1]),
        axis.title.y=element_text(colour=col[1], size = 12, face = "bold"),
        axis.title.x=element_text(size = 12, face = "bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = colorDH) +
  labs(colour="") + ylab('COVID-19 Hospitalizations per 1000') + xlab('STATES') +
  ggtitle('Statewise Hospitalization rate with lab confirmed COVID-19
  & COVID-19 death rate')

#ggarrange(ggCH,ggDH, ncol = 2, labels = c("a","b"))

plot_grid(ggCH,ggDH, nrow = 1, ncol = 2,align = "h", labels = c("a","b"),label_size = 18)



##### Figure 1a. Cumulative COVID-19 hospitalization rates & cumulative case [Pre-Omicron and Total] rates across 50 states; X-axis organized in acending order of case rates for 50 states
##### Figure 1b. Cumulative COVID-19 hospitalization rates & cumulative death [Pre-Delta and Total] rates across 50 states; X-axis organized in acending order of death rates for 50 states


#case_death_vax <- read.csv('https://data.cdc.gov/api/views/54ys-qyzm/rows.csv?accessType=DOWNLOAD',fileEncoding = 'UTF-8-BOM')
#case_death_vax$month <- as.Date(as.yearmon(case_death_vax$month), format = "%Y-%m")

#case_vax_sub <- subset(case_death_vax,outcome %in% 'case')
#case_vax <- data.frame(with(case_vax_sub, tapply(vaccinated_with_outcome, list(month,vaccination_status), sum)))
#case_unvax <- data.frame(with(case_vax_sub, tapply(unvaccinated_with_outcome, list(month,vaccination_status), sum)))


#death_vax_sub <- subset(case_death_vax,outcome %in% 'death')
#death_vax <- data.frame(with(death_vax_sub, tapply(vaccinated_with_outcome, list(month,vaccination_status), sum)))
#death_unvax <- data.frame(with(death_vax_sub, tapply(unvaccinated_with_outcome, list(month,vaccination_status), sum)))




########### Variants ##########

delta_estimate <- read.csv("DeltaEstimate.csv")
b117_estimate <- read.csv("EstimatedB117.csv")
Omicron_estimate <- read.csv("Estimated_omnicron.csv")
#####################

#Omicron_proportion <- read.csv("https://lapis.cov-spectrum.org/open/v1/sample/aggregated?fields=date&pangoLineage=B.1.1.529*&country=USA&dataFormat=csv")
#Omicron_proportion$date <- format(as.Date(Omicron_proportion$date, format = "%Y-%m-%d"), "%Y-%m-%d")

#Omicron_remove <- which(Omicron_proportion$date %in% NA)
#Omicron_proportion <- Omicron_proportion[-Omicron_remove,]


#Omicron_national_total <- read.csv("https://lapis.cov-spectrum.org/open/v1/sample/aggregated?fields=date&country=USA&dataFormat=csv")
#Omicron_national_total$date <- format(as.Date(Omicron_national_total$date, format = "%Y-%m-%d"), "%Y-%m-%d")

#Omicron_remove2 <- which(Omicron_national_total$date %in% NA)
#Omicron_national_total <- Omicron_national_total[-Omicron_remove2,]

#Omicron_estimate <- cbind(Omicron_national_total[which(Omicron_national_total$date %in% Omicron_proportion$date),],Omicron_proportion$count)
#colnames(Omicron_estimate) <- c('Date','NationalSeq', 'OmicronProp')
#Omicron_estimate <- cbind(Omicron_estimate, Omicron_estimate$OmicronProp/Omicron_estimate$NationalSeq)
#colnames(Omicron_estimate) <- c('date','NationalSeq', 'OmicronProp','Proportion_Omicron')
#Omicron_estimate <- subset(Omicron_estimate,as.Date(Omicron_estimate$date, format = "%Y-%m-%d") > "2021-07-05")

#cases_date <- data.frame(cbind(rownames(cases1), rowSums(cases1)))
#rownames(cases_date) <- NULL
#Omicron_cases <- cases_date[which(cases_date$X1 %in% Omicron_estimate$date),]
#Omicron_estimate <- Omicron_estimate[which(Omicron_estimate$date %in% Omicron_cases$X1),]

#Omicron_estimate <- cbind(Omicron_estimate,Omicron_cases$X2,ceiling(Omicron_estimate$Proportion_Omicron*as.numeric(Omicron_cases$X2)))
#colnames(Omicron_estimate) <- c('date','NationalSeq', 'OmicronSeq','Proportion_Omicron','Total_daily_cases','estimatedCases')

########################

scale0 <- max(Omicron_estimate$estimatedCases)/max(hdata_date$`Daily Hospital Admissions`)

color_variant <- c("Omicron/B1.1.529"="blue","Delta/B1.617.2"="darkorchid4",
                   "Alpha/B1.1.7"="darkcyan","Hosptial admissions"="firebrick2")  




ggplot() +  
  geom_line(aes(x=as.Date(Omicron_estimate$date, format = "%Y-%m-%d"), 
                y=Omicron_estimate$estimatedCases,
                color="Omicron/B1.1.529"),size=0.9) +
  geom_line(aes(x=as.Date(delta_estimate$date, format = "%Y-%m-%d"), 
                y=delta_estimate$estimatedCases,
                color="Delta/B1.617.2"),size=0.9) +
  geom_line(aes(x=as.Date(b117_estimate$date, format = "%Y-%m-%d"), 
                y=b117_estimate$estimatedCases,
                color="Alpha/B1.1.7"),size=0.9) +
  geom_line(aes(x=as.Date(hdata_date$Date, format = "%Y-%m-%d"),
                y=hdata_date$`Daily Hospital Admissions`*scale0,
                color="Hosptial admissions"),size=0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2020-12-14")),color = 'darkcyan',
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-07-01")),color = "purple",
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-01")),color = "red",
             linetype="dotted",size=0.9)+
  annotate("text", x = as.Date("2020-12-14"), y = 1000000, hjust = 0, vjust = 1, label = "Alpha",color='darkcyan') +
  annotate("text", x = as.Date("2021-07-03"), y = 1000000, hjust = 0, vjust = 1, label = "Delta", color="purple") +
  annotate("text", x = as.Date("2021-12-04"), y = 1000000, hjust = 0, vjust = 1, label = "Omicron", color="red") +
  scale_y_continuous(sec.axis = sec_axis (~./scale0, name = "National daily hospitalvadimission 
  Counts with lab confirmed COVID-19"))+
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  theme(axis.text.x=element_text(angle=-30,hjust=0,size=13),
        axis.text.y.right=element_text(colour='firebrick1',size=14,face="bold"),
        axis.ticks.y.right=element_line(colour='firebrick1'),
        axis.title.y.right=element_text(colour='firebrick1'),
        axis.text.y=element_text(colour='darkorchid4',size=15),
        axis.ticks.y=element_line(colour='darkorchid4'),
        axis.title.y=element_text(colour='darkorchid4',size=14,face="bold"),
        axis.ticks.x=element_line(size=1),
        axis.title.x=element_text(size=12),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(face='bold',size=12),
        legend.key = element_rect(fill = NA),
        legend.key.size = unit(1, "cm"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black")) +
  scale_color_manual(values = color_variant) + 
  labs(colour="") + xlab('Date') + ylab('National daily estimated 
  Count of SARS-CoV-2 variants')


##### Figure 4. Comparison of the national daily COVID-19 hospital admissions against reported number of B1.1.7 (alpha), B1.617.2 (Delta) and B1.1.529 (Omicron) variants 
###### NOTE Data on Omicron cases was only available in raw format


  
  
  

vaccine_rate2 <- data.frame(t(vaccine_rate))
names(vaccine_rate2) <- sub("^X", "", names(vaccine_rate2))

cases_rate2 <- data.frame(t(cumsum(cases1)))
names(cases_rate2) <- sub("^X", "", names(cases_rate2))
cases_rate3 <- cbind(state_name,cases_rate2)
cases_rate3 <- cases_rate3[order(cases_rate3$state_name),]
cases_rate3 <- (cases_rate3[,-1]/censusage$Total)*1000

deaths_rate2 <- data.frame(t(cumsum(deaths1)))
names(deaths_rate2) <- sub("^X", "", names(deaths_rate2))
deaths_rate3 <- cbind(state_name,deaths_rate2)
deaths_rate3 <- deaths_rate3[order(deaths_rate3$state_name),]
deaths_rate3 <- (deaths_rate3[,-1]/censusage$Total)*1000



vax_max <- vaccine_rate2 %>% slice_max(vaccine_rate2[ , ncol(vaccine_rate2), drop = FALSE])
vax_max2 <- subset(vaccine_rate2,sort(z <- rank(vaccine_rate2[ , ncol(vaccine_rate2), drop = FALSE]),T)[2]==z)
vax_min <- vaccine_rate2 %>% slice_min (vaccine_rate2[ , ncol(vaccine_rate2), drop = FALSE])                   
vax_min2 <- subset(vaccine_rate2,sort(z <- rank(vaccine_rate2[ , ncol(vaccine_rate2), drop = FALSE]),T)[49]==z)

cases_max <- cases_rate3 %>% slice_max(cases_rate3[ , ncol(cases_rate3), drop = FALSE])
cases_max2 <- subset(cases_rate3,sort(z <- rank(cases_rate3[ , ncol(cases_rate3), drop = FALSE]),T)[2]==z)
cases_min <- cases_rate3 %>% slice_min (cases_rate3[ , ncol(cases_rate3), drop = FALSE])                   
cases_min2 <- subset(cases_rate3,sort(z <- rank(cases_rate3[ , ncol(cases_rate3), drop = FALSE]),T)[49]==z)

deaths_max <- deaths_rate3 %>% slice_max(deaths_rate3[ , ncol(deaths_rate3), drop = FALSE])
deaths_max2 <- subset(deaths_rate3,sort(z <- rank(deaths_rate3[ , ncol(deaths_rate3), drop = FALSE]),T)[2]==z)
deaths_min <- deaths_rate3 %>% slice_min (deaths_rate3[ , ncol(deaths_rate3), drop = FALSE])                   
deaths_min2 <- subset(deaths_rate3,sort(z <- rank(deaths_rate3[ , ncol(deaths_rate3), drop = FALSE]),T)[49]==z)

top_state_cases_rate <- data.frame(t(rbind(cases_max,cases_max2,cases_min,cases_min2)))
top_state_deaths_rate <- data.frame(t(rbind(deaths_max,deaths_max2,deaths_min,deaths_min2)))

top_state_vax_rate <- data.frame(t(rbind(vax_max,vax_max2,vax_min,vax_min2)))

vaccine3 <- vaccine_rate

top_state_cases_vax <- vaccine3[colnames(vaccine3) %in% colnames(top_state_cases_rate)]
top_state_cases_vax = top_state_cases_vax[names(top_state_cases_rate)]

top_state_deaths_vax <- vaccine3[colnames(vaccine3) %in% colnames(top_state_deaths_rate)]
top_state_deaths_vax = top_state_deaths_vax[names(top_state_deaths_rate)]


###################Hosp top table #######################

hosp1 <- data.frame(with(hdata1, tapply(hdata1$hdata_total, list(hdata1$date,hdata1$state), sum)))
as.numeric(hosp1[is.na(hosp1)] <- 0)
remove_h = c("AS","BP2", "DC","DD2", "FM", "GU","IH2", "LTC", "MH", "MP", "PR", "RP", "US", "VA2", "VI") 
rows_to_remove_h2 <- which(colnames(hosp1) %in% remove_h)
hosp1 <- hosp1[,-rows_to_remove_h2]

hosp2 <- data.frame(t(cumsum(hosp1)))
names(hosp2) <- sub("^X", "", names(hosp2))
hosp_rate <- cbind(state_name,hosp2)
hosp_rate <- hosp_rate[order(hosp_rate$state_name),]
hosp_rate <- (hosp_rate[,-1]/censusage$Total)*1000

hosp_max <- hosp_rate %>% slice_max(hosp_rate[ , ncol(hosp_rate), drop = FALSE])
hosp_max2 <- subset(hosp_rate,sort(z <- rank(hosp_rate[ , ncol(hosp_rate), drop = FALSE]),T)[2]==z)
hosp_min <- hosp_rate %>% slice_min (hosp_rate[ , ncol(hosp_rate), drop = FALSE])                   
hosp_min2 <- subset(hosp_rate,sort(z <- rank(hosp_rate[ , ncol(hosp_rate), drop = FALSE]),T)[49]==z)

top_state_hosp_rate <- data.frame(t(rbind(hosp_max,hosp_max2,hosp_min,hosp_min2)))
top_state_hosp_vax <- vaccine3[colnames(vaccine3) %in% colnames(top_state_hosp_rate)]
top_state_hosp_vax = top_state_hosp_vax[names(top_state_hosp_rate)]






#########Top 2 cases #########
scale_topcases <- max(top_state_cases_vax)/max(top_state_cases_rate)

linetype_topcases <- c("Vaccination Rate" = "solid", "Case Rate" = "dashed")

casecolors <-  c("violetred","saddlebrown","dodgerblue","limegreen")
names(casecolors) <- colnames(top_state_cases_rate)
casestates <- colnames(top_state_cases_rate)

gg1 <- ggplot() + 
  geom_line(aes(as.Date(x=rownames(top_state_cases_vax), format = "%Y.%m.%d"), 
                y= top_state_cases_vax[,1],
                color=casestates[1],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_cases_rate),format = "%Y.%m.%d"), 
                y= top_state_cases_rate[,1]*scale_topcases, 
                color=casestates[1],
                linetype="Case Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_cases_vax), format = "%Y.%m.%d"), 
                y= top_state_cases_vax[,2],
                color=casestates[2],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_cases_rate),format = "%Y.%m.%d"), 
                y= top_state_cases_rate[,2]*scale_topcases,
                color=casestates[2],
                linetype="Case Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_cases_vax), format = "%Y.%m.%d"), 
                y= top_state_cases_vax[,3],
                color=casestates[3],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_cases_rate),format = "%Y.%m.%d"), 
                y= top_state_cases_rate[,3]*scale_topcases,
                color=casestates[3],
                linetype="Case Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_cases_vax), format = "%Y.%m.%d"), 
                y= top_state_cases_vax[,4],
                color=casestates[4],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_cases_rate),format = "%Y.%m.%d"), 
                y= top_state_cases_rate[,4]*scale_topcases, 
                color=casestates[4], 
                linetype="Case Rate"), group = 2,size=0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2021-07-01")),
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-01")),color = "red",
             linetype="dotted",size=0.9)+
  annotate("text", x = as.Date("2021-07-01"), y = 950, hjust = 0, vjust = 1, label = "Delta") +
  annotate("text", x = as.Date("2021-12-01"), y = 950, hjust = 0, vjust = 1, label = "Omnicron", color="red") +
  scale_y_continuous(sec.axis = sec_axis (~./scale_topcases, name = "Cumulative COVID-19 
  cases per 1000"))+
  theme(axis.text.x=element_text(angle=-20,hjust=0,size=12),
        axis.text.y.right=element_text(colour='darkblue'),
        axis.ticks.y.right=element_line(colour='darkblue'),
        axis.title.y.right=element_text(colour='darkblue',size=12,face="bold"),
        axis.text.y=element_text(colour=col[2]),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2],size=12,face="bold"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5,size=13),
        legend.position = "bottom",
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        legend.key = element_rect(fill = NA)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  guides(linetype=guide_legend(nrow=2, byrow=TRUE))+
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccination 
  per 1000', x='Date',color = "Color", linetype = "Linetype") +
  scale_color_manual(values = casecolors) +
  scale_linetype_manual(values = linetype_topcases) +
  ggtitle("Top 2 States with Highest Cases 
  & Top 2 States with Lowest Cases")


#####Top 2 deaths #####


linetype_topdeaths <- c("Vaccination Rate" = "solid", "Death Rate" = "dashed")

scale_topdeaths <- max(top_state_deaths_vax)/max(top_state_deaths_rate)

deathcolors <-  c("violetred","saddlebrown","dodgerblue","limegreen")
names(deathcolors) <- colnames(top_state_deaths_rate)
deathstates <- colnames(top_state_deaths_rate)

gg2 <- ggplot() + 
  geom_line(aes(as.Date(x=rownames(top_state_deaths_vax), format = "%Y.%m.%d"), 
                y= top_state_deaths_vax[,1],
                color=deathstates[1],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_deaths_rate),format = "%Y.%m.%d"), 
                y= top_state_deaths_rate[,1]*scale_topdeaths, 
                color=deathstates[1],
                linetype="Death Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_deaths_vax), format = "%Y.%m.%d"), 
                y= top_state_deaths_vax[,2],
                color=deathstates[2],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_deaths_rate),format = "%Y.%m.%d"), 
                y= top_state_deaths_rate[,2]*scale_topdeaths,
                color=deathstates[2],
                linetype="Death Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_deaths_vax), format = "%Y.%m.%d"), 
                y= top_state_deaths_vax[,3],
                color=deathstates[3],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_deaths_rate),format = "%Y.%m.%d"), 
                y= top_state_deaths_rate[,3]*scale_topdeaths,
                color=deathstates[3],
                linetype="Death Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_deaths_vax), format = "%Y.%m.%d"), 
                y= top_state_deaths_vax[,4],
                color=deathstates[4],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_deaths_rate),format = "%Y.%m.%d"), 
                y= top_state_deaths_rate[,4]*scale_topdeaths, 
                color=deathstates[4], 
                linetype="Death Rate"), group = 2,size=0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2021-07-01")),
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-01")),color = "red",
             linetype="dotted",size=0.9)+
  annotate("text", x = as.Date("2021-07-01"), y = 950, hjust = 0, vjust = 1, label = "Delta") +
  annotate("text", x = as.Date("2021-12-01"), y = 950, hjust = 0, vjust = 1, label = "Omnicron", color="red") +
  scale_y_continuous(sec.axis = sec_axis (~./scale_topdeaths, name = "Cumulative COVID-19 
  deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-20,hjust=0,size=12),
        axis.text.y.right=element_text(colour='darkblue'),
        axis.ticks.y.right=element_line(colour='darkblue'),
        axis.title.y.right=element_text(colour='darkblue',size=12,face="bold"),
        axis.text.y=element_text(colour=col[2]),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2],size=12,face="bold"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5,size=13),
        legend.position = "bottom",
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        legend.key = element_rect(fill = NA)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  guides(linetype=guide_legend(nrow=2, byrow=TRUE))+
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccination 
  per 1000', x='Date', linetype = "Linetype", color = "Color") +
  scale_color_manual(values = deathcolors) +
  scale_linetype_manual(values = linetype_topdeaths) +
  ggtitle("Top 2 States with Highest Deaths 
  & Top 2 States with Lowest Deaths")


#####Top 2 hosp

linetype_tophosp <- c("Vaccination Rate" = "solid", "Hospitalization Rate" = "dashed")

scale_tophosp <- max(top_state_hosp_vax)/max(top_state_hosp_rate)

hospcolors <-  c("violetred","saddlebrown","dodgerblue","limegreen")
names(hospcolors) <- colnames(top_state_hosp_rate)
hospstates <- colnames(top_state_hosp_rate)

gg3 <- ggplot() + 
  geom_line(aes(as.Date(x=rownames(top_state_hosp_vax), format = "%Y.%m.%d"), 
                y= top_state_hosp_vax[,1],
                color=hospstates[1],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_hosp_rate),format = "%Y.%m.%d"), 
                y= top_state_hosp_rate[,1]*scale_tophosp, 
                color=hospstates[1],
                linetype="Hospitalization Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_hosp_vax), format = "%Y.%m.%d"), 
                y= top_state_hosp_vax[,2],
                color=hospstates[2],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_hosp_rate),format = "%Y.%m.%d"), 
                y= top_state_hosp_rate[,2]*scale_tophosp,
                color=hospstates[2],
                linetype="Hospitalization Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_hosp_vax), format = "%Y.%m.%d"), 
                y= top_state_hosp_vax[,3],
                color=hospstates[3],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_hosp_rate),format = "%Y.%m.%d"), 
                y= top_state_hosp_rate[,3]*scale_tophosp,
                color=hospstates[3],
                linetype="Hospitalization Rate"), group = 2,size=0.9) +
  geom_line(aes(as.Date(x=rownames(top_state_hosp_vax), format = "%Y.%m.%d"), 
                y= top_state_hosp_vax[,4],
                color=hospstates[4],
                linetype="Vaccination Rate"), group = 1,size=0.9) + 
  geom_line(aes(as.Date(x=rownames(top_state_hosp_rate),format = "%Y.%m.%d"), 
                y= top_state_hosp_rate[,4]*scale_tophosp, 
                color=hospstates[4], 
                linetype="Hospitalization Rate"), group = 2,size=0.9) +
  geom_vline(xintercept = as.numeric(as.Date("2021-07-01")),
             linetype="dotted",size=0.9)+
  geom_vline(xintercept = as.numeric(as.Date("2021-12-01")),color = "red",
             linetype="dotted",size=0.9)+
  annotate("text", x = as.Date("2021-07-01"), y = 950, hjust = 0, vjust = 1, label = "Delta") +
  annotate("text", x = as.Date("2021-12-01"), y = 950, hjust = 0, vjust = 1, label = "Omnicron", color="red") +
  scale_y_continuous(sec.axis = sec_axis (~./scale_tophosp, name = "Cumulative COVID-19 
  Hospitalizations per 1000"))+
  theme(axis.text.x=element_text(angle=-20,hjust=0,size=12),
        axis.text.y.right=element_text(colour='darkblue'),
        axis.ticks.y.right=element_line(colour='darkblue'),
        axis.title.y.right=element_text(colour='darkblue',size=12,face="bold"),
        axis.text.y=element_text(colour=col[2]),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2],size=12,face="bold"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5,size=13),
        legend.position = "bottom",
        legend.text = element_text(size=13),
        legend.title = element_text(size=14),
        legend.key = element_rect(fill = NA)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  guides(linetype=guide_legend(nrow=2, byrow=TRUE))+
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccination 
  per 1000', x='Date', linetype = "Linetype", color = "Color") +
  scale_color_manual(values = hospcolors) +
  scale_linetype_manual(values = linetype_tophosp) +
  ggtitle("Top 2 States with Highest Hospitalizations 
  & Top 2 States with Lowest Hospitalizations")


#ggarrange(gg1, gg2, gg3, labels = c("a","b","c"),nrow = 2,ncol = 2)

plot_grid(gg1,gg2,gg3, nrow = 1, ncol = 3,align = "hv", labels = c("a","b","c"),label_size = 16)




##### Figure 2a. Two states with highest and lowest COVID-19 case rates presented along with the vaccination rates for respective state
##### Figure 2b. Two states with highest and lowest COVID-19 death rates presented along with the vaccination rates for respective state
##### Figure 2c. Two states with highest and lowest hospitalization rates with lab confirmed COVID-19, presented along with the vaccination rates for respective state
###### NOTE Drop in cumulative vaccination records for certain states were observed due to data submission error 


  
  
  

######################################################
#                  REGIONALS
######################################################
######Regional census population #####

## For sorting rownames(censusage) only ####
western3 <- c("Alaska","Arizona","California","Colorado",
              "Hawaii","Idaho","Montana","Nevada","New.Mexico",
              "Oregon","Utah","Washington","Wyoming")
midwestern3 <- c("Illinois","Indiana","Iowa","Kansas","Michigan",
                 "Minnesota","Missouri","Nebraska","North.Dakota",
                 "Ohio","South.Dakota","Wisconsin")
southern3 <- c("Alabama","Arkansas","Delaware","Florida","Georgia",
               "Kentucky","Louisiana","Maryland","Mississippi",
               "North.Carolina","Oklahoma","South.Carolina",
               "Tennessee","Texas","Virginia","West Virginia")
northeastern3 <- c("Connecticut","Maine","Massachusetts","New.Hampshire",
                   "New.Jersey","New.York","Pennsylvania","Rhode.Island","Vermont")

westcensus <- censusage[which(rownames(censusage) %in% western3),]
midwestcensus <- censusage[which(rownames(censusage) %in% midwestern3),]
southcensus <- censusage[which(rownames(censusage) %in% southern3),]
northeastcensus <- censusage[which(rownames(censusage) %in% northeastern3),]

##############timeline regional cases and deaths ############

western2 <- c("AK","AZ","CA","CO",
              "HI","ID","MT","NV","NM",
              "OR","UT","WA","WY")
midwestern2 <- c("IL","IN","IA","KS","MI",
                 "MN","MO","NE","ND",
                 "OH","SD","WI")
southern2 <- c("AL","AR","DE","FL","GA",
               "KY","LA","MD","MS",
               "NC","OK","SC",
               "TN","TX","VA","WV")
northeastern2 <- c("CT","ME","MA","NH",
                   "NJ","NY","PA","RI","VT")


#########cases regionalization codes #############
west2 <- cases1[,which(colnames(cases1) %in% western2)]
west2 <- cbind(west2,(rowSums(west2)/sum(westcensus$Total))*1000)
westc <- cumsum(west2)
colnames(west2) <- c("AK","AZ","CA","CO",
                     "HI","ID","MT","NV","NM",
                     "OR","UT","WA","WY","total")
colnames(westc) <- c("AK","AZ","CA","CO",
                     "HI","ID","MT","NV","NM",
                     "OR","UT","WA","WY","total")

midwest2 <- cases1[,which(colnames(cases1) %in% midwestern2)]
midwest2 <- cbind(midwest2,(rowSums(midwest2)/sum(midwestcensus$Total))*1000)
midwestc <- cumsum(midwest2)
colnames(midwest2) <- c("IL","IN","IA","KS","MI",
                        "MN","MO","NE","ND",
                        "OH","SD","WI","total")
colnames(midwestc) <- c("IL","IN","IA","KS","MI",
                        "MN","MO","NE","ND",
                        "OH","SD","WI","total")

south2 <- cases1[,which(colnames(cases1) %in% southern2)]
south2 <- cbind(south2,(rowSums(south2)/sum(southcensus$Total))*1000)
southc <- cumsum(south2)
colnames(south2) <- c("AL","AR","DE","FL","GA",
                      "KY","LA","MD","MS",
                      "NC","OK","SC",
                      "TN","TX","VA","WV","total")
colnames(southc) <- c("AL","AR","DE","FL","GA",
                      "KY","LA","MD","MS",
                      "NC","OK","SC",
                      "TN","TX","VA","WV","total")

northeast2 <- cases1[,which(colnames(cases1) %in% northeastern2)]
northeast2 <- cbind(northeast2,(rowSums(northeast2)/sum(northeastcensus$Total))*1000)
northeastc <- cumsum(northeast2)
colnames(northeast2) <- c("CT","ME","MA","NH",
                          "NJ","NY","PA","RI","VT","total")
colnames(northeastc) <- c("CT","ME","MA","NH",
                          "NJ","NY","PA","RI","VT","total")


#############Vaccination regionalization code ###########
west3 <- vaccine1[,which(colnames(vaccine1) %in% western2)]
midwest3 <- vaccine1[,which(colnames(vaccine1) %in% midwestern2)]
south3 <- vaccine1[,which(colnames(vaccine1) %in% southern2)]
northeast3 <- vaccine1[,which(colnames(vaccine1) %in% northeastern2)]

west3 <- (west3/sum(westcensus$X10.years.and.over))*1000
midwest3 <- (midwest3/sum(midwestcensus$X10.years.and.over))*1000
south3 <- (south3/sum(southcensus$X10.years.and.over))*1000
northeast3 <- (northeast3/sum(northeastcensus$X10.years.and.over))*1000



##########Hospitalization regionalization code ###########
west4 <- hosp1[,which(colnames(hosp1) %in% western2)]
west4 <- cbind(west4,(rowSums(west4)/sum(westcensus$Total))*1000)
westh <- cumsum(west4)
colnames(west4) <- c("AK","AZ","CA","CO",
                     "HI","ID","MT","NV","NM",
                     "OR","UT","WA","WY","total")
colnames(westh) <- c("AK","AZ","CA","CO",
                     "HI","ID","MT","NV","NM",
                     "OR","UT","WA","WY","total")

midwest4 <- hosp1[,which(colnames(hosp1) %in% midwestern2)]
midwest4 <- cbind(midwest4,(rowSums(midwest4)/sum(midwestcensus$Total))*1000)
midwesth <- cumsum(midwest4)
colnames(midwest4) <- c("IL","IN","IA","KS","MI",
                        "MN","MO","NE","ND",
                        "OH","SD","WI","total")
colnames(midwesth) <- c("IL","IN","IA","KS","MI",
                        "MN","MO","NE","ND",
                        "OH","SD","WI","total")

south4 <- hosp1[,which(colnames(hosp1) %in% southern2)]
south4 <- cbind(south4,(rowSums(south4)/sum(southcensus$Total))*1000)
southh <- cumsum(south4)
colnames(south4) <- c("AL","AR","DE","FL","GA",
                      "KY","LA","MD","MS",
                      "NC","OK","SC",
                      "TN","TX","VA","WV","total")
colnames(southh) <- c("AL","AR","DE","FL","GA",
                      "KY","LA","MD","MS",
                      "NC","OK","SC",
                      "TN","TX","VA","WV","total")


northeast4 <- hosp1[,which(colnames(hosp1) %in% northeastern2)]
northeast4 <- cbind(northeast4,(rowSums(northeast4)/sum(northeastcensus$Total))*1000)
northeasth <- cumsum(northeast4)
colnames(northeast4) <- c("CT","ME","MA","NH",
                          "NJ","NY","PA","RI","VT","total")
colnames(northeasth) <- c("CT","ME","MA","NH",
                          "NJ","NY","PA","RI","VT","total")


##########Death regionalization code #########

west6 <- deaths1[,which(colnames(deaths1) %in% western2)]
west6 <- cbind(west6,(rowSums(west6)/sum(westcensus$Total))*1000)
westd <- cumsum(west6)
colnames(west6) <- c("AK","AZ","CA","CO",
                     "HI","ID","MT","NV","NM",
                     "OR","UT","WA","WY","total")
colnames(westd) <- c("AK","AZ","CA","CO",
                     "HI","ID","MT","NV","NM",
                     "OR","UT","WA","WY","total")

midwest6 <- deaths1[,which(colnames(deaths1) %in% midwestern2)]
midwest6 <- cbind(midwest6,(rowSums(midwest6)/sum(midwestcensus$Total))*1000)
midwestd <- cumsum(midwest6)
colnames(midwest6) <- c("IL","IN","IA","KS","MI",
                        "MN","MO","NE","ND",
                        "OH","SD","WI","total")
colnames(midwestd) <- c("IL","IN","IA","KS","MI",
                        "MN","MO","NE","ND",
                        "OH","SD","WI","total")

south6 <- deaths1[,which(colnames(deaths1) %in% southern2)]
south6 <- cbind(south6,(rowSums(south6)/sum(southcensus$Total))*1000)
southd <- cumsum(south6)
colnames(south6) <- c("AL","AR","DE","FL","GA",
                      "KY","LA","MD","MS",
                      "NC","OK","SC",
                      "TN","TX","VA","WV","total")
colnames(southd) <- c("AL","AR","DE","FL","GA",
                      "KY","LA","MD","MS",
                      "NC","OK","SC",
                      "TN","TX","VA","WV","total")

northeast6 <- deaths1[,which(colnames(deaths1) %in% northeastern2)]
northeast6 <- cbind(northeast6,(rowSums(northeast6)/sum(northeastcensus$Total))*1000)
northeastd <- cumsum(northeast6)
colnames(northeast6) <- c("CT","ME","MA","NH",
                          "NJ","NY","PA","RI","VT","total")
colnames(northeastd) <- c("CT","ME","MA","NH",
                          "NJ","NY","PA","RI","VT","total")



##############timeline regional cases ##############

scaleregion2 <- max(c(rowSums(west3),rowSums(midwest3),rowSums(south3),rowSums(northeast3)))/max(c(west2$total,midwest2$total,south2$total,northeast2$total))

ggwc <- ggplot()+
  geom_line(aes(as.Date(x=rownames(west3), format = "%Y-%m-%d"), y= rowSums(west3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(west2), format = "%Y-%m-%d"), y= west2$total*scaleregion2), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2, name = "Daily COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Western Region")

ggmc <- ggplot()+
  geom_line(aes(as.Date(x=rownames(midwest3), format = "%Y-%m-%d"), y= rowSums(midwest3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(midwest2), format = "%Y-%m-%d"), y= midwest2$total*scaleregion2), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2, name = "Daily COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Midwestern Region")


ggsc <- ggplot()+
  geom_line(aes(as.Date(x=rownames(south3), format = "%Y-%m-%d"), y= rowSums(south3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(south2), format = "%Y-%m-%d"), y= south2$total*scaleregion2), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2, name = "Daily COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Southern Region")


ggnc <- ggplot()+
  geom_line(aes(as.Date(x=rownames(northeast3), format = "%Y-%m-%d"), y= rowSums(northeast3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(northeast2), format = "%Y-%m-%d"), y= northeast2$total*scaleregion2), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2, name = "Daily COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Northeastern Region")

#ggarrange(ggwc, ggmc, ggsc, ggnc,labels = c("a"),ncol = 4, nrow = 1)

plot_grid(ggwc,ggmc,ggsc,ggnc, nrow = 1, ncol = 4,align = "v", labels = c("a"),label_size = 20)



##### Figure 7a. Comparison of daily COVID-19 cases per 1000 between the 4 national regions with respective cumulative vaccination rates


  

##############timeline regional hosps ##############

scaleregionh <- max(c(rowSums(west3),rowSums(midwest3),rowSums(south3),rowSums(northeast3)))/max(c(west4$total,midwest4$total,south4$total,northeast4$total))

ggwh <- ggplot()+
  geom_line(aes(as.Date(x=rownames(west3), format = "%Y-%m-%d"), y= rowSums(west3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(west4), format = "%Y-%m-%d"), y= west4$total*scaleregionh), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionh, name = "Daily COVID-19 hospitalizations per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Western Region")

ggmh <- ggplot()+
  geom_line(aes(as.Date(x=rownames(midwest3), format = "%Y-%m-%d"), y= rowSums(midwest3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(midwest4), format = "%Y-%m-%d"), y= midwest4$total*scaleregionh), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionh, name = "Daily COVID-19 hospitalizations per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Midwestern Region")

ggsh <- ggplot()+
  geom_line(aes(as.Date(x=rownames(south3), format = "%Y-%m-%d"), y= rowSums(south3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(south4), format = "%Y-%m-%d"), y= south4$total*scaleregionh), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionh, name = "Daily COVID-19 hospitalizations per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Southern Region")

ggnh <- ggplot()+
  geom_line(aes(as.Date(x=rownames(northeast3), format = "%Y-%m-%d"), y= rowSums(northeast3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(northeast4), format = "%Y-%m-%d"), y= northeast4$total*scaleregionh), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionh, name = "Daily COVID-19 hospitalizations per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Northeast Region")

#ggarrange(ggwh, ggmh, ggsh, ggnh,labels = c("b"),ncol = 4, nrow = 1)
plot_grid(ggwh,ggmh,ggsh,ggnh, nrow = 1, ncol = 4,align = "v", labels = c("b"),label_size = 20)




##### Figure 7b. Comparison of daily COVID-19 hospitalizations per 1000 between the 4 national regions with respective cumulative vaccination rates


  
 
##############timeline regional deaths ##############

scaleregion3 <- max(c(rowSums(west3),rowSums(midwest3),rowSums(south3),rowSums(northeast3)))/max(c(west6$total,midwest6$total,south6$total,northeast6$total))

ggwd <- ggplot()+
  geom_line(aes(as.Date(x=rownames(west3), format = "%Y-%m-%d"), y= rowSums(west3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(west6), format = "%Y-%m-%d"), y= west6$total*scaleregion3), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3, name = "Daily COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Western Region")

ggmd <- ggplot()+
  geom_line(aes(as.Date(x=rownames(midwest3), format = "%Y-%m-%d"), y= rowSums(midwest3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(midwest6), format = "%Y-%m-%d"), y= midwest6$total*scaleregion3), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3, name = "Daily COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Midwestern Region")

ggsd <- ggplot()+
  geom_line(aes(as.Date(x=rownames(south3), format = "%Y-%m-%d"), y= rowSums(south3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(south6), format = "%Y-%m-%d"), y= south6$total*scaleregion3), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3, name = "Daily COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Southern Region")

ggnd <- ggplot()+
  geom_line(aes(as.Date(x=rownames(northeast3), format = "%Y-%m-%d"), y= rowSums(northeast3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(northeast6), format = "%Y-%m-%d"), y= northeast6$total*scaleregion3), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3, name = "Daily COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations per 1000', x='Date') +
  ggtitle("Northeastern Region")

#ggarrange(ggwd, ggmd, ggsd, ggnd,labels = c("c"),ncol = 4, nrow = 1)

plot_grid(ggwd,ggmd,ggsd,ggnd, nrow = 1, ncol = 4,align = "v", labels = c("c"),label_size = 20)




##### Figure 7c. Comparison of daily COVID-19 deaths per 1000, between the 4 national regions with respective cumulative vaccination rates


  
  
  
  


##############timeline regional CUMULATIVE cases ##############

westcumC <- cumsum(west2)
midwestcumC <- cumsum(midwest2)
southcumC <- cumsum(south2)
northeastcumC <- cumsum(northeast2)

scaleregion2cumC <- max(c(rowSums(west3),rowSums(midwest3),rowSums(south3),rowSums(northeast3)))/
  max(c(westcumC$total,midwestcumC$total,southcumC$total,northeastcumC$total))

ggwccum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(west3), format = "%Y-%m-%d"), y= rowSums(west3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(westcumC), format = "%Y-%m-%d"), y= westcumC$total*scaleregion2cumC), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2cumC, name = "Cumulative COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Western Region")

ggmccum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(midwest3), format = "%Y-%m-%d"), y= rowSums(midwest3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(midwestcumC), format = "%Y-%m-%d"), y= midwestcumC$total*scaleregion2cumC), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2cumC, name = "Cumulative COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Midwestern Region")


ggsccum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(south3), format = "%Y-%m-%d"), y= rowSums(south3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(southcumC), format = "%Y-%m-%d"), y= southcumC$total*scaleregion2cumC), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2cumC, name = "Cumulative COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Southern Region")


ggnccum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(northeast3), format = "%Y-%m-%d"), y= rowSums(northeast3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(northeastcumC), format = "%Y-%m-%d"), y= northeastcumC$total*scaleregion2cumC), 
            color="darkblue",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 900), sec.axis = sec_axis (~./scaleregion2cumC, name = "Cumulative COVID-19 cases per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="darkblue",size=15),
        axis.ticks.y.right=element_line(colour="darkblue"),
        axis.title.y.right=element_text(colour="darkblue", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Northeastern Region")

#ggarrange(ggwc, ggmc, ggsc, ggnc,labels = c("a"),ncol = 4, nrow = 1)

plot_grid(ggwccum,ggmccum,ggsccum,ggnccum, nrow = 1, ncol = 4,align = "v", labels = c("a"),label_size = 20)



##### Supplementary Figure 3a. Comparison of Cumulative COVID-19 cases per 1000 between the 4 national regions with respective cumulative vaccination rates


##############timeline regional hosps ##############

westcumH <- cumsum(west4)
midwestcumH <- cumsum(midwest4)
southcumH <- cumsum(south4)
northeastcumH <- cumsum(northeast4)

scaleregionhcum <- max(c(rowSums(west3),rowSums(midwest3),rowSums(south3),rowSums(northeast3)))/
  max(c(westcumH$total,midwestcumH$total,southcumH$total,northeastcumH$total))

ggwhcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(west3), format = "%Y-%m-%d"), y= rowSums(west3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(westcumH), format = "%Y-%m-%d"), y= westcumH$total*scaleregionhcum), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionhcum, name = "Cumulative COVID-19 hospitalizations 
  per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Western Region")

ggmhcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(midwest3), format = "%Y-%m-%d"), y= rowSums(midwest3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(midwestcumH), format = "%Y-%m-%d"), y= midwestcumH$total*scaleregionhcum), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionhcum, name = "Cumulative COVID-19 hospitalizations 
  per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Midwestern Region")

ggshcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(south3), format = "%Y-%m-%d"), y= rowSums(south3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(southcumH), format = "%Y-%m-%d"), y= southcumH$total*scaleregionhcum), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionhcum, name = "Cumulative COVID-19 hospitalizations 
  per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Southern Region")

ggnhcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(northeast3), format = "%Y-%m-%d"), y= rowSums(northeast3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(northeastcumH), format = "%Y-%m-%d"), y= northeastcumH$total*scaleregionhcum), 
            color="firebrick2",group=2,size=0.9)+
  scale_y_continuous(limits = c(0, 850), sec.axis = sec_axis (~./scaleregionhcum, name = "Cumulative COVID-19 hospitalizations 
  per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 12),
        axis.text.y.right=element_text(colour="firebrick2",size=15),
        axis.ticks.y.right=element_line(colour="firebrick2"),
        axis.title.y.right=element_text(colour="firebrick2", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Northeast Region")

#ggarrange(ggwh, ggmh, ggsh, ggnh,labels = c("b"),ncol = 4, nrow = 1)
plot_grid(ggwhcum,ggmhcum,ggshcum,ggnhcum, nrow = 1, ncol = 4,align = "v", labels = c("b"),label_size = 20)




##### Supplementary Figure 3b. Comparison of cumulative COVID-19 hospitalizations per 1000 between the 4 national regions with respective cumulative vaccination rates

##############timeline regional deaths ##############

westcumD <- cumsum(west6)
midwestcumD <- cumsum(midwest6)
southcumD <- cumsum(south6)
northeastcumD <- cumsum(northeast6)

scaleregion3cumD <- max(c(rowSums(west3),rowSums(midwest3),rowSums(south3),rowSums(northeast3)))/
  max(c(westcumD$total,midwestcumD$total,southcumD$total,northeastcumD$total))


ggwdcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(west3), format = "%Y-%m-%d"), y= rowSums(west3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(westcumD), format = "%Y-%m-%d"), y= westcumD$total*scaleregion3cumD), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3cumD, name = "Cumulative COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Western Region")

ggmdcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(midwest3), format = "%Y-%m-%d"), y= rowSums(midwest3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(midwestcumD), format = "%Y-%m-%d"), y= midwestcumD$total*scaleregion3cumD), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3cumD, name = "Cumulative COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Midwestern Region")

ggsdcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(south3), format = "%Y-%m-%d"), y= rowSums(south3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(southcumD), format = "%Y-%m-%d"), y= southcumD$total*scaleregion3cumD), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3cumD, name = "Cumulative COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Southern Region")

ggndcum <- ggplot()+
  geom_line(aes(as.Date(x=rownames(northeast3), format = "%Y-%m-%d"), y= rowSums(northeast3)), 
            color=col[2],group=1,size=0.9)+
  geom_line(aes(as.Date(x=rownames(northeastcumD), format = "%Y-%m-%d"), y= northeastcumD$total*scaleregion3cumD), 
            color="goldenrod4",group=2,size=0.9)+
  scale_y_continuous(limits=c(0,850), sec.axis = sec_axis (~./scaleregion3cumD, name = "Cumulative COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0, size = 13),
        axis.text.y.right=element_text(colour="goldenrod4",size=15),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4", size = 15, face = "bold"),
        axis.text.y=element_text(colour=col[2],size=15),
        axis.ticks.y=element_line(colour=col[2]),
        axis.title.y=element_text(colour=col[2], size = 15, face = "bold"),
        axis.title.x=element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "3 months" , date_labels = "%Y-%b") +
  labs(y='Cumulative vaccinations 
  per 1000', x='Date') +
  ggtitle("Northeastern Region")

#ggarrange(ggwd, ggmd, ggsd, ggnd,labels = c("c"),ncol = 4, nrow = 1)

plot_grid(ggwdcum,ggmdcum,ggsdcum,ggndcum, nrow = 1, ncol = 4,align = "v", labels = c("c"),label_size = 20)




##### Supplementary Figure 3c. Comparison of cumulative COVID-19 deaths per 1000, between the 4 national regions with respective cumulative vaccination rates


  
  
  
  
  

########Correlation between Hospital beds by STATES #######

hosp_beds <- read.csv("Hospitalbed_Capacity2022.csv")

hosp_beds <- cbind(state_name,hosp_beds)
hosp_beds <- hosp_beds[order(hosp_beds$state_name),]

statedeaths_icu <- data.frame(cbind(hosp_beds$state_name,
                                    (hosp_beds$inpatient_beds/censusage$Total)*1000,
                                    death_total$`Deaths per 1000`))

colnames(statedeaths_icu) <- c("State","Total Hosp Beds per 1000","Deaths per 1000")

scale_icu <- max(as.numeric(statedeaths_icu$`Total Hosp Beds per 1000`))/
  max(as.numeric(statedeaths_icu$`Deaths per 1000`))




########Correlation between Hospital beds by STATES #######

########graph hosp beds and deaths ########
hospbeds <- ggplot()+
  geom_point(aes(x=reorder(statedeaths_icu$State, as.numeric(statedeaths_icu$`Total Hosp Beds per 1000`), mean),
                y=as.numeric(statedeaths_icu$`Total Hosp Beds per 1000`), color=col[1]),group=1,size=2)+
  geom_point(aes(x=reorder(statedeaths_icu$State, as.numeric(statedeaths_icu$`Total Hosp Beds per 1000`), mean),
                y=(as.numeric(statedeaths_icu$`Deaths per 1000`))*scale_icu), 
            color="goldenrod4",group=2,size=2) +
  scale_y_continuous(sec.axis = sec_axis (~./scale_icu, name = "COVID-19 deaths per 1000"))+
  theme(axis.text.x=element_text(angle=-90,hjust=0,size=13),
        axis.text.y.right=element_text(colour="goldenrod4"),
        axis.ticks.y.right=element_line(colour="goldenrod4"),
        axis.title.y.right=element_text(colour="goldenrod4",size=14,face="bold"),
        axis.text.y=element_text(colour=col[1]),
        axis.ticks.y=element_line(colour=col[1]),
        axis.title.y=element_text(colour=col[1],size=14, face="bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  xlab('States') + ylab('Staffed Hospital beds per 1000')+
  ggtitle("Number of staffed hospital beds in each 
  state per 1000 individuals & COVID-19 deaths per 1000")

shapiro.test(as.numeric(statedeaths_icu$`Total Hosp Beds per 1000`))

cor.test(as.numeric(statedeaths_icu$`Total Hosp Beds per 1000`),as.numeric(statedeaths_icu$`Deaths per 1000`),
         alternative = "two.sided",method = "spearman")


hospcorrtest <- ggplot(statedeaths_icu, aes(as.numeric(`Total Hosp Beds per 1000`),as.numeric(`Deaths per 1000`)))+
  geom_point()+
  xlab('Hospital beds per 1000')+ylab('Deaths per 1000 due to COVID-19')+
  geom_smooth(method = "lm", formula= y~x, se = FALSE)+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        panel.border = element_rect(fill = NA, color = "black")) +
  ggtitle("Spearman's correlation between number of staffed hospital 
beds in each state per 1000 individuals & COVID-19 deaths per 1000")

#ggarrange(hospbeds, hospcorrtest,widths= c(7,4),labels = c("a","b"), ncol = 2, nrow = 1, align="h")

plot_grid(hospbeds,hospcorrtest, nrow = 1, ncol = 2,align = "v", labels = c("a","b"),label_size = 15,rel_widths = c(6,4))




##### Supplementary Figure 6a. Number of staffed hospital beds in each state per 1000 individuals & COVID-19 deaths per 1000 
##### Supplementary Figure 6b. Spearman's Rank Correlation test between state wise availability of Hospital beds per 1000 and COVID-19 deaths per 1000 



#############################################################################################################################################


########## Vaccine Dosage Model building ##############

as.numeric(vaccine[is.na(vaccine)] <- 0)
second_booster <- (vaccine$Second_Booster_Janssen+vaccine$Second_Booster_Moderna+vaccine$Second_Booster_Pfizer)

vaccine_dosage <- data.frame(cbind(vaccine$Date,vaccine$Location,vaccine$Series_Complete_Yes,vaccine$Additional_Doses,
                                   second_booster,vaccine$Bivalent_Booster_12Plus))
colnames(vaccine_dosage) <- c('Date','State','Primary_Series','First_Booster','Second_Booster','Bivalent_Booster')

vax_dose <- subset(vaccine_dosage, Date %in% max(Date))
vax_dose <- vax_dose[order(vax_dose$State),]

vax_dose <- data.frame(cbind(state_name,vax_dose))
vax_dose <- vax_dose[order(state_name),]

statewise_model <- data.frame(cbind(cases_state,hdata_state$`Daily Hospitalization Count`,deaths_state$Deaths,
                              as.numeric(vax_dose$Primary_Series),
                              as.numeric(vax_dose$First_Booster),
                              as.numeric(vax_dose$Second_Booster),
                              as.numeric(vax_dose$Bivalent_Booster),
                              state_hesitant$`State Avg Vaccine hesitancy`,
                              statedeaths_icu$`Total Hosp Beds per 1000`))

colnames(statewise_model) <- c("State","Case","Hosp","Death",'Primary_Series','First_Booster','Second_Booster','Bivalent_Booster','Hesi', 'Hospbeds')
statewise_model$Hesi <- as.numeric(statewise_model$Hesi)

#ols_step_both_p (fitted.model_case, pent = 0.05, prem = 0.3, details = F)


fitted.model_case <- lm(Case ~ Primary_Series + First_Booster + Second_Booster + Bivalent_Booster + Hesi+ as.numeric(Hospbeds), data=statewise_model)
summary(fitted.model_case)
final.model_case <- step(fitted.model_case,direction="both")
summary(final.model_case) 
anova(fitted.model_case)

fitted.model_death <- lm(Death ~ Primary_Series + First_Booster + Second_Booster + Bivalent_Booster + Hesi + as.numeric(Hospbeds), data=statewise_model)
summary(fitted.model_death)
final.model_death <- step(fitted.model_death,direction="both")
summary(final.model_death)
anova(fitted.model_death)

fitted.model_hosp <- lm(Hosp ~ Primary_Series + First_Booster + Second_Booster + Bivalent_Booster + Hesi + as.numeric(Hospbeds), data=statewise_model)
summary(fitted.model_hosp)
final.model_hosp <- step(fitted.model_hosp,direction="both")
summary(final.model_hosp)
anova(fitted.model_hosp)

## Hospbeds vs Deaths model (FIGURE 8)
fitted.model_hospbed <- lm(Death ~ Primary_Series + First_Booster + Second_Booster + Bivalent_Booster + as.numeric(Hospbeds), data=statewise_model)
summary(fitted.model_hospbed)
final.model_hospbed <- step(fitted.model_hospbed,direction="both")
summary(final.model_hospbed)

#############################################################################################################################################

##### ANOVA test for partial F-test between Reduced (vaccination as independent variable) and 
##### Full Model (vaccination and vaccine hesitancy as independent variables) & 
##### ANOVA test for Reduced model; Hospitalizations, Cases and Deaths as response variables respectively

cases_state <- data.frame(cbind(state_name,rowSums(cases2)))
colnames(cases_state) <- c("State","Cases") 
cases_state <- cases_state[order(cases_state$State),]

deaths_state <- data.frame(cbind(state_name,rowSums(deaths2)))
colnames(deaths_state) <- c("State","Deaths") 
deaths_state <- deaths_state[order(deaths_state$State),]
statewise <- NULL
statewise <- data.frame(cbind(hdata_state$State, vaccine_state$cumulative_vaxrate, 
                              state_hesitant$`State Avg Vaccine hesitancy`,
                              hdata_state$`Daily Hospitalization Rate`,
                              (as.numeric(cases_state$Cases)/censusstate$POPESTIMATE042020)*1000,
                              (as.numeric(deaths_state$Deaths)/censusstate$POPESTIMATE042020)*1000))
colnames(statewise) <- c("State","Vax","Hesi","Hosp","Case","Death")

#write.csv(statewise,file='/Users/DB/Library/CloudStorage/GoogleDrive-bajracharyadeewan@outlook.com/My Drive/PHDclasses/research COV/R files/variant and hosp DATA/automatic_coder/stateoutcome.csv',col.names=TRUE)


## Hospitalization as response variable
FullModel_Hosp <- lm(formula =  as.numeric(Hosp)  ~ as.numeric(Vax) + as.numeric(Hesi), data = statewise)
ReducedModel_Hosp <- lm(formula =  as.numeric(Hosp)  ~ as.numeric(Vax), data = statewise)
## ANOVA for partial F-test
anova(ReducedModel_Hosp,FullModel_Hosp)


## Cases as response variable
FullModel_Case <- lm(formula =  as.numeric(Case)  ~ as.numeric(Vax) + as.numeric(Hesi), data = statewise)
ReducedModel_Case <- lm(formula =  as.numeric(Case)  ~ as.numeric(Vax), data = statewise)
## ANOVA for partial F-test
anova(ReducedModel_Case,FullModel_Case)


## Death as response variable
FullModel_Death <- lm(formula =  as.numeric(Death)  ~ as.numeric(Vax) + as.numeric(Hesi), data = statewise)
ReducedModel_Death <- lm(formula =  as.numeric(Death)  ~ as.numeric(Vax), data = statewise)

## ANOVA for partial F-test
anova(ReducedModel_Death,FullModel_Death)




#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################

###### CASES AND DEATHS AND HOSPS BEFORE AND AFTER OMICRON AND DELTA
hosp_total <- data.frame(colSums(hosp1))
hosp_total <- cbind(state_name,hosp_total)
hosp_total <- hosp_total[order(hosp_total$state_name),]
colnames(hosp_total) <- c('State','Hosps')
hosp_total <- cbind(hosp_total,(hosp_total$`Hosps`/censusage$Total)*1000)
colnames(hosp_total) <- c('State','Hosps','Hosps per 1000')

## Post VOCs
## Delta VOC detected in December 2020
## Omicron VOC detected in November 2021
cases_delta <- subset(cases1, as.Date(rownames(cases1), format = "%Y-%m-%d") > "2020-12-1" &
                              as.Date(rownames(cases1), format = "%Y-%m-%d") < "2021-11-26")
deaths_delta <- subset(deaths1, as.Date(rownames(cases1), format = "%Y-%m-%d") > "2020-12-1" &
                                as.Date(rownames(cases1), format = "%Y-%m-%d") < "2021-11-26")  


cases_omicron <- subset(cases1, as.Date(rownames(cases1), format = "%Y-%m-%d") > "2021-11-26")
deaths_omicron <- subset(deaths1, as.Date(rownames(deaths1), format = "%Y-%m-%d") > "2021-11-26")

hosps_delta <- subset(hosp1, as.Date(rownames(hosp1), format = "%Y-%m-%d") > "2020-12-1" &
                        as.Date(rownames(hosp1), format = "%Y-%m-%d") < "2021-11-26")
hosps_omicron <- subset(hosp1, as.Date(rownames(hosp1), format = "%Y-%m-%d") > "2021-11-26")


case_total_delta <- data.frame(colSums(cases_delta))
case_total_delta <- cbind(state_name,case_total_delta)
case_total_delta <- case_total_delta[order(case_total_delta$state_name),]
colnames(case_total_delta) <- c('State','Delta_cases')
case_total_delta <- cbind(case_total_delta,(case_total_delta$`Delta_cases`/censusage$Total)*1000)
colnames(case_total_delta) <- c('State','Delta_cases','Delta_cases_per_1000')

case_total_omicron <- data.frame(colSums(cases_omicron))
case_total_omicron <- cbind(state_name,case_total_omicron)
case_total_omicron <- case_total_omicron[order(case_total_omicron$state_name),]
colnames(case_total_omicron) <- c('State','Omicron_cases')
case_total_omicron <- cbind(case_total_omicron,(case_total_omicron$`Omicron_cases`/censusage$Total)*1000)
colnames(case_total_omicron) <- c('State','Omicron_cases','Omicron_cases_per_1000')


death_total_delta <- data.frame(colSums(deaths_delta))
death_total_delta <- cbind(state_name,death_total_delta)
death_total_delta <- death_total_delta[order(death_total_delta$state_name),]
colnames(death_total_delta) <- c('State','Delta_deaths')
death_total_delta <- cbind(death_total_delta,(death_total_delta$`Delta_deaths`/censusage$Total)*1000)
colnames(death_total_delta) <- c('State','Delta_deaths','Delta_deaths_per_1000')

death_total_omicron <- data.frame(colSums(deaths_omicron))
death_total_omicron <- cbind(state_name,death_total_omicron)
death_total_omicron <- death_total_omicron[order(death_total_omicron$state_name),]
colnames(death_total_omicron) <- c('State','Omicron_deaths')
death_total_omicron <- cbind(death_total_omicron,(death_total_omicron$`Omicron_deaths`/censusage$Total)*1000)
colnames(death_total_omicron) <- c('State','Omicron_deaths','Omicron_deaths_per_1000')


hosp_total_delta <- data.frame(colSums(hosps_delta))
hosp_total_delta <- cbind(state_name,hosp_total_delta)
hosp_total_delta <- hosp_total_delta[order(hosp_total_delta$state_name),]
colnames(hosp_total_delta) <- c('State','Delta_hosps')
hosp_total_delta <- cbind(hosp_total_delta,(hosp_total_delta$`Delta_hosps`/censusage$Total)*1000)
colnames(hosp_total_delta) <- c('State','Delta_hosps','Delta_hosps_per_1000')

hosp_total_omicron <- data.frame(colSums(hosps_omicron))
hosp_total_omicron <- cbind(state_name,hosp_total_omicron)
hosp_total_omicron <- hosp_total_omicron[order(hosp_total_omicron$state_name),]
colnames(hosp_total_omicron) <- c('State','Omicron_hosps')
hosp_total_omicron <- cbind(hosp_total_omicron,(hosp_total_omicron$`Omicron_hosps`/censusage$Total)*1000)
colnames(hosp_total_omicron) <- c('State','Omicron_hosps','Omicron_hosps_per_1000')


cases_outcomes <- as.data.frame(cbind(States=case_total_delta$State,
                                      Delta_cases=case_total_delta$Delta_cases_per_1000,
                                      Omicron_cases=case_total_omicron$Omicron_cases_per_1000,
                                      Total_cases=case_total$`Cases per 1000`))

deaths_outcomes <- as.data.frame(cbind(States=death_total_delta$State,
                                      Delta_deaths=death_total_delta$Delta_deaths_per_1000,
                                      Omicron_deaths=death_total_omicron$Omicron_deaths_per_1000,
                                      Total_deaths=death_total$`Deaths per 1000`))

hosps_outcomes <- as.data.frame(cbind(States=hosp_total_delta$State,
                                       Delta_hosps=hosp_total_delta$Delta_hosps_per_1000,
                                       Omicron_hosps=hosp_total_omicron$Omicron_hosps_per_1000,
                                       Total_hosps=hosp_total$`Hosps per 1000`))

                                        
cases_outcomes$other_VOCs_cases <- as.numeric(cases_outcomes$Total_cases) - 
                                            as.numeric(cases_outcomes$Delta_cases) - 
                                            as.numeric(cases_outcomes$Omicron_cases)   
# Convert variables to numeric
cases_outcomes$Delta_cases <- as.numeric(cases_outcomes$Delta_cases)
cases_outcomes$Omicron_cases <- as.numeric(cases_outcomes$Omicron_cases)
cases_outcomes$other_VOCs_cases <- as.numeric(cases_outcomes$other_VOCs_cases)


deaths_outcomes$other_VOCs_deaths <- as.numeric(deaths_outcomes$Total_deaths) - 
                                            as.numeric(deaths_outcomes$Delta_deaths) - 
                                            as.numeric(deaths_outcomes$Omicron_deaths)
# Convert variables to numeric
deaths_outcomes$Delta_deaths <- as.numeric(deaths_outcomes$Delta_deaths)
deaths_outcomes$Omicron_deaths <- as.numeric(deaths_outcomes$Omicron_deaths)
deaths_outcomes$other_VOCs_deaths <- as.numeric(deaths_outcomes$other_VOCs_deaths)


hosps_outcomes$other_VOCs_hosps <- as.numeric(hosps_outcomes$Total_hosps) - 
  as.numeric(hosps_outcomes$Delta_hosps) - 
  as.numeric(hosps_outcomes$Omicron_hosps)   
# Convert variables to numeric
hosps_outcomes$Delta_hosps <- as.numeric(hosps_outcomes$Delta_hosps)
hosps_outcomes$Omicron_hosps <- as.numeric(hosps_outcomes$Omicron_hosps)
hosps_outcomes$other_VOCs_hosps <- as.numeric(hosps_outcomes$other_VOCs_hosps)

library(tidyverse)
library(tidytext)
# Reshape the data to a long format
cases_outcomes_long <- cases_outcomes %>%
  gather(key = "Variant", value = "Cases", other_VOCs_cases, Delta_cases, Omicron_cases)

# Match the Total_cases values for each state
cases_outcomes_long$Total_cases <- cases_outcomes$Total_cases[match(cases_outcomes_long$States, cases_outcomes$States)]

# Calculate the percentage of Total_cases for each Variant within each State
cases_outcomes_long <- cases_outcomes_long %>%
  group_by(States) %>%
  mutate(Percentage = Cases / sum(Cases) * 100)

# Create the stacked bar plot with states ordered by total cases
ggCASE <- ggplot(cases_outcomes_long, aes(x = reorder(States, as.numeric(Total_cases)), y = Cases, fill = Variant)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1))), position = position_stack(vjust = 0.5), angle = -90, size = 6, alpha = 0.8) +
  ylab("Cases per 1000") +
  xlab('States') +
  ggtitle("Case Rates for SARS-CoV-2 VOCs by State") +
  scale_fill_manual(values = c("Omicron_cases" = "firebrick2", "Delta_cases" = "dodgerblue", "other_VOCs_cases" = "limegreen")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0, size=16),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5, face ="bold", size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 14))


ggCASE


# Reshape the data to a long format
deaths_outcomes_long <- deaths_outcomes %>%
  gather(key = "Variant", value = "Deaths", other_VOCs_deaths, Delta_deaths, Omicron_deaths)

# Match the Total_deaths values for each state
deaths_outcomes_long$Total_deaths <- deaths_outcomes$Total_deaths[match(deaths_outcomes_long$States, deaths_outcomes$States)]
deaths_outcomes_long$Deaths <- as.numeric(deaths_outcomes_long$Deaths)
# Create a sequence from min to max with the defined separation
breaks <- seq(from = 0, 
              to = 5, by = 0.5)

# Calculate the percentage of Total_deaths for each Variant within each State
deaths_outcomes_long <- deaths_outcomes_long %>%
  group_by(States) %>%
  mutate(Percentage = Deaths / sum(Deaths) * 100)

# Create the stacked bar plot with states ordered by total deaths
ggDEATH <- ggplot(deaths_outcomes_long, aes(x = reorder(States, as.numeric(Total_deaths)), y = Deaths, fill = Variant)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1))), position = position_stack(vjust = 0.5), angle = -90, size = 6, alpha = 0.8) +
  ylab("Deaths per 1000") +
  xlab('States') +
  ggtitle("Death Rates for SARS-CoV-2 VOCs by State") +
  scale_fill_manual(values = c("Omicron_deaths" = "firebrick2", "Delta_deaths" = "dodgerblue", "other_VOCs_deaths" = "limegreen")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0, size=16),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5, face ="bold", size = 16),
        legend.position = "bottom",
        legend.text = element_text(size = 14))

ggDEATH



# Reshape the data to a long format
hosps_outcomes_long <- hosps_outcomes %>%
  gather(key = "Variant", value = "Hosps", other_VOCs_hosps, Delta_hosps, Omicron_hosps)

# Match the Total_cases values for each state
hosps_outcomes_long$Total_hosps <- hosps_outcomes$Total_hosps[match(hosps_outcomes_long$States, hosps_outcomes$States)]

# Calculate the percentage of Total_hosps for each Variant within each State
hosps_outcomes_long <- hosps_outcomes_long %>%
  group_by(States) %>%
  mutate(Percentage = Hosps / sum(Hosps) * 100)

# Create the stacked bar plot with states ordered by total hosps
ggHOSP <- ggplot(hosps_outcomes_long, aes(x = reorder(States, as.numeric(Total_hosps)), y = Hosps, fill = Variant)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  geom_text(aes(label = paste0(round(Percentage, 1))), position = position_stack(vjust = 0.5), angle = 30, size = 4, alpha = 0.8) +
  ylab("Hospitalizations per 1000") +
  xlab('States') +
  ggtitle("Hospitalization Rates for SARS-CoV-2 VOCs by State") +
  scale_fill_manual(values = c("Omicron_hosps" = "firebrick2", "Delta_hosps" = "dodgerblue", "other_VOCs_hosps" = "limegreen")) +
  theme(axis.text.x=element_text(angle=-90,hjust=0,size=14),
        axis.title.x=element_text(size=14, face="bold"),
        axis.title.y=element_text(size=14, face="bold"),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray90", size = 0.5),
        panel.grid.major.y = element_line(color = "gray90", size = 0.5),
        panel.grid.minor.y = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(hjust = 0.5, face ="bold", size = 14),
        legend.position = "bottom")

ggHOSP



# Calculate the average percentage for each variant

cases_outcomes_long %>%
  group_by(Variant) %>%
  summarise(
    Average_Percentage = mean(Percentage)
  )

deaths_outcomes_long %>%
  group_by(Variant) %>%
  summarise(
    Average_Percentage = mean(Percentage)
  )

hosps_outcomes_long %>%
  group_by(Variant) %>%
  summarise(
    Average_Percentage = mean(Percentage)
  )


#plot_grid(ggCASE,ggDEATH, nrow = 2, ncol = 1,align = "v", labels = c("a","b"),label_size = 18)






#save.image("Manuscript1.Rdata")






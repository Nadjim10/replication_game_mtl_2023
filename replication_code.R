#install.packages("tidyverse")
#install.packages("stargazer")
library(tidyverse) ##data cleaning
library(stargazer) ##tex output
library(haven)
library(estimatr)
library(dplyr)
library(fixest)
library(modelsummary)


############################################
############## CREATING FIGURE 1 ###########
############################################

#Load in data
load("dyadic_data_1-4-22.Rdata")
dta <- updated_data 

#### Remove unneeded variables
vars <- c("to_mp_number", "to_rile", "to_economy", "to_society", "year", "country", 
          "to_pfeml", "to_femaleleader")
dta <- dta[vars]
dta <- na.omit(dta)

### Identiy unique parties being evaluated
dta_unique <- unique(dta)


fig1 <- ggplot(dta_unique, aes(x = to_pfeml)) +
  geom_histogram(color="black", fill="grey40", binwidth =0.1, center=0.25) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  theme_minimal() +
  theme(plot.title = element_text(size=12)) +
  ylab("Frequency")+
  xlab("Proportion of Women MPs");fig1


############################################
###### CREATING TABLE 1 COLUMNS 1 & 2 ######
############################################

#Out party % women, non-clustered SEs
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

#creating the country-year fixed effects
dta$cntryyr <-paste(dta$country, dta$year, sep = "")

## Removing smaller parties
dta <- subset(dta, dta$to_prior_seats >=4)


vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike", "party_like", "cntryyr", "to_pfeml", "to_prior_seats", "to_mp_number")
dta <- dta[vars]
dta <- na.omit(dta)


table1.1 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta)
table1.2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = dta)

### With clustered SEs
stargazer(table1.1, table1.2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.1, table1.2,
                        clusters = dta$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
###### CREATING TABLE 1 COLUMNS 3 & 4 ######
############################################

## Note in gendered data, the party_like and party_dislike variable indicate mean levels of
## like/dislike for party by ALL partisans
## the "dislike" variable indicates level of dislike towards out-party by partisans of specified gender
dta <- readRDS("gender_disagregated_8-8-21.rds")

#creating the country-year fixed effects
dta$countryyear <-paste(dta$country, dta$year, sep = "")

## Removing smaller parties
dta <- subset(dta, dta$to_prior_seats >=4)

### Create Like variable for gendered data from dislike
dta$like <- 10- dta$dislike

## Remove unneeded variables and NAs
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "to_pfeml",
          "countryyear", "gender", "like", "dislike", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

## Only men subset
dta_male <- subset(dta, gender==1)
dta_female <- subset(dta, gender==2)

table1.3 <-lm(like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_female)
table1.4 <-lm(like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_male)

### With clustered SEs - women
stargazer(table1.3,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.3,
                        clusters = dta_female$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", "to_pfeml2"))

### With clustered SEs - men
stargazer(table1.4, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.4, 
                        clusters = dta_male$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", "to_pfeml2"))


############################################
###### CREATING TABLE S2 COLUMNS 1 & 2 ######
############################################

#Out party % women, non-clustered SEs
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

#creating the country-year fixed effects
dta$cntryyr <-paste(dta$country, dta$year, sep = "")



vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike", "party_like", "cntryyr", "to_pfeml", "to_prior_seats", "to_mp_number")
dta <- dta[vars]
dta <- na.omit(dta)


tableS2.1 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta)
tableS2.2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = dta)

summary(tableS2.1)
summary(tableS2.2)

### With clustered SEs
stargazer(tableS2.1, tableS2.2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(tableS2.1, tableS2.2,
                        clusters = dta$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
###### CREATING TABLE S2 COLUMNS 3 & 4 ######
############################################

dta <- readRDS("gender_disagregated_8-8-21.rds")

#creating the country-year fixed effects
dta$countryyear <-paste(dta$country, dta$year, sep = "")


### Create Like variable for gendered data from dislike
dta$like <- 10- dta$dislike

## Remove unneeded variables and NAs
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "to_pfeml",
          "countryyear", "gender", "like", "dislike", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

## Only men subset
dta_male <- subset(dta, gender==1)
dta_female <- subset(dta, gender==2)

tableS2.3 <-lm(like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_female)
tableS2.4 <-lm(like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_male)

summary(tableS2.3)
summary(tableS2.4)

### With clustered SEs - women
stargazer(tableS2.3,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(tableS2.3,
                        clusters = dta_female$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", "to_pfeml2"))

### With clustered SEs - men
stargazer(tableS2.4, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(tableS2.4, 
                        clusters = dta_male$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", "to_pfeml2"))


############################################
############ CREATING TABLE S3 #############
############################################

## Read in data
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data
colnames(dta)

#creating the country-year fixed effects
dta$cntryyr <-paste(dta$country, dta$year, sep = "")
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike","party_like", "cntryyr", "to_pfeml", "from_rile", "to_rile",
          "from_left_bloc", "from_right_bloc", "to_left_bloc", "to_right_bloc", "from_parfam", "to_parfam",
          "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

dta_nrr <- subset(dta, dta$to_parfam!=70)
dta_nrr <- subset(dta_nrr, dta_nrr$from_parfam!=70)

## Remove small parties, with fewer than 4 seats
dta_small_nrr <- subset(dta_nrr, dta_nrr$to_prior_seats >=4)

table.S3 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(country), data = dta_small_nrr)
summary(table.S3)

stargazer(table.S3, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S3,
                        clusters = dta_small_nrr$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))


############################################
############ CREATING TABLE S3B ############
############################################

## Load
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

#creating the country-year fixed effects
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike", "party_like", "to_parfam", "to_left_bloc", "to_right_bloc", "cntryyr", "to_pfeml",
          "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

## Remove small parties, with fewer than 4 seats
dta_small <- subset(dta, dta$to_prior_seats >=4)

table.3B.1 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta_small)
table.3B.2 <-lm(party_like ~ to_pfeml + rile_distance_s + to_left_bloc + prior_coalition + prior_opposition + as.factor(cntryyr), data = dta_small)

summary(table.3B.2)

### With clustered SEs
stargazer(table.3B.1, table.3B.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.3B.1, table.3B.2,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "rile_distance_s", "to_left_bloc", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))


############################################
############ CREATING TABLE S4 ############
############################################

## Read in data
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

#creating the country-year fixed effects
dta$countryyear <-paste(dta$country, dta$year, sep = "")
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike","party_like", "countryyear", "to_pfeml", "from_rile", "to_rile",
          "logDM", "to_left_bloc", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

### Split by year, 1996-2006 and 2007-2017
dta_early <- subset(dta, dta$year<=2006)
dta_late <- subset(dta, dta$year>=2007)


table.early <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_early)
table.late <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition  + as.factor(countryyear), data = dta_late)

### Without small parties
dta_early_small <- subset(dta_early, dta_early$to_prior_seats >=4)
dta_late_small <- subset(dta_late, dta_late$to_prior_seats >=4)

table.4.1 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_early_small)
table.4.2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition  + as.factor(countryyear), data = dta_late_small)

summary(table.4.1)
summary(table.4.2)

### With clustered SEs
stargazer(table.4.1,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.4.1,
                        clusters = dta_early_small$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition"
          ))

### With clustered SEs
stargazer(table.4.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.4.2,
                        clusters = dta_late_small$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition"
          ))



############################################
############ CREATING TABLE S5 #############
############################################

load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

#creating the country-year fixed effects
dta$countryyear <-paste(dta$country, dta$year, sep = "")
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike", "party_like", "countryyear", 
          "to_pfeml", "from_pfeml", "diff_pfeml", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

## Remove small parties, with fewer than 4 seats
dta_small <- subset(dta, dta$to_prior_seats >=4)

table.S5.1 <-lm(party_like ~ to_pfeml + from_pfeml + diff_pfeml + as.factor(countryyear), data = dta_small)
table.S5.2 <-lm(party_like ~ to_pfeml + from_pfeml + diff_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_small)

summary(table.S5.1)
summary(table.S5.2)

### With clustered SEs
stargazer(table.S5.1, table.S5.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S5.1,  table.S5.2,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "from_pfeml", "diff_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING FIG. S1 #############
############################################


### Create Plot Data

## All values of Out-Party % women
plot_1 <- as.data.frame((unique(dta$to_pfeml)))
colnames(plot_1) <- c("to_pfeml")

## All 1 Sd above mean of In-party % women
plot_1$from_pfeml <- mean(dta$from_pfeml, na.rm=T) + sd(dta$from_pfeml, na.rm=T)

## Create difference between in-and out-party women
plot_1$diff_pfeml <- abs(plot_1$to_pfeml - plot_1$from_pfeml)

## Select other values (mean RILE distance, opposition together, France 2012 country year)
plot_1$rile_distance_s <- mean(dta$rile_distance_s, na.rm=T)
plot_1$prior_coalition <- 0
plot_1$prior_opposition <- 1
plot_1$countryyear <- "France2012"
plot_1$to_mp_number <- "31320"
plot_1$group <- "above_mean"

## All values of Out-Party % women
plot_2 <- as.data.frame((unique(dta$to_pfeml)))
colnames(plot_2) <- c("to_pfeml")

## All 1 Sd below mean of In-party % women
plot_2$from_pfeml <- mean(dta$from_pfeml, na.rm=T) - sd(dta$from_pfeml, na.rm=T)

## Create difference between in-and out-party women
plot_2$diff_pfeml <- abs(plot_2$to_pfeml - plot_2$from_pfeml)

## Select other values (opposition together, France 2012 country year)
plot_2$rile_distance_s <- mean(dta$rile_distance_s, na.rm=T)
plot_2$prior_coalition <- 0
plot_2$prior_opposition <- 1
plot_2$countryyear <- "France2012"
plot_2$to_mp_number <- "31320"
plot_2$group <- "below_mean"

plot_dta <- rbind(plot_1, plot_2)


###### Plot based on table.S5.2
figureS1.data <- as.data.frame(predict(table.S5.2, newdata = plot_dta, interval = "confidence"))

plot_dta$fit <- figureS1.data$fit
plot_dta$lwr <- figureS1.data$lwr
plot_dta$upr <- figureS1.data$upr

figS1 <- ggplot(plot_dta, aes(x=to_pfeml, y=fit, lty=group))
figS1 <- figS1 + geom_line() +
  geom_ribbon(aes(x = to_pfeml, y = fit, ymin = lwr,
                  ymax = upr),
              lwd = 1/2, alpha=0.1) +
  theme_minimal() + 
  theme(plot.title = element_text(size=12)) +
  ylab("Predicted Out-Party Thermometer Rating")+
  xlab("Proportion of Out-Party Women MPs") + 
  theme(legend.position = "none") + 
  geom_text(x=0.70, y=5.3, label="in-party % of women is \n1 SD above the mean") +
  geom_text(x=0.70, y=4.0, label="in-party % of women is \n1 SD below the mean", color="grey37") +
  ylim(c(2.5,6.5));figS1

pdf("figS1.pdf")
figS1
dev.off()


############################################
############ CREATING TABLE S6 #############
############################################

## Read in data
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

#creating the country-year fixed effects
dta$countryyear <-paste(dta$country, dta$year, sep = "")
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike","party_like", "countryyear", "to_pfeml", "from_rile", "to_rile",
          "logDM", "to_left_bloc", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

dta_small <- subset(dta, dta$to_prior_seats >=4)

table.S6.1 <-lm(party_like ~ to_pfeml + rile_distance_s + logDM + prior_coalition + prior_opposition  + as.factor(year), data = dta_small)
table.S6.2 <-lm(party_like ~ to_pfeml*logDM + rile_distance_s + prior_coalition + prior_opposition  + as.factor(year), data = dta_small)
table.S6.3 <-lm(party_like ~ to_pfeml*logDM + rile_distance_s*logDM + prior_coalition*logDM + prior_opposition*logDM  + as.factor(year), data = dta_small)

summary(table.S6.1)
summary(table.S6.2)
summary(table.S6.3)

stargazer(table.S6.1, table.S6.2, table.S6.3, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S6.1, table.S6.2, table.S6.3,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "rile_distance_s", "logDM", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))


############################################
############ CREATING TABLE S7 #############
############################################

#Out party % women, non-clustered SEs
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

#creating the country-year fixed effects
dta$cntryyr <-paste(dta$country, dta$year, sep = "")
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike", "party_like", "cntryyr", "to_pfeml", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

## Creating squared term for out-party % women
dta$to_pfeml2 <- dta$to_pfeml^2

## Remove small parties, with fewer than 4 seats
dta <- subset(dta, dta$to_prior_seats >=4)

table.S7.1 <-lm(party_like ~ to_pfeml + to_pfeml2 + as.factor(cntryyr), data = dta)
table.S7.2 <-lm(party_like ~ to_pfeml + to_pfeml2 + rile_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = dta)

summary(table.S7.1)
summary(table.S7.2)

### With clustered SEs
stargazer(table.S7.1, table.S7.2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S7.1, table.S7.2,
                        clusters = dta$country),
          keep = c("to_pfeml", "to_pfeml2", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING FIG. S2 #############
############################################

### Create Plot Data

## All values of Out-Party % women
plot_S2 <- as.data.frame((unique(dta$to_pfeml)))
colnames(plot_S2) <- c("to_pfeml")

## Create difference between in-and out-party women
plot_S2$to_pfeml2 <- plot_S2$to_pfeml^2

## Select other values (mean RILE distance, opposition together, France 2012 country year)
plot_S2$rile_distance_s <- mean(dta$rile_distance_s, na.rm=T)
plot_S2$prior_coalition <- 0
plot_S2$prior_opposition <- 1
plot_S2$cntryyr <- "France2012"
plot_S2$to_mp_number <- "31320"

figureS2.data <- as.data.frame(predict(table.S7.2, newdata = plot_S2, interval = "confidence"))

plot_S2$fit <- figureS2.data$fit
plot_S2$lwr <- figureS2.data$lwr
plot_S2$upr <- figureS2.data$upr

figS2 <- ggplot(plot_S2, aes(x=to_pfeml, y=fit))
figS2 <- figS2 + geom_line() +
  geom_ribbon(aes(x = to_pfeml, y = fit, ymin = lwr,
                  ymax = upr),
              lwd = 1/2, alpha=0.1) +
  theme_minimal() + 
  theme(plot.title = element_text(size=12)) +
  ylab("Predicted Out-Party Thermometer Rating")+
  xlab("Proportion of Out-Party Women MPs") + 
  ylim(c(2,5));figS2

pdf("figS2.pdf")
figS2
dev.off()

############################################
############ CREATING TABLE S8 #############
############################################

#Women-led parties
load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

dta_womenlead <- subset(dta, dta$to_femaleleader==1)

#creating the country-year fixed effects
dta_womenlead$countryyear <-paste(dta_womenlead$country, dta_womenlead$year, sep = "")
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike","party_like", "countryyear", "to_pfeml", "to_prior_seats")
dta_womenlead <- dta_womenlead[vars]
dta_womenlead <- na.omit(dta_womenlead)

## Exclude small parties
dta_womenlead <- subset(dta_womenlead, dta_womenlead$to_prior_seats >=4)

table.S8A1 <-lm(party_like ~ to_pfeml + as.factor(countryyear), data = dta_womenlead)
table.S8A2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_womenlead)

summary(table.S8A1)
summary(table.S8A2)

### With clustered SEs
stargazer(table.S8A1, table.S8A2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S8A1, table.S8A2,
                        clusters = dta_womenlead$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

#Male-led parties
load("dyadic_data_1-4-22.Rdata")
dta <-updated_data

dta_malelead <- subset(dta, dta$to_femaleleader==0)

#creating the country-year fixed effects
dta_malelead$countryyear <-paste(dta_malelead$country, dta_malelead$year, sep = "")
vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike","party_like", "countryyear", "to_pfeml", "to_prior_seats")
dta_malelead <- dta_malelead[vars]
dta_malelead <- na.omit(dta_malelead)

## Exclude small parties
dta_malelead <- subset(dta_malelead, dta_malelead$to_prior_seats >=4)

table.S8B1 <-lm(party_like ~ to_pfeml + as.factor(countryyear), data = dta_malelead)
table.S8B2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_malelead)

summary(table.S8B1)
summary(table.S8B2)

### With clustered SEs
stargazer(table.S8B1, table.S8B2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S8B1, table.S8B2, 
                        clusters = dta_malelead$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING TABLE S9 #############
############################################

## Read in data
load("dyadic_data_1-4-22.Rdata")

dta <- updated_data

#creating the country-year fixed effects
dta$countryyear <-paste(dta$country, dta$year, sep = "")

#creating the party fixed effects / cluster
dta$partydyad <-paste(dta$from_mp_number, dta$to_mp_number, sep = "")

vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike","party_like", "countryyear", "to_pfeml", 
          "from_rile", "to_rile", "to_mp_number", "partydyad", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

## Exclude small parties
dta <- subset(dta, dta$to_prior_seats >=4)

table.S9 <-lm(party_like ~ to_pfeml  + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta)
summary(table.S9)

### With clustered SEs
stargazer(table.S9,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S9,
                        clusters = dta$partydyad),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))


############################################
############ CREATING TABLE 10 #############
############################################

load("dyadic_data_1-4-22.Rdata")

dta <-updated_data

vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country", "party_dislike", "to_pfeml", "party_like", "to_prior_seats")
dta <- dta[vars]
dta <- na.omit(dta)

## Remove small parties, with fewer than 4 seats
dta_small <- subset(dta, dta$to_prior_seats >=4)

table.S10.1 <-lm(party_like ~ to_pfeml + as.factor(country), data = dta_small)
table.S10.2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(country), data = dta_small)

summary(table.S10.2)

### With clustered SEs
stargazer(table.S10.1, table.S10.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S10.1, table.S10.2,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

##################################################
############ CREATING TABLES 11 & 12 #############
##################################################

load("Data/multilevel_1-5-22.Rdata") 

indiv_data <-multilevel_data


vars <- c("rile_distance_s", "prior_coalition", "prior_opposition", "econ_distance_s", "society_distance_s",
          "year", "country",  "cntryyr", "to_pfeml", "from_pfeml", "thermometer_score", "ID", "party_to", "party_from",
          "from_partyname", "to_partyname", "to_left_bloc", "from_left_bloc",
          "to_right_bloc", "from_right_bloc", "gender", "to_parfam", "from_parfam", "to_prior_seats",
          "from_mp_number", "to_mp_number")


indiv_data <- indiv_data[vars]

## Create gender variable
indiv_data <-mutate(indiv_data, gender = ifelse(gender == "1", "male",
                                                ifelse(gender == "2", "female", NA)))

indiv_data$gender <-as.factor(indiv_data$gender)

##filter out parties with no data, mainly parties who were not in parliament plus a few cases from early 1990s
indiv_data <-filter(indiv_data, is.na(to_pfeml) == F)

### Create dyads for FEs/Clustered SEs
indiv_data$dyad <-paste(indiv_data$from_mp_number, indiv_data$to_mp_number, sep ="_to_")

### Create Table 11, column 1, with Standard errors clustered at country-year, party-dyad, and individual levels
table11A.1.1 <-feols(thermometer_score ~ to_pfeml | ID, data = indiv_data, cluster = ~cntryyr)
table11A.1.2 <-feols(thermometer_score ~ to_pfeml | ID, data = indiv_data, cluster = ~dyad)
table11A.1.3 <-feols(thermometer_score ~ to_pfeml | ID, data = indiv_data, cluster = ~ID)

### Create Table 11, column 2, with Standard errors clustered at country-year, party-dyad, and individual levels
table11A.2.1 <-feols(thermometer_score ~ to_pfeml + + rile_distance_s + prior_coalition + prior_opposition | ID, data = indiv_data, cluster = ~cntryyr)
table11A.2.2 <-feols(thermometer_score ~ to_pfeml + + rile_distance_s + prior_coalition + prior_opposition | ID, data = indiv_data, cluster = ~dyad)
table11A.2.3 <-feols(thermometer_score ~ to_pfeml + + rile_distance_s + prior_coalition + prior_opposition | ID, data = indiv_data, cluster = ~ID)

### Create Table 11B, column 1, with Standard errors clustered at country-year, party-dyad, and individual levels
table11B.1.1 <-feols(thermometer_score ~ to_pfeml | cntryyr, data = indiv_data, cluster = ~cntryyr)
table11B.1.2 <-feols(thermometer_score ~ to_pfeml | cntryyr, data = indiv_data, cluster = ~dyad)
table11B.1.3 <-feols(thermometer_score ~ to_pfeml | cntryyr, data = indiv_data, cluster = ~ID)

### Create Table 11B, column 2, with Standard errors clustered at country-year, party-dyad, and individual levels
table11B.2.1 <-feols(thermometer_score ~ to_pfeml + + rile_distance_s + prior_coalition + prior_opposition | cntryyr, data = indiv_data, cluster = ~cntryyr)
table11B.2.2 <-feols(thermometer_score ~ to_pfeml + + rile_distance_s + prior_coalition + prior_opposition | cntryyr, data = indiv_data, cluster = ~dyad)
table11B.2.3 <-feols(thermometer_score ~ to_pfeml + + rile_distance_s + prior_coalition + prior_opposition | cntryyr, data = indiv_data, cluster = ~ID)


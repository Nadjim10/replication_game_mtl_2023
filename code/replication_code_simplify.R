########################################################### Replication of the article Can’t We All Just Get Along? ###########################################################

#### Packages ####

Packages <- c("tidyverse", "stargazer", "fixest", "modelsummary", "haven", "estimatr")
lapply(Packages, library, character.only = T)
options(tibble.print_max = Inf,scipen=999)

############################################
############## CREATING FIGURE 1 ###########
############################################

load("_data/dyadic_data_1-4-22.Rdata") # Load data 

dta_unique <- updated_data |> 
  select(country, year, to_mp_number, to_rile, to_economy, to_society, to_femaleleader, to_pfeml) |> # Remove unneeded variables
  na.omit() |> 
  distinct() # Keep only the unique parties being evaluated

# Graph 1 

ggplot(dta_unique, aes(x = to_pfeml)) +
  geom_histogram(color="black", fill="grey40", binwidth =0.1, center=0.25) +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  theme_minimal() +
  theme(plot.title = element_text(size=12)) +
  ylab("Frequency")+
  xlab("Proportion of Women MPs")
ggsave("_graphs/fig1.pdf", width = 12, height = 10)


############################################
###### CREATING TABLE 1 COLUMNS 1 & 2 ######
############################################

dta <- updated_data |> 
  mutate(cntryyr = paste(country, year, sep = "")) |> # creating the country-year fixed effects
  filter(to_prior_seats >= 4) |>  # Removing smaller parties
  select(year, country, rile_distance_s, prior_coalition, prior_opposition, 
         econ_distance_s, society_distance_s, party_dislike, party_like, cntryyr,
         to_pfeml, to_prior_seats, to_mp_number) |> # selecting variables
  na.omit()

table1.1 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta)
table1.2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = dta)

# With clustered ses
stargazer(type = "text", table1.1, table1.2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.1, table1.2,
                        clusters = dta$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
###### CREATING TABLE 1 COLUMNS 3 & 4 ######
############################################

# Note in gendered data, the party_like and party_dislike variable indicate mean levels of
# like/dislike for party by ALL partisans
# the "dislike" variable indicates level of dislike towards out-party by partisans of specified gender
  
dta <- readRDS("_data/gender_disagregated_8-8-21.rds") |> 
  mutate(countryyear = paste(country, dta$year, sep = "")) |>  #creating the country-year fixed effects
  filter(to_prior_seats >= 4) |> # Removing smaller parties
  mutate(like = 10-dislike) |> # Create Like variable for gendered data from dislike
  select(year, country, rile_distance_s, prior_coalition, prior_opposition, 
         econ_distance_s, society_distance_s, to_pfeml, 
         countryyear, gender, like, dislike, to_prior_seats) |> 
  na.omit() 

# Only men subset
dta_male   <- dta |> filter(gender == 1)
dta_female <- dta |> filter(gender == 2)

tableS2.3 <-lm(like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_female)
tableS2.4 <-lm(like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_male)

summary(tableS2.3)
summary(tableS2.4)

# With clustered ses - women
stargazer(type = "text", tableS2.3,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered ses?", "Yes")),
          se = starprep(tableS2.3,
                        clusters = dta_female$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", "to_pfeml2"))

# With clustered ses - men
stargazer(type = "text", tableS2.4, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered ses?", "Yes")),
          se = starprep(tableS2.4, 
                        clusters = dta_male$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", "to_pfeml2"))

############################################
############ CREATING TABLE S3 #############
############################################

load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  mutate(cntryyr = paste(country, year, sep = "")) |> # creating the country-year fixed effects
  select(year, country, rile_distance_s, prior_coalition, prior_opposition, 
         econ_distance_s, society_distance_s, party_dislike,party_like, cntryyr, 
         to_pfeml, from_rile, to_rile, from_left_bloc, from_right_bloc, to_left_bloc, 
         to_right_bloc, from_parfam, to_parfam, to_prior_seats) |> 
  na.omit()

dta_nrr <- dta |> filter(to_parfam != 70) 
dta_nrr <- dta_nrr |> filter(from_parfam != 70)

# Remove small parties, with fewer than 4 seats

dta_small_nrr <- dta_nrr |> filter(to_prior_seats >= 4)

table.S3 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(country), data = dta_small_nrr)
summary(table.S3)

stargazer(type = "text", table.S3, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered ses?", "Yes")),
          se = starprep(table.S3,
                        clusters = dta_small_nrr$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING TABLE S3B ############
############################################

# Load
load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         party_dislike, party_like, to_parfam, to_left_bloc, 
         to_right_bloc, cntryyr, to_pfeml, to_prior_seats) |> 
  na.omit() 
  
# Remove small parties, with fewer than 4 seats
dta_small <- dta |> filter(to_prior_seats >= 4)

table.3B.1 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta_small)
table.3B.2 <-lm(party_like ~ to_pfeml + rile_distance_s + to_left_bloc + prior_coalition + prior_opposition + as.factor(cntryyr), data = dta_small)
summary(table.3B.2)

# With clustered ses

stargazer(type = "text", table.3B.1, table.3B.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.3B.1, table.3B.2,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "rile_distance_s", "to_left_bloc", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING TABLE S4 ############
############################################

# Read in data
load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  mutate(countryyear = paste(country, year, sep = "")) |> # creating the country-year fixed effects
  select(year, country, rile_distance_s, prior_coalition, prior_opposition, 
         econ_distance_s, society_distance_s, party_dislike, party_like, 
         countryyear, to_pfeml, from_rile, to_rile, logDM, to_left_bloc, to_prior_seats) |> 
  na.omit()

# Split by year, 1996-2006 and 2007-2017
dta_early <- dta |> filter(year <= 2006) 
dta_late <- dta |> filter(year >= 2007) 

table.early <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_early)
table.late <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition  + as.factor(countryyear), data = dta_late)

# Without small parties
dta_early_small <- dta_early |> filter(to_prior_seats >= 4) 
dta_late_small <- dta_late |> filter(to_prior_seats >= 4) 

table.4.1 <- lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_early_small)
table.4.2 <- lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition  + as.factor(countryyear), data = dta_late_small)

summary(table.4.1)
summary(table.4.2)

# With clustered ses
stargazer(type = "text", table.4.1,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered ses?", "Yes")),
          se = starprep(table.4.1,
                        clusters = dta_early_small$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition"
          ))

# With clustered ses

stargazer(type = "text", table.4.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.4.2,
                        clusters = dta_late_small$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition"
          ))

############################################
############ CREATING TABLE S5 #############
############################################

load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  mutate(countryyear = paste(country, year, sep = "")) |> # creating the country-year fixed effects
  select(year, country, rile_distance_s, prior_coalition, prior_opposition, 
         econ_distance_s, society_distance_s, party_dislike, party_like, countryyear, 
         to_pfeml, from_pfeml, diff_pfeml, to_prior_seats) |> 
  na.omit()

# Remove small parties, with fewer than 4 seats
dta_small <- dta |> filter(to_prior_seats >= 4) 

table.S5.1 <-lm(party_like ~ to_pfeml + from_pfeml + diff_pfeml + as.factor(countryyear), data = dta_small)
table.S5.2 <-lm(party_like ~ to_pfeml + from_pfeml + diff_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_small)

summary(table.S5.1)
summary(table.S5.2)

# With clustered ses
stargazer(type = "text", table.S5.1, table.S5.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S5.1,  table.S5.2,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "from_pfeml", "diff_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING FIG. S1 #############
############################################

# All values of Out-Party % women

plot_1 <- dta |> 
  distinct(to_pfeml) |> 
  mutate(from_pfeml = mean(dta$from_pfeml, na.rm=T) + sd(dta$from_pfeml, na.rm=T), # All 1 Sd above mean of in-party % women
         diff_pfeml = abs(to_pfeml - from_pfeml),  # Create difference between in-and out-party women
         # Select other values (mean RILE distance, opposition together, France 2012 country year)
         rile_distance_s = mean(dta$rile_distance_s, na.rm=T),
         prior_coalition = 0,
         prior_opposition = 1,
         countryyear = "France2012",
         to_mp_number = "31320",
         group = "above_mean"
         )

# All values of Out-Party % women
plot_2 <- dta |> 
  distinct(to_pfeml) |> 
  mutate(from_pfeml = mean(dta$from_pfeml, na.rm=T) - sd(dta$from_pfeml, na.rm=T), # All 1 Sd below mean of In-party % women
         diff_pfeml = abs(to_pfeml - from_pfeml), # Create difference between in-and out-party women
         # Select other values (opposition together, France 2012 country year)
         rile_distance_s = mean(dta$rile_distance_s, na.rm=T),
         prior_coalition = 0,
         prior_opposition = 1,
         countryyear = "France2012",
         to_mp_number = "31320",
         group = "below_mean"
         ) 

plot_dta <- bind_rows(plot_1, plot_2)

## Plot based on table.S5.2

figureS1.data <- as.data.frame(predict(table.S5.2, newdata = plot_dta, interval = "confidence"))

plot_dta <- plot_dta |> 
  mutate(fit = figureS1.data$fit,
         lwr = figureS1.data$lwr,
         upr = figureS1.data$upr)

figS1 <- ggplot(plot_dta, aes(x=to_pfeml, y=fit, lty=group)) +
  geom_line() +
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
ggsave("_graphs/figS1.pdf", width = 12, height = 10)

############################################
############ CREATING TABLE S6 #############
############################################

## Read in data
load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  mutate(countryyear = paste(country, year, sep = "")) |> #creating the country-year fixed effects
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         party_dislike, party_like, countryyear, to_pfeml, 
         from_rile, to_rile, logDM, to_left_bloc, to_prior_seats) |> 
  na.omit()

dta_small <- dta |> filter(to_prior_seats >= 4) 

table.S6.1 <-lm(party_like ~ to_pfeml + rile_distance_s + logDM + prior_coalition + prior_opposition  + as.factor(year), data = dta_small)
table.S6.2 <-lm(party_like ~ to_pfeml*logDM + rile_distance_s + prior_coalition + prior_opposition  + as.factor(year), data = dta_small)
table.S6.3 <-lm(party_like ~ to_pfeml*logDM + rile_distance_s*logDM + prior_coalition*logDM + prior_opposition*logDM  + as.factor(year), data = dta_small)

summary(table.S6.1)
summary(table.S6.2)
summary(table.S6.3)

stargazer(type = "text", table.S6.1, table.S6.2, table.S6.3, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S6.1, table.S6.2, table.S6.3,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "rile_distance_s", "logDM", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING TABLE S7 #############
############################################

#Out party % women, non-clustered SEs
load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  mutate(cntryyr = paste(country, year, sep = "")) |>  #creating the country-year fixed effects
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         party_dislike, party_like, cntryyr, to_pfeml, to_prior_seats) |> 
  na.omit() |> 
  mutate(to_pfeml2 = to_pfeml^2) |>  # Creating squared term for out-party % women
  filter(to_prior_seats >= 4)

table.S7.1 <-lm(party_like ~ to_pfeml + to_pfeml2 + as.factor(cntryyr), data = dta)
table.S7.2 <-lm(party_like ~ to_pfeml + to_pfeml2 + rile_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = dta)

summary(table.S7.1)
summary(table.S7.2)

# With clustered ses
stargazer(type = "text", table.S7.1, table.S7.2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S7.1, table.S7.2,
                        clusters = dta$country),
          keep = c("to_pfeml", "to_pfeml2", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING FIG. S2 #############
############################################

## Create Plot Data

# All values of Out-Party % women

plot_S2 <- dta |> 
  distinct(to_pfeml) |> 
  mutate(to_pfeml2 = to_pfeml^2, # Create difference between in-and out-party women
         ## Select other values (mean RILE distance, opposition together, France 2012 country year)
         rile_distance_s = mean(dta$rile_distance_s, na.rm=T),
         prior_coalition = 0,
         prior_opposition = 1,
         cntryyr = "France2012",
         to_mp_number = "31320"
  )

figureS2.data <- data.frame(predict(table.S7.2, newdata = plot_S2, interval = "confidence"))

plot_S2 <- plot_S2 |> 
  mutate(fit = figureS2.data$fit,
         lwr = figureS2.data$lwr,
         upr = figureS2.data$upr)

ggplot(plot_S2, aes(x=to_pfeml, y=fit))+ 
  geom_line() +
  geom_ribbon(aes(x = to_pfeml, y = fit, ymin = lwr,
                  ymax = upr),
              lwd = 1/2, alpha=0.1) +
  theme_minimal() + 
  theme(plot.title = element_text(size=12)) +
  ylab("Predicted Out-Party Thermometer Rating")+
  xlab("Proportion of Out-Party Women MPs") + 
  ylim(c(2,5))
ggsave("_graphs/figS2.pdf", width = 12, height = 10)

############################################
############ CREATING TABLE S8 #############
############################################

# Women-led parties
load("_data/dyadic_data_1-4-22.Rdata")

dta_womenlead <- updated_data |> 
  filter(to_femaleleader == 1) |> 
  mutate(countryyear = paste(country, year, sep = "")) |>  #creating the country-year fixed effects
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         party_dislike, party_like, countryyear, to_pfeml, to_prior_seats) |> 
  na.omit() |> 
  filter(to_prior_seats >= 4)

table.S8A1 <-lm(party_like ~ to_pfeml + as.factor(countryyear), data = dta_womenlead)
table.S8A2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_womenlead)

summary(table.S8A1)
summary(table.S8A2)

# With clustered SEs
stargazer(type = "text", table.S8A1, table.S8A2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S8A1, table.S8A2,
                        clusters = dta_womenlead$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

# Male-led parties

load("_data/dyadic_data_1-4-22.Rdata")

dta_malelead <- updated_data |> 
  filter(to_femaleleader == 0) |> 
  mutate(countryyear = paste(country, year, sep = "")) |> # creating the country-year fixed effects
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         party_dislike, party_like, countryyear, to_pfeml, to_prior_seats) |> 
  na.omit() |> 
  filter(to_prior_seats >= 4) # Exclude small parties

table.S8B1 <-lm(party_like ~ to_pfeml + as.factor(countryyear), data = dta_malelead)
table.S8B2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta_malelead)

summary(table.S8B1)
summary(table.S8B2)  

### With clustered SEs
stargazer(type = "text", table.S8B1, table.S8B2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S8B1, table.S8B2, 
                        clusters = dta_malelead$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING TABLE S9 #############
############################################

## Read in data
load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  mutate(countryyear = paste(country, year, sep = ""), # creating the country-year fixed effects
         partydyad   = paste(from_mp_number, to_mp_number, sep = "")) |>  #creating the party fixed effects / cluster
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         party_dislike,party_like, countryyear, to_pfeml, 
         from_rile, to_rile, to_mp_number, partydyad, to_prior_seats) |> 
  na.omit() |> 
  filter(to_prior_seats >= 4) # Exclude small parties

table.S9 <-lm(party_like ~ to_pfeml  + rile_distance_s + prior_coalition + prior_opposition + as.factor(countryyear), data = dta)
summary(table.S9)

### With clustered SEs
stargazer(type = "text", table.S9,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Out-Party Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S9,
                        clusters = dta$partydyad),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

############################################
############ CREATING TABLE 10 #############
############################################

load("_data/dyadic_data_1-4-22.Rdata")

dta <- updated_data |> 
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         party_dislike, to_pfeml, party_like, to_prior_seats) |> 
  na.omit() |> 
  filter(to_prior_seats >= 4) # Remove small parties, with fewer than 4 seats

table.S10.1 <-lm(party_like ~ to_pfeml + as.factor(country), data = dta_small)
table.S10.2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(country), data = dta_small)

summary(table.S10.2)

### With clustered SEs
stargazer(type = "text", table.S10.1, table.S10.2,
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table.S10.1, table.S10.2,
                        clusters = dta_small$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

##################################################
############ CREATING TABLES 11 & 12 #############
##################################################  

load("_data/multilevel_1-5-22.Rdata") 

indiv_data <- multilevel_data |> 
  select(year, country, rile_distance_s, prior_coalition, 
         prior_opposition, econ_distance_s, society_distance_s,
         cntryyr, to_pfeml, from_pfeml, thermometer_score, ID, 
         party_to, party_from, from_partyname, to_partyname, to_left_bloc, from_left_bloc,
         to_right_bloc, from_right_bloc, gender, to_parfam, from_parfam, to_prior_seats,
         from_mp_number, to_mp_number) |> 
  mutate(gender = case_when(gender == "1" ~ "male",
                            gender == "2" ~ "female"),  # Create gender variable
         gender = as.factor(gender)) |> 
  filter(is.na(to_pfeml) == F) |> 
  mutate(dyad = paste(from_mp_number, to_mp_number, sep ="_to_"))

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

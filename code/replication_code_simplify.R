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

############################################
###### CREATING TABLE 1 COLUMNS 1 & 2 ######
############################################

dta <- updated_data |> 
  mutate(cntryyr = paste(country, year, sep = "")) |> # creating the country-year fixed effects
  filter(to_prior_seats >= 4) # Removing smaller parties
  


  

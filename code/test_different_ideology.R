########################################################### Replication of the article Can’t We All Just Get Along? - testing alternative scales ###########################################################

#### ___ ####

#### 0.1 - Packages ####

Packages <- c("tidyverse", "stargazer", "fixest", "modelsummary", "haven", "estimatr")
lapply(Packages, library, character.only = T)
options(tibble.print_max = Inf,scipen=999)

#### 0.2 - Load data ####

load("_data/multilevel_1-5-22.Rdata")

Data <- multilevel_data |> 
  mutate(gender = case_when(gender == "1" ~ "male",
                            gender == "2" ~ "female"),  # Create gender variable
         gender = as.factor(gender)) |> 
  filter(is.na(to_pfeml) == F) |> 
  mutate(dyad = paste(from_mp_number, to_mp_number, sep ="_to_"))
  
#### ___ ####

#### 1 - Regression (basic) ####

model_1 <- lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = Data)
summary(model_1)

model_1_data <- broom::tidy(model_1) 

model_1_conf_95 <- confint(model_1, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..",
         "conf.high_95" = "X97.5..")

model_1_data_clean <- bind_cols(model_1_data, model_1_conf_95) |> 
  filter(term %in% c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition")) |> 
  mutate(model = "rile_distance_s")

#### 1.1 - Regression (Other ideological scales) ####

#### nationalism_distance_s ####

model_2 <- lm(party_like ~ to_pfeml + nationalism_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = Data)
summary(model_2)

model_2_data <- broom::tidy(model_2) 

model_2_conf_95 <- confint(model_2, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..",
         "conf.high_95" = "X97.5..")

model_2_data_clean <- bind_cols(model_2_data, model_2_conf_95) |> 
  filter(term %in% c("to_pfeml", "nationalism_distance_s", "prior_coalition", "prior_opposition")) |> 
  mutate(model = "nationalism_distance_s")

#### econ_distance_s ####

model_3 <- lm(party_like ~ to_pfeml + econ_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = Data)
summary(model_3)

model_3_data <- broom::tidy(model_3) 

model_3_conf_95 <- confint(model_3, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..",
         "conf.high_95" = "X97.5..")

model_3_data_clean <- bind_cols(model_3_data, model_3_conf_95) |> 
  filter(term %in% c("to_pfeml", "econ_distance_s", "prior_coalition", "prior_opposition")) |> 
  mutate(model = "econ_distance_s")


#### galtan_distance_s ####

model_4 <- lm(party_like ~ to_pfeml + galtan_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = Data)
summary(model_4)

model_4_data <- broom::tidy(model_4) 

model_4_conf_95 <- confint(model_4, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..",
         "conf.high_95" = "X97.5..")

model_4_data_clean <- bind_cols(model_4_data, model_4_conf_95) |> 
  filter(term %in% c("to_pfeml", "galtan_distance_s", "prior_coalition", "prior_opposition")) |> 
  mutate(model = "galtan_distance_s")

#### society_distance_s ####

model_5 <- lm(party_like ~ to_pfeml + society_distance_s + prior_coalition + prior_opposition + as.factor(cntryyr), data = Data)
summary(model_5)

model_5_data <- broom::tidy(model_5) 

model_5_conf_95 <- confint(model_5, level = 0.95) %>% 
  data.frame() %>%
  rename("conf.low_95" = "X2.5..",
         "conf.high_95" = "X97.5..")

model_5_data_clean <- bind_cols(model_5_data, model_5_conf_95) |> 
  filter(term %in% c("to_pfeml", "society_distance_s", "prior_coalition", "prior_opposition")) |> 
  mutate(model = "society_distance_s")


#### Bind dataset ####

Bind_model_data <- bind_rows(model_1_data_clean, 
                             model_2_data_clean, 
                             model_3_data_clean, 
                             model_4_data_clean,
                             model_5_data_clean)
### Final graph ###

ggplot(Bind_model_data, aes(x = term, y = estimate)) +
  geom_point(size=6) +
  facet_wrap(~model) +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "black", lty = 2, size = 2) +
  scale_x_discrete("") +
  scale_y_continuous("\nCoefficients") +
  geom_point(aes(x = term, y = estimate)) + 
  geom_linerange(aes(x = term,  ymin = conf.low_95, ymax = conf.high_95), lwd = 1/2) + 
  geom_errorbar(aes(ymin = conf.low_95, ymax = conf.high_95), size=10, width=0) +
  theme_bw(base_size = 25) + 
  theme(plot.title = element_text(size=22, hjust = 0.5, colour = "black"),
        axis.text = element_text(size = 21, colour = "black"), 
        plot.caption = element_text(size = 21, hjust = 0, colour = "black"))
ggsave("_graphs/ideological_scale.png", width = 12, height = 8)


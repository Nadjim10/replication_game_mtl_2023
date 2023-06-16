#' ---
#' title: '**Replicating Adams et al. (2023)**'
#' subtitle: Analysis of missing data
#' author: "Rémi Thériault"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: cerulean
#'     highlight: pygments
#'     toc: yes # Add a table of content?
#'     toc_depth: 3 # What levels to include in the table of content?
#'     toc_float: yes # Make the table of content follow when scrolling down?
#'     number_sections: no # Add a number to your sections?
#'     df_print: kable
#'     code_folding: show # Add a button to fold code (or: hide)
#'     code_download: yes # Add a button to download the script
#'     anchor_sections: # Add link buttons to each section
#'       style: symbol
#' ---

#+ setup, echo=FALSE, include=FALSE
if(!require(remotes)){install.packages("remotes")}
if(!require(klippy)){remotes::install_github("rlesur/klippy")}
klippy::klippy(position = c('top', 'right'))

#' # Table 1 With and Without `na.omit()`

#+ results=FALSE, warning=FALSE, message=FALSE
Packages <- c("tidyverse", "stargazer", "fixest", "modelsummary", "haven", "estimatr", "doParallel", "missForest")
lapply(Packages, library, character.only = T)
options(tibble.print_max = Inf,scipen=999)

#+ include=TRUE
load("_data/dyadic_data_1-4-22.Rdata")

#' First, let us check the number of rows in the data provided by the original authors.

nrow(updated_data)

dta2 <- updated_data |> 
  mutate(cntryyr = paste(country, year, sep = "")) |> # creating the country-year fixed effects
  filter(to_prior_seats >= 4) |>  # Removing smaller parties
  select(year, country, rile_distance_s, prior_coalition, prior_opposition, 
         econ_distance_s, society_distance_s, party_dislike, party_like, cntryyr,
         to_pfeml, to_prior_seats, to_mp_number) # selecting variables

dta <- dta2 |>
  na.omit()

#' Then the number of rows in transformed data.

nrow(dta)

#' And the difference between the two, so how many rows were lost.

nrow(updated_data) - nrow(dta)

#' We have thus lost 424 observations. Yet, we only have 177 missing observations as part of the variables necessary for Table 1.1, as demonstrated below.

updated_data |> 
  select(party_like, to_pfeml, cntryyr) |> 
  na.omit() |> 
  nrow() - nrow(dta)

#' For Table 1.2, we also have only 177 missing observations.

true.missing <- updated_data |> 
  select(party_like, to_pfeml, cntryyr, rile_distance_s,
         prior_coalition, prior_opposition) |> 
  na.omit() |> 
  nrow() - nrow(dta)
true.missing

#' From the 424 originally excluded observations, we can remove the 177 true missing to see how many observations were excluded unnecessarily.

nrow(updated_data) - nrow(dta) - true.missing

#' Thus, the original authors excluded 247 observations unnecessarily. Yet, the row difference betweeen the two data sets is still only 56, not 247, probably because we are losing some rows (368) when removing smaller parties with `filter(to_prior_seats >= 4)` when creating dta2.

nrow(dta2) - nrow(dta)

#' ## Tables Comparison

#' Let us now compare results with and without the missing data.

table1.1 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta)
table1.2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + 
                prior_opposition + as.factor(cntryyr), data = dta)

table1.1_2 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta2)
table1.2_2 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + 
                  prior_opposition + as.factor(cntryyr), data = dta2)

#+ results="asis"
# With clustered ses
stargazer(type = "html", table1.1, table1.2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.1, table1.2,
                        clusters = dta$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

stargazer(type = "html", table1.1_2, table1.2_2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), 
                           c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.1_2, table1.2_2,
                        clusters = dta$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

#' 
#' > The conclusion thus far is that the results from the two data sets are perfectly identical, even though they differ on 56 observations (out of 1898, ~3% of the data). Thus, using `na.omit()` was unnecessary since it did not generate an error and did not change the results.
#' 
#' At the same time, I have noticed that the number of clusters used in the `se` argument of the stargazer function above still uses the `dta` dataset (with `na.omit`), not the one with the missing values. It is likely why the results are identical.
#' 
#' > `se`     a list of numeric vectors that will replace the default coefficient values for each model. Behaves exactly like the argument coef.
#' 
#' I have tried changing it to `dta2`, but this leads to an error: 
#' 

#+ eval=FALSE
"Error in commarobust(x, se_type = se_type, clusters = clusters, alpha = alpha) : 
  `clusters` must be the same length as the model data"

#'  
#'  It is likely that this is because `lm()` uses `na.omit()` on the data, but not on the cluster. Indeed, na.omit is the default treatment of missing values in lm:
#'  
#' > `na.action`    a function which indicates what should happen when the data contain NAs. The default is set by the `na.action`
#'  
#' Setting `na.action = NULL` in the lm models leads to an error, so that is not an option. Our only choice then might be to impute the missing data. However, this at least explains why the original authors used `na.omit()`.
#' 

#+ eval=FALSE
"Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
  NA/NaN/Inf in 'x'"

#' # Imputing Missing Data {.tabset}
#' 

#' ## ...

#' 
#' What if we impute the missing data? Here, we will impute missing data with the `missForest` package, as it is one of the best imputation methods.
#' 

#' ## Imputation

# Need logical and character variables as factors for missForest
# "Error: Can not handle categorical predictors with more than 53 categories."
new.data <- dta2 %>% 
  select(-cntryyr) %>% # Too many categories (> 53)
  mutate(across(c(where(is.character), where(is.logical)), as.factor)) %>% 
  as.data.frame()

# Parallel processing
registerDoParallel(cores = 4)

# Variables
set.seed(100)
data.imp <- missForest(new.data, verbose = TRUE, parallelize = "variables")

# Extract imputed dataset
dta3 <- data.imp$ximp

# Add back country-year
dta3 <- dta3 %>% 
  mutate(cntryyr = dta2$cntryyr)

#' ## Details

#' Why impute the data? van Ginkel explains,
#' 
#' > Regardless of the missingness mechanism, multiple imputation is always to be preferred over listwise deletion. Under MCAR it is preferred because it results in more statistical power, under MAR it is preferred because besides more power it will give unbiased results whereas listwise deletion may not, and under NMAR it is also the preferred method because it will give less biased results than listwise deletion.
#'  
#' van Ginkel, J. R., Linting, M., Rippe, R. C. A., & van der Voort, A. (2020). Rebutting existing misconceptions about multiple imputation as a method for handling missing data. *Journal of Personality Assessment*, *102*(3), 297-308. https://doi.org/10.1080/00223891.2018.1530680
#' 
#' Why `missForest`? It outperforms other imputation methods, including the popular MICE (multiple imputation by chained equations). You also don’t end up with several datasets, which makes it easier for following analyses. Finally, it can be applied to mixed data types (missings in numeric & categorical variables).
#' 
#' Waljee, A. K., Mukherjee, A., Singal, A. G., Zhang, Y., Warren, J., Balis, U., ... & Higgins, P. D. (2013). Comparison of imputation methods for missing laboratory data in medicine. *BMJ open*, *3*(8), e002847. https://doi.org/10.1093/bioinformatics/btr597
#' 
#' Stekhoven, D. J., & Bühlmann, P. (2012). MissForest—non-parametric missing value imputation for mixed-type data. *Bioinformatics*, *28*(1), 112-118. https://doi.org/10.1093/bioinformatics/btr597
#' 

#' 
#' # Final Tables Comparison
#' 

#' Let us now compare results with and without the imputed data.

table1.1_3 <-lm(party_like ~ to_pfeml + as.factor(cntryyr), data = dta3)
table1.2_3 <-lm(party_like ~ to_pfeml + rile_distance_s + prior_coalition + 
                  prior_opposition + as.factor(cntryyr), data = dta3)

#+ results="asis"
# With clustered ses
stargazer(type = "html", table1.1, table1.2, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.1, table1.2,
                        clusters = dta$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

stargazer(type = "html", table1.1_3, table1.2_3, 
          add.lines = list(c("Country-Year Fixed Effects?", "Yes"), 
                           c("Country-Level Clustered SEs?", "Yes")),
          se = starprep(table1.1_3, table1.2_3,
                        clusters = dta3$country),
          keep = c("to_pfeml", "rile_distance_s", "prior_coalition", "prior_opposition", 
                   "econ_distance_s", "society_distance_s"))

#' > After imputation, the numbers differ a little bit (good sign that they are not identical!). However, the results appear pretty robust since they are very similar and nothing has changed signed or significance threshold.
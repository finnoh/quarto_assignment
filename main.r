#######################################################################
#### load packages and data

# load packages
if (!require(pacman)) {
    install.packages("pacman")
}
p_load(
    tidyverse, dplyr, stargazer, ivreg, kableExtra, sampleSelection,
    stats
)

# load data
df_data <- read.csv("assignment1_2023.csv")

# attach data frame
suppressMessages(attach(df_data))

#######################################################################
#### functions

# extract F statistic from ivreg model
extract_F_stat <- function(model) {
    summary_obj <- summary(model)
    tmp <- summary_obj$diagnostics %>% as.data.frame()
    return(tmp["Weak instruments", "statistic"] %>% round(4) %>% as.character())
}

# extract F statistic from ivreg model
extract_WH_pval <- function(model) {
    summary_obj <- summary(model)
    tmp <- summary_obj$diagnostics %>% as.data.frame()
    return(tmp["Wu-Hausman", "p-value"] %>% round(4) %>% as.character())
}

#######################################################################
#### question 1 i)

model_1_i <- lm(logwage ~ schooling + age + I(age^2))

#######################################################################
#### question 1 iii)

# create dummy
logwage_D <- ifelse(
    is.na(logwage),
    0,
    1
)

# model without exclusion restriction
model_1_iii_1 <- selection(
    logwage_D ~ schooling,
    logwage ~ schooling + age + I(age^2),
    method = "2step"
)

# model with exlusion restriction
model_1_iii_2 <- selection(
    logwage_D ~ married,
    logwage ~ schooling + age + I(age^2),
    method = "2step"
)

#######################################################################
#### question 1 iv)

# model without exclusion restriction
model_1_iv_1 <- selection(
    logwage_D ~ schooling,
    logwage ~ schooling + age + I(age^2),
    method = "ML"
)

# model with exlusion restriction
model_1_iv_2 <- selection(
    logwage_D ~ married,
    logwage ~ schooling + age + I(age^2),
    method = "ML"
)

#######################################################################
#### question 2 ii)
model_2_ols <- lm(logwage ~ schooling)
model_2_iv_dist <- ivreg(logwage ~ schooling | distance)
model_2_iv_subs <- ivreg(logwage ~ schooling | subsidy)
model_2_iv_both <- ivreg(logwage ~ schooling | subsidy + distance)

#######################################################################
#### question 2 iii)

# create table of Wu-Hausman p-vals
table_hausman <- tibble(
    IV = c("IV Distance", "IV Subsidy", "IV Both"),
    pvalue = c(
        extract_WH_pval(model_2_iv_dist),
        extract_WH_pval(model_2_iv_subs),
        extract_WH_pval(model_2_iv_both)
    )
)

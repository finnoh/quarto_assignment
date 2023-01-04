#######################################################################
#### load packages and data

# load packages
if (!require(pacman)) {
    install.packages("pacman")
}
p_load(tidyverse, dplyr, stargazer, ivreg, kableExtra)

# load data
df_data <- read.csv("assignment1_2023.csv")

# attach data frame
suppressMessages(attach(df_data))

#######################################################################
# extract F statistic from ivreg model
extract_F_stat <- function(model) {
   summary_obj <- summary(model)
   tmp <- summary_obj$diagnostics %>% as.data.frame()
   return(tmp['Weak instruments', 'statistic'] %>% round(4) %>% as.character())
}
# extract F statistic from ivreg model
extract_WH_pval <- function(model) {
   summary_obj <- summary(model)
   tmp <- summary_obj$diagnostics %>% as.data.frame()
   return(tmp['Wu-Hausman', 'p-value'] %>% round(4) %>% as.character())
}
#######################################################################
#### question 1 i)

model_1_i <- lm(logwage ~ schooling + age + I(age^2))

### question 2 ii)
model_2_ols <- lm(logwage ~ schooling)
model_2_iv_dist <- ivreg(logwage ~ schooling | distance)
model_2_iv_subs <- ivreg(logwage ~ schooling | subsidy)
model_2_iv_both <- ivreg(logwage ~ schooling | subsidy + distance)

### question 2 iii)
# create table of Wu-Hausman p-vals

table_hausman <- tibble(IV = c("IV Distance", "IV Subsidy", "IV Both"),
pvalue = c(extract_WH_pval(model_2_iv_dist),
extract_WH_pval(model_2_iv_subs),
extract_WH_pval(model_2_iv_both)))

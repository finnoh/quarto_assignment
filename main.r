#######################################################################
#### load packages and data

# load packages
if (!require(pacman)) {
    install.packages("pacman")
}
p_load(tidyverse, dplyr, stargazer)

# load data
df_data <- read.csv("assignment1_2023.csv")

# attach data frame
suppressMessages(attach(df_data))

#######################################################################
#### question 1 i)

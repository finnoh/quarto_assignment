# Setup -----------------------------------------
if (!require(pacman)) {
    install.packages("pacman")
}
pacman::p_load(tidyverse, ggpubr, kableExtra)

# common theme to add to all ggplot plots
mytheme <-  theme_bw() + theme(legend.position = "bottom")
# create a common format for all your tables
create_table <- function(df){
    df |>
    kableExtra::kbl(digits = 3) |>
    kableExtra::kable_styling(latex_options = c("HOLD_position", "scale_down"))
}

# Functions -----------------------------------------

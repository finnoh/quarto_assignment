# Setup -----------------------------------------
# load and install packages
if (!require(pacman)) {
    install.packages("pacman")
}
pacman::p_load(tidyverse)

# source the toolbox
source("./dev/toolbox.r")
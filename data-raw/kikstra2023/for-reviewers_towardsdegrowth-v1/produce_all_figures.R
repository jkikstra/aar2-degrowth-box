try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
library(here)
here::i_am("pg-aus-ii.Rproj")

# import functions from other files
source("utils.R")
load_pkgs()

# prepare data (requires downloading IPCC scenarios - this step has been processed already for the reviewers)
# source("process-data.R")

# run all main figures
source("f02.R")
source("f03.R")
source("f04.R")
source("f05.R")
source("f06.R")
source("f07.R")
source("f08.R")
source("f09.R")
source("f10.R")
source("f11.R")

# run all supplementary figures
source("sf01.R")
source("sf02.R")
source("sf03.R")
source("sf04.R")
source("sf05.R")

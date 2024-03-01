# Libraries required for
# talk-functions.R and paper-functions.R
library(stringr)
library(dplyr)
library(magrittr)
library(purrr)
library(lubridate)
library(RefManageR)
library(webshot2)

source("code/talk-functions.R")
source("code/paper-functions.R")

# Read in talk data
library(readxl)
talk_data <- read_xlsx("data/CV.xlsx", sheet ="Talks")

if (!dir.exists("posts/talks")) dir.create("posts/talks", recursive = T)
talk_data %>%
  # Cleaning
  mutate(across(everything(), ~str_remove(., ", ?$"))) %>%
  talk_to_params %>%
  purrr::transpose() %>%
  purrr::walk(., create_talk)


# Read in bib data
pub_bib <- "data/CV.bib"
# pub_bib <- "https://raw.githubusercontent.com/srvanderplas/CV/master/CV.bib"

bibfile <- readLines(pub_bib)
# Remove author annotations so RefManageR doesn't get confused
bibfile <- bibfile[!grepl("\\+an", bibfile)]
# Remove comments from bibfile
bibfile <- bibfile[!grepl("\\s{0,}%", bibfile)]
# and write remaining lines to tfile
tfile <- tempfile(fileext = ".bib")
writeLines(bibfile, tfile)
pubs <- RefManageR::ReadBib(tfile)

#   vector of package names in paper tiles
pkg_names <- c("cmcR", "animint", "ggplot2", "ggenealogy")
pkg_names_fix <- paste("`", pkg_names, "`", sep = "")
names(pkg_names_fix) <- pkg_names

if (!dir.exists("posts/papers")) dir.create("posts/papers", recursive = T)

pub_info <- lapply(pubs, pub_to_params)
purrr::walk(pub_info, create_paper)

# This script reads in a bibtex file and generates posts for each paper in the file
#
# Configuration:
#  Link to bib file
pub_bib <- "https://raw.githubusercontent.com/srvanderplas/CV/master/SusanVanderplas-CV.bib"
#  vector of package names in paper tiles
pkg_names <- c("cmcR", "animint", "ggplot2", "ggenealogy")
#  keywords to use to indicate different paper types
types <- c("pr" = "Peer Reviewed", "npr" = "Other (Not Peer Reviewed)")


# R packages
library(yaml)
library(RefManageR)
library(magrittr)
library(stringr)


bibfile <- readLines(pub_bib)
# Remove author annotations for now
bibfile <- bibfile[!grepl("\\+an", bibfile)]
tfile <- tempfile(fileext = ".bib")
writeLines(bibfile, tfile)
pubs <- RefManageR::ReadBib(tfile)
pub_df <- as.data.frame(pubs)

pkg_names_fix <- paste("`", pkg_names, "`", sep = "")
names(pkg_names_fix) <- pkg_names


pub_to_params <- function(entry) {
  post_params <- list()
  df <- as.data.frame(entry)

  post_params$name <- names(entry)
  post_params$title <- stringr::str_remove_all(df$title, "\\{\\}") %>%
    stringr::str_replace_all(pkg_names_fix)

  post_params$author <- df$author
  post_params$date <- df$year

  RefManageR::NoCite(entry)
  post_params$citation <- capture.output(RefManageR::PrintBibliography(entry)) %>%
    stringr::str_remove("^\\[\\d{1,}\\] ")

  # Optional stuff
  post_params$other <- ""


  if (length(types) > 0) {
    post_params$other <- paste("### Type\n", stringr::str_replace_all(df$keywords, types))
  }

  df$keywords <- setdiff(df$keywords, names(types))
  if (length(df$keywords) > 0) {
    post_params$other <- paste("### Keywords")
  }

  if (!is.na(df$github)) {
    post_params$other <- c(
      post_params$other,
      sprintf("[{{< fa brands github size=2x >}} Repository for Paper and Additional Resources](%s){.btn .btn-info role=\"button\"}", df$github)
    )
  }

  return(post_params)
}

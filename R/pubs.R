# This script reads in a bibtex file and generates posts for each paper in the file
#
# Configuration:
#   Link to bib file
pub_bib <- "https://raw.githubusercontent.com/srvanderplas/CV/master/SusanVanderplas-CV.bib"
#   vector of package names in paper tiles
pkg_names <- c("cmcR", "animint", "ggplot2", "ggenealogy")

# This function converts publications to a list of information necessary to make the post
pub_to_params <- function(entry) {
  post_params <- list()
  entrylist <- unlist(entry)

  post_params$name <- names(entry)
  post_params$title <- stringr::str_remove_all(entry$title, "[^[[:alnum:] :?!\\.,-]]") %>%
    stringr::str_replace_all(pkg_names_fix) %>%
    stringr::str_remove_all("[{}]{1,}")

  post_params$author <- paste(entry$author, collapse = ", ")
  post_params$date <- entry[["date"]] %>%
    str_remove_all("NA")


  addendum <- entrylist$addendum
  RefManageR::NoCite(entry)
  post_params$citation <- capture.output(RefManageR::PrintBibliography(entry, .opts = list(no.print.fields = "addendum", style = "markdown", bib.style = "authoryear", first.inits = T, dashed = F))) %>% paste(collapse = " ")

  post_params$citation <- str_replace(post_params$citation, "NA", "")

  post_params$bibtex <-  capture.output(RefManageR::PrintBibliography(entry, .opts = list(no.print.fields = "addendum", style = "Bibtex", bib.style = "authoryear")))

  # Optional stuff
  post_params$other <- ""

  if ("pic" %in% names(entrylist)) {
    post_params$image <- entry$pic
  }

  if ("addendum" %in% names(entrylist)) {
    post_params$other <- c(post_params$other, "### Contribution", "", "Writing and programming entries estimated from `git fame` for repositories where this would be meaningful.", "", addendum)
  }

  if ("keywords" %in% names(entrylist)) {
    post_params$keywords <- stringr::str_split(entrylist$keywords, ",")
  } else {
    post_params$keywords <- ""
  }


  if ("github" %in% names(entrylist)) {
    post_params$other <- c(
      post_params$other,
      "",
      sprintf("[{{< fa brands github size=2x >}} Repository for Paper and Additional Resources](%s){.btn .btn-tip role=\"button\"}", entrylist$github)
    )
  }

  return(post_params)
}

yaml_kv <- function(key,value) {
  value = unlist(value)
  if (length(value) == 1) {
    sprintf("%s: \"%s\"", key, value)
  } else {
    valseq <- paste(value,  collapse = ", ")
    # message(valseq)
    sprintf("%s: [%s]", key, valseq)
  }
}
# yaml_kv("test", 1)
# yaml_kv("keywords", value = c("1", "2", "3"))

# This function writes out a qmd file in the correct directory corresponding to a post
create_paper <- function(params, path = "posts/papers") {

  post_name <- str_replace_all(params$name, "[[:punct:][:space:]]{1,}", "-")
  post_name <- paste0(post_name, ".qmd")

  md_lines <- c(
    "---",
    yaml_kv("title", params$title),
    yaml_kv("author", params$author),
    yaml_kv("date", params$date),
    ifelse("image" %in% names(params), yaml_kv("image", params$image), ""),
    "categories: papers",
    "listing:",
    "  contents: posts/papers",
    "  sort: date desc",
    "  fields: [date, title, author]",
    "page-layout: full",
    "title-block-banner: true",
    ifelse(length(params$keywords) > 0,
           yaml_kv("keywords", params$keywords),
           ""),
    "format:",
    "  html:",
    "    code-copy: true",
    "---",
    " ",
    ifelse("image" %in% names(params), sprintf("![](%s){.preview-image}", params$image), ""),
    " ",
    "## Citation",
    sprintf("> %s", params$citation),
    "",
    "::: {.callout-note collapse='true'}",
    "## Bibtex",
    "```",
    params$bibtex,
    "```",
    ":::",
    "",
    "",
    params$other
  )

  writeLines(md_lines, con = file.path(path, post_name))
}


# R packages
library(RefManageR)
library(magrittr)
library(stringr)


bibfile <- readLines(pub_bib)
# Remove author annotations for now
bibfile <- bibfile[!grepl("\\+an", bibfile)]
tfile <- tempfile(fileext = ".bib")
writeLines(bibfile, tfile)
pubs <- RefManageR::ReadBib(tfile)
# pub_df <- as.data.frame(pubs)

pkg_names_fix <- paste("`", pkg_names, "`", sep = "")
names(pkg_names_fix) <- pkg_names

pub_info <- purrr::map(pubs, pub_to_params)
purrr::walk(pub_info, create_paper)

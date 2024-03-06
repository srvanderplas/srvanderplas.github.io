source("code/common-post-functions.R")

month_dict <- sprintf("%02d", c(1:12, 1:12, 1:12)) %>%
  as.list() %>%
  magrittr::set_names(c(month.abb, month.name, sprintf("%02d", 1:12)))

format_title <- function(x) {
  if (!exists("pkg_names_fix")) {
    pkg_names_fix <- c()
  }
  stringr::str_remove_all(x, "[^[[:alnum:] :?!\\.,-]]") %>%
    stringr::str_replace_all(pkg_names_fix) %>%
    stringr::str_remove_all("[{}]{1,}")
}

format_github <- function(x) {
  c("", sprintf("[{{< fa brands github size=2x >}} Repository for Paper and Additional Resources](%s){.btn .btn-tip role=\"button\"}", x))
}

format_addendum <- function(x) {
  c("### Contribution",
    "",
    "Writing and programming entries estimated from `git fame` for repositories where this would be meaningful.", "",
    x)
}

format_citation <- function(x) {
  RefManageR::NoCite(x)
  y <- capture.output(
    RefManageR::PrintBibliography(
      entry,
      .opts = list(no.print.fields = c("addendum", "keywords"),
                   style = "markdown",
                   first.inits = T,
                   dashed = F))
  ) %>%
    paste(collapse = " ")

  y <- stringr::str_replace(y, "NA", "")
  return(y)
}

format_bibtex <- function(x, ...) {
  capture.output(
    RefManageR::PrintBibliography(x, ...)
  )
}

# This function converts publications to a list of information necessary to make the post
pub_to_params <- function(entry) {
  post_params <- list()
  entrylist <- unlist(entry, recursive = F)

  post_params$name <- names(entry)
  post_params$title <- format_title(entry$title)

  post_params$author <- paste(entry$author, collapse = ", ")

  if ("date" %in% names(entrylist)) {
    post_params$date <- entrylist[["date"]] %>%
      str_remove_all("NA")
  } else if ("year" %in% names(entrylist)) {
    if ("month" %in% names(entrylist)) {
      post_params$date <- sprintf("%s%s", entrylist[["year"]], month_dict[[entrylist[["month"]]]])
    } else {
      post_params$date <- entrylist[["year"]]
    }
  } else {
    post_params$date <- "unknown"
  }

  post_params$citation <- format_citation(entry)

  post_params$bibtex <- format_bibtex(entry,
    .opts = list(no.print.fields = "addendum",
                 style = "Bibtex",
                 bib.style = "authoryear"))

  if ("pic" %in% names(entrylist)) {
    post_params$image <- entry$pic
  }

  if ("keywords" %in% names(entrylist)) {
    post_params$keywords <- stringr::str_split(entrylist$keywords, ",")
    post_params$keywords <- post_params$keywords[nchar(post_params$keywords) > 0]
  } else {
    post_params$keywords <- ""
  }

  # Optional stuff
  post_params$other <- ""

  if ("addendum" %in% names(entrylist)) {
    addendum <- entrylist[["addendum"]]
    post_params$other <- c(post_params$other,
                           format_addendum(addendum))
  }

  if ("github" %in% names(entrylist)) {
    post_params$other <- c(post_params$other, format_github(entrylist$github))
  }

  return(post_params)
}



# This function writes out a qmd file in the correct directory
# corresponding to a post
create_paper <- function(params, path = "posts/papers") {

  post_name <- format_post_name(params$name)
  post_name <- paste0(post_name, ".qmd")

  img_yaml <- ifelse("image" %in% names(params),
                     yaml_kv("image", params$image), "")
  kw_yaml <-  ifelse(length(params$keywords) > 0,
                     yaml_kv("keywords", params$keywords), "")
  md_lines <- c(
    "---",
    yaml_kv("title", params$title),
    yaml_kv("author", params$author),
    yaml_kv("date", params$date),
    img_yaml,
    kw_yaml,
    "categories: papers",
    "page-layout: full",
    "title-block-banner: true",
    "format:",
    "  html:",
    "    code-copy: true",
    "---", " ",
    ifelse("image" %in% names(params),
           sprintf("![](%s){.preview-image}", params$image), ""),
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
    "", "",
    params$other
  )

  writeLines(md_lines, con = file.path(path, post_name))
}


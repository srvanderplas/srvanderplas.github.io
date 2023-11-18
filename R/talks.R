library(googlesheets4)
library(lubridate)
library(dplyr)
library(stringr)
library(purrr)

# Update to match your info
sheet_link <- "https://docs.google.com/spreadsheets/d/1zOKie2rqIcxQMuAzn1g7K-2_O5yOhHo2QD7EAGjeFzs/edit?usp=sharing"
talk_order <- c("Invited", "Contributed", "Seminars")

talk_data <- read_sheet(sheet_link, sheet = "Talks")

format_abstract <- function(x) {
  if_else(
    is.na(x), "",
    paste("## Abstract",
          paste("> ", x, collapse = "\n"),
          sep = "\n\n"))
}

format_slides <- function(x) {
  if_else(
    is.na(x), "",
    paste("## Slides",
          sprintf("<iframe src='%s' height='auto' width='80%%'>
                        </iframe>",
                  x),
          sep = "\n\n"))
}

format_keywords <- function(x) {
  y <- str_split(x, ", ", simplify = F)
  y <- map(y, append, values = "Talk")
  y
}

talk_to_params <- function(df) {
  post_params <- select(
    df,
    title = Title,
    author = Authors,
    date = Date,
    abstract = Abstract,
    slides = Link,
    keywords = Keywords) %>%
    mutate(name = title %>% str_to_lower %>% str_replace_all("[^[a-z]]{1,}", "-"),
           date = format.Date(date, "%Y-%m-%d"),
           abstract = format_abstract(abstract),
           slides = format_slides(slides),
           keywords = format_keywords(keywords))

  post_params$event <- sprintf("## Location\n%s\n%s, %s\n%s",
                               df$Event2,
                               df$Event, df$EventType,
                               df$Location) %>%
    # Remove any missing information
    str_remove_all("( ,)?NA( ,)?")

  post_params
}

yaml_kv <- function(key,value) {
  value = unlist(value)
  if (length(value) == 1) {
    sprintf("%s: \"%s\"", key, value)
  } else {
    valseq <- paste(value,  collapse = "\', \'")
    # message(valseq)
    sprintf("%s: [\'%s\']", key, valseq)
  }
}
# yaml_kv("test", 1)
# yaml_kv("keywords", value = c("1", "2", "3"))

# This function writes out a qmd file in the correct directory corresponding to a post
create_talk <- function(params, path = "posts/talks") {

  post_name <- str_replace_all(params$name, "[[:punct:][:space:]]{1,}", "-")
  post_name <- paste0(post_name, ".qmd")

  md_lines <- c(
    "---",
    yaml_kv("title", params$title),
    yaml_kv("author", params$author),
    yaml_kv("date", params$date),
    "categories: talks",
    # "listing:",
    # "  contents: posts/talks",
    # "  sort: date desc",
    # "  fields: [date, title, author]",
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
    params$slides,
    params$event
  )

  writeLines(md_lines, con = file.path(path, post_name))
}

talk_data %>%
  talk_to_params %>%
  purrr::transpose() %>%
  walk(., create_talk)


source("code/common-post-functions.R")
source("code/image-functions.R")

format_post_name <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:][:space:]]{1,}", "-")
}

make_abstract <- function(x) {
  c("## Abstract", paste("> ", x, collapse = "\n"), "", "")
}

format_abstract <- function(x) {
  y <- x %>%
    str_split(., "\n", simplify = F) %>%
    map(make_abstract) %>%
    map_chr(~paste(., collapse = "\n"))

  dplyr::if_else(is.na(x), "", y)
}

format_slides <- function(x, width = 800, height = 500) {
  y <- paste(
    "## Slides",
    sprintf("<iframe src='%s' height='%spx' width='%spx'></iframe>",
            x, width, height),
    sep = "\n\n")
  dplyr::if_else(is.na(x), "", y)
}

format_keywords <- function(x) {
  y <- stringr::str_split(x, ", ", simplify = F) %>%
    purrr::map(stringr::str_squish) %>%
    purrr::map(~.[nchar(.) > 0])
  y <- purrr::map(y, append, values = "Talk")
  y
}

format_event <- function(df) {
  stopifnot(all(c("Event2", "Event", "EventType", "Location") %in% names(df)))

  sprintf(
    "## Location\n
    %s, %s
    Event Type: %s
    Location: %s",
    df$Event2,
    df$Event, df$EventType,
    df$Location) %>%
    # Remove any missing information
    stringr::str_remove_all("Event Type: NA    \\n") %>%
    stringr::str_remove_all("Location: NA") %>%
    stringr::str_remove_all("(\\s,)?NA(\\s,)?")
}

talk_to_params <- function(df) {
  post_params <- df %>%
    # Combine event data into a larger
    # event string with all 4 fields
    tidyr::nest(
      eventdata = c(Event2, Event, EventType, Location)
    ) %>%
    dplyr::mutate(
      event = purrr::map_chr(eventdata, format_event)
    ) %>%
    # Select cols corresponding to fields
    dplyr::select(
      title = Title,
      author = Authors,
      date = Date,
      abstract = Abstract,
      slides = Link,
      url = Link,
      image = Image,
      keywords = Keywords,
      event
    ) %>%
    # Create/format simple fields
    dplyr::mutate(
      name = format_post_name(title),
      date = format.Date(date, "%Y-%m-%d"),
      abstract = format_abstract(abstract),
      slides = format_slides(slides),
      keywords = format_keywords(keywords))


  post_params
}


# This function writes out a qmd file in the correct directory corresponding to a post
create_talk <- function(params, path = "posts/talks") {

  post_name <- paste0(params$name, ".qmd")

  post_img <- get_image_link(params, path)

  md_lines <- c(
    "---",
    yaml_kv("title", params$title),
    yaml_kv("author", params$author),
    yaml_kv("date", params$date),
    "categories: talks",
    "page-layout: full",
    "title-block-banner: true",
    ifelse(nchar(post_img) > 0,
           yaml_kv("image", post_img),
           ""),
    ifelse(length(params$keywords) > 0,
           yaml_kv("keywords", params$keywords),
           ""),
    "format:",
    "  html:",
    "    code-copy: true",
    "---",
    " ",
    params$abstract,
    params$event,
    params$slides
  )

  writeLines(md_lines, con = file.path(path, post_name))
}


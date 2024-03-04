
format_post_name <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:][:space:]]{1,}", "-")
}

format_abstract <- function(x) {
  dplyr::if_else(
    is.na(x), "",
    paste("## Abstract",
          paste("> ", x, collapse = "\n"),
          sep = "\n\n"))
}

format_slides <- function(x) {
  dplyr::if_else(
    is.na(x), "",
    paste("## Slides",
          sprintf("<iframe src='%s' height='auto' width='80%%'>
                        </iframe>",
                  x),
          sep = "\n\n"))
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

# https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  return(is.null(check))
}

screenshot_slides <- function(url, output_file, width = 1920, height = 1080, ...) {
  webshot2::webshot(
    url = url,
    file = output_file,
    vwidth = width,
    vheight = height,
    ...
  )
}

get_image <- function(x, output_file, verbose = F, ...) {
  if(file.exists(x)) {
    # Handle case where image is a file path - copy to posts/talks/file-name
    ext <- tools::file_ext(x)
    ext2 <- tools::file_ext(output_file)
    output_file <- stringr::str_replace(output_file, paste0(ext, "$"), ext2)
    if(file.copy(x, output_file)) {
      return(output_file)
    } else {
      return("")
    }
  } else {
    if (valid_url(x)) {
      if (!file.exists(output_file)) {
        screenshot_slides(x, output_file, ...)
      } else {
        if(verbose) message("File exists, will not overwrite")
      }
      return(output_file)
    } else {
      warning(paste("No way to interpret ", x, "... skipping"))
      return("")
    }
  }
}

get_image_link <- function(params, path = "posts/talks") {

  post_name <- format_post_name(params$name)
  output_file <- file.path(path, paste0(post_name, ".png"))

  # Choose between specified image (if it exists and is not NA) and first slide
  img_opts <- c(params$image, params$url) %>% na.omit()
  img_opts <- img_opts[nchar(img_opts) > 0]

  if (length(img_opts) > 0) {
    to_get <- img_opts[1]
  } else {
    to_get <- ""
  }

  if(nchar(to_get) > 0) {
    img_acquired <- get_image(to_get, output_file)
  } else {
    img_acquired <- ""
  }

  return(basename(img_acquired))
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
    params$slides,
    params$event
  )

  writeLines(md_lines, con = file.path(path, post_name))
}


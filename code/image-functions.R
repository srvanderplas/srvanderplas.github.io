
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
        img_file_exts <- c("svg", "png", "jpg", "jpeg", "gif", "webp")
        img_file_exts <- c(img_file_exts, toupper(img_file_exts))
        img_file_ext_expr <- img_file_exts %>% paste0("\\.", ., "$")
        is_img <- map_lgl(img_file_exts, ~stringr::str_detect(x, .))
        if (any(is_img)) {
          # webshot doesn't screenshot svgs without issues... this is faster.
          img_ext <- img_file_exts[is_img]
          output_file <- stringr::str_replace(output_file, "png$", img_ext)
          download.file(x, destfile = output_file, mode = "wb")
        } else {
          screenshot_slides(x, output_file, ...)
        }
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

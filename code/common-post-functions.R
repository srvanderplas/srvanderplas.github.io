`%>%` <- magrittr::`%>%`

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


format_post_name <- function(name) {
  name %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[[:punct:][:space:]]{1,}", "-")
}

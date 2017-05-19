?stringr::str_replace

#' @export
str_tokenize <- function(string) {
  string %>%
    strsplit(NULL) %>%
    unlist()
}

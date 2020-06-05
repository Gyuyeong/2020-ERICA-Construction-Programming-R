parse_string <- function(s) {
  s <- str_replace(s, "^\\.\\.\\.", "")
  s <- str_replace(s, "\\.\\.\\.$", "")
  s <- str_replace_all(s, "[\t\n]", "")
  s <- str_replace_all(s, "\\[.+\\]", "")
  s <- str_replace(s, "^\\s+", "")
  s <- str_replace(s, "[ ]+$", "")
  s <- str_replace(s, "\\(.+\\)", "")
  s <- str_replace_all(s, "¢º", "")
  s <- str_replace_all(s, "¡ß", "")
  s <- str_replace_all(s, "¡Ş", "")
  s <- str_replace_all(s, "¡ã", "")
  s <- str_replace_all(s, "¡á", "")
  s <- str_replace_all(s, "¡â", "")
  return (s)
}

parse_date <- function(s) {
  s <- substring(s, 1, 10)
  s <- str_replace_all(s, "\\.", "-")
  s <- as.character(s)
  return (s)
}

parse_price <- function(s) {
  s <- str_replace_all(s, ",", "")
  s <- as.integer(s)
  return (s)
}


fast_df = function(...) {
  as_tibble(list(...),validate = FALSE)
}

split.by = function(df, by) {
  split.li = split(df, df[by])
  split.li
}

remove.quotes = function(str, quotes=c("'",'"')) {
  has.quotes = substring(str,1,1) %in% quotes
  str[has.quotes] = substring(str[has.quotes],2, nchar(str[has.quotes])-1)
  str
}

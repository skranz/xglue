split.by = function(df, by) {
  dt = as.data.table(df)
  split.li = split(dt,by=by)
  split.li
}

do.by = function(df,fun, by) {
  library(data.table)
  dt = as.data.table(df)
  split.li = split(dt,by=by)
  res = lapply(split.li, fun)
  res
}

do.by.summarize = function(df,fun, by, res.name="value") {
  library(data.table)

  dt = as.data.table(df)
  split.li = split(dt,by=by)

  keys = rbindlist(lapply(split.li, function(sub.dt) {
    sub.dt[1,by,with=FALSE]
  })) %>% as.data.frame()

  res = lapply(split.li, fun)

  keys[[res.name]] = res
  keys

}

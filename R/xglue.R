examples.xglue = function() {
  setwd("C:/research/comment_phack")
  library(rmdtools)
  dat = readRDS("deround_binom.Rds")


  dat = filter(dat, method=="ALL", mode %in% c("reported","omit"), h %in% c(0.1,0.2))
  txt = readLines("tab_binom_glue.tex")
  modes = unique(dat$mode)
  options(warn=1)
  res = xglue(txt, open="<<", close=">>")

  writeLines(res, "test_table.tex")

  res$glued[[3]]

  df = data.frame(a=rep(0:1, length=100),b=rep(0:2, length=100),c=1:100)
  fun = function(mydat,...) {
    restore.point("fun")
    mydat$d = mydat$b*mydat$c
    mydat
  }
  res = do.by(df, fun, by=c("a","b"), "data")
}

#' Perform xglue operation on a template text
#'
#' @param txt The template text on which xglue operations shall be performed
#' @param envir Environment or list that contains objects whose value is spliced in. By default the environment from which xglue is called.
#' @param open the opening string of glue whiskers
#' @param close the closing string of glue whiskers
#' @param enclos If envir is a list the enclosing environment.
#' @param bdf the result of a manual call of parse.xglue.blocks. Don't need to change this parameter, may only be helpful for speeding things up if the same txt shall be glued with different with values.
#'
#' @returns The glued text as a single character
#' @export
xglue = function(txt, envir=parent.frame(),open="{", close="}", enclos=parent.frame(), bdf = parse.xglue.blocks(txt)) {
  restore.point("xglue")
  txt = sep.lines(txt)

  if (is.list(envir)) {
    envir = as.environment(envir)
    parent.env(envir) = enclos
    env = envir
  } else {
    env = new.env(envir)
  }

  xglue.by.placeholder = function(ph.dat, dat.var, by) {
    by.vals =
    dat$str
  }
  environment(xglue.by.placeholder) = env


  dat.vars = unique(unlist(strsplit(bdf$with,",",fixed=TRUE))) %>% na.omit
  for (dat.var in dat.vars) {

  }


  if (is.null(envir[["newline"]]))
    env$newline = "\n"

  txt.sep = rep("\n", length(txt))
  txt.sep[length(txt)] = ""

  # Remove sep blocks from bdf and txt
  sep.rows = which(bdf$type=="sep")
  for (row in sep.rows) {
    lines = bdf$start[row]:bdf$end[row]
    txt[lines] = ""
    txt.sep[lines] = ""
  }
  if (length(sep.rows)>0) bdf = bdf[-sep.rows,]

  bdf$glued = vector("list", NROW(bdf))


  xglue.one.obs = function(row) {
    restore.point("xglue.one.obs")
    # No data set
    dat.name = bdf$with[row]
    lines = (bdf$start[row]+1):(bdf$end[row]-1)
    txt.sep[max(lines)] = ""
    content = paste0(txt[lines], txt.sep[lines], collapse="")
    if (is.na(dat.name)) {
      str = glue(content,.envir=env, .open=open, .close=close, .trim=FALSE) %>%
        paste0(collapse=bdf$collapse[row])
      return(str)
    }

    dat = get(dat.name, envir)

    parent.by = bdf$parent.by[row]
    by = bdf$by[row]
    collapse = bdf$collapse[row]
    glue.fun = function(df) {
      restore.point("glue.fun")
      glue_data(df,content,.envir=env, .open=open, .close=close, .trim=FALSE) %>%
        paste0(collapse=collapse)
    }

    if (is.na(parent.by) & is.na(by)) {
      str = glue_fun(dat)
      return(str)
    } else if (!is.na(parent.by) & is.na(by)) {
      parent.by.vec = strsplit(parent.by,",",fixed = TRUE)[[1]]
      str = do.by(dat,glue.fun, by=parent.by.vec)
      return(str)
    } else if (is.na(parent.by) & !is.na(by)) {
      restore.point("jslkflksfjdkjfh")
      by.vec = strsplit(by,",",fixed = TRUE)[[1]]
      str = do.by(dat,glue.fun, by=by.vec)
      str = paste0(str, collapse=collapse)
      return(str)
    } else {
      stop("by and parent.by together not yet implemented.")
    }
  }

  levels = sort(unique(bdf$level))
  lev = max(levels)
  for (lev in rev(levels)) {
    rows = which(bdf$level==lev)
    row = first(rows)
    for (row in rows) {
      # Evaluate block content
      res = xglue.one.obs(row)
      str = unlist(res)
      bdf$glued[[row]] = str

      # Replace txt in block with str
      # and adapt txt.sep
      start = bdf$start[row]; end = bdf$end[row]
      txt[start:end] = ""
      txt.sep[start:end] = ""
      if (length(res)==1) {
        txt[start] = str
      } else {
        ph.id = paste0("._pLaCeh0Ld_", row)
        env[[ph.id]] = str
        ph.code = paste0("xglue.by.placeholder(", ph.id,")")
        txt[start] = paste0(open,ph.id,close)
      }
      if (start > 1) txt.sep[start-1] = ""
    }
  }

  str = paste0(txt, txt.sep, collapse="")
  res.str = glue(str,.envir=env, .open=open, .close=close, .trim=FALSE)

  #if (return.bdf)
  #  return(list(str=res.str, bdf=bdf))

  return(res.str)
}


parse.xglue.blocks = function(txt) {
  bdf = rmdtools::find.rmd.nested(txt) %>% filter(form=="block")
  bdf = bdf %>%
    mutate(head = paste0(type," ", arg.str))

  arg.li = strsplit(bdf$head,";")
  i = 3
  args = arg.li[[3]]
  arg.mat = do.call(rbind,lapply(arg.li, function(args) {
    extract.block.args.str(args)
  }))
  bdf = do.call(cbind, list(bdf, arg.mat)) %>%
    mutate(collapse=remove.quotes(collapse)) %>%
    mutate(collapse = gsub("<<newline>>","\n", collapse)) %>%
    mutate(collapse = ifelse(is.na(collapse),"",collapse))

  sep.rows = which(bdf$type=="sep")
  if (length(sep.rows)>0) {
    seps =  rmdtools::get.blocks.txt(txt, bdf[sep.rows,], inner=TRUE)
    bdf$collapse[bdf$parent[sep.rows]] = seps
    #bdf = bdf[-sep.rows,,drop=FALSE]
  }

  levels = sort(unique(bdf$level))

  bdf$parent.by = ""
  for (lev in setdiff(levels, min(levels))) {
    rows = bdf$level == lev
    prows = bdf$parent[rows]
    ppby = bdf$parent.by[prows]
    pby = bdf$by[prows]
    comma = ifelse(ppby=="" | pby=="","",",")

    bdf$parent.by[rows] = paste0(ppby,comma, pby)
    bdf$with[rows] = ifelse(is.na(bdf$with[rows]), bdf$with[prows], bdf$with[rows])
  }

  bdf$parent.by = ifelse(bdf$parent.by=="", NA, bdf$parent.by)

  bdf$content = get.blocks.txt(txt, bdf, inner=TRUE)

  bdf[,c("start", "end", "type","form", "level", "parent", "head", "with", "by", "collapse", "parent.by","content")
]
}

extract.block.args.str = function(args, types=c("with","by","collapse")) {
  restore.point("jsfkjdlf")
  res = rep(NA_character_, length(types))
  names(res) = types
  args = trimws(args)
  for (t in types) {
    arg = args[startsWith(args,t)]
    if (length(arg)==1) {
      res[t] = trimws(str.right.of(arg," ",not.found = ""))
    }
  }
  res
}


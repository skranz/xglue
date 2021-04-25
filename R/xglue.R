#' Perform xglue operation on a template text
#'
#' @param txt The template text on which xglue operations shall be performed
#' @param envir Environment or list that contains objects whose value is spliced in. By default the environment from which xglue is called.
#' @param open the opening string of glue whiskers
#' @param close the closing string of glue whiskers
#' @param pre.open the opening string of glue whiskers when pre compiling pre blocks
#' @param pre.close the closing string of glue whiskers when pre compiling pre blocks
#' @param enclos If envir is a list the enclosing environment.
#' @returns The glued text as a single character
#' @export
xglue = function(txt, envir=parent.frame(),open="{", close="}",pre.open="<<", pre.close=">>", enclos=parent.frame(), newline = "<<newline>>") {
  restore.point("xglue")
  txt = sep.lines(txt)

  # Remove ignore blocks
  bdf = rmdtools::find.rmd.nested(txt)
  ibdf = bdf %>% filter(type=="ignore")
  if (NROW(ibdf)>0) {
    remove.lines = unlist(lapply(1:NROW(ibdf), function(i) {
      ibdf$start:ibdf$end
    }))
    txt = txt[-remove.lines]
  }

  # Compile pre blocks
  txt = xglue.pre(txt,envir, open=pre.open, close=pre.close, enclos=enclos,bdf=bdf, newline=newline)

  bdf = parse.xglue.blocks(txt, newline=newline)
  
  
  if (is.list(envir)) {
    envir = as.environment(envir)
    parent.env(envir) = enclos
    env = envir
  } else {
    env = new.env(parent=envir)
  }

  if (is.null(bdf)) {
    str = glue(txt,.envir=env, .open=open, .close=close, .trim=FALSE)
    return(str)
  }

  #if (is.null(envir[["newline"]]))
  #  env$newline = "\n"

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

  edit = as.environment(
    list(txt=txt, txt.sep=txt.sep, cur.env=env, open=open, close=close, num.txt.change = 0)
  )
  glue.all.with.parent(0,edit, bdf)

  content = paste0(edit$txt, edit$txt.sep, collapse="")

  str = glue(content,.envir=env, .open=edit$open, .close=edit$close, .trim=FALSE)

  return(str)
}

#' Just compile pre blocks and return resulting text
#'
#' @param txt The template text on which xglue operations shall be performed
#' @param envir Environment or list that contains objects whose value is spliced in. By default the environment from which xglue is called.
#' @param open the opening string of the to be replaced glue whiskers in pre block
#' @param close the closing string of the to be replaced glue whiskers in pre block
#' @param enclos If envir is a list the enclosing environment.
#' @returns The resulting text of the template were pre blocks have been replaced
#' @export
xglue.pre = function(txt, envir=parent.frame(),open="<<", close=">>", enclos=parent.frame(), newline="<<newline>>", bdf=NULL) {
  restore.point("xlgue.pre")
  if (is.null(bdf))
    bdf = rmdtools::find.rmd.nested(txt)

  pre.bdf = bdf %>% filter(type=="pre")
  if (NROW(pre.bdf)==0) return(txt)
  ignore.lines = NULL
  for (i in 1:NROW(pre.bdf)) {
    start = pre.bdf$start[i];
    end = pre.bdf$end[i]
    pre.txt = txt[(start+1):(end-1)]
    ignore.lines = c(ignore.lines, start:end)

    str = xglue(pre.txt, envir=envir, open=open, close=close, enclos=enclos, newline=newline)

    txt[start:end] = ""
    line = pmax(start-1,1)
    txt[line] = paste0(txt[line],paste0(str, collapse = "\n"))
    txt
  }
  ignore.lines = setdiff(ignore.lines, 1)
  if (length(ignore.lines)>0) {
    txt = txt[-ignore.lines]
  }

  txt
}


glue.all.with.parent = function(parent = 0, edit,bdf) {
  restore.point("glue.all.with.parent")
  rows = which(bdf$parent == parent)

  for (row in rows) {
    type = bdf$type[row]
    if (type=="use") {
      glue.use.block(row, edit, bdf)
    } else if (type=="collapse") {
      glue.collapse.block(row, edit, bdf)
    } else {
      stop(paste0("Unknown block type ", type))
    }
  }
}


glue.use.block = function(row, edit, bdf) {
  restore.point("glue.use.block")
  org.edit = edit
  by.vars = strsplit(bdf$by[row],",",fixed=TRUE)[[1]] %>% trimws()
  dat.name = bdf$use[[row]]
  dat = try(get(dat.name, org.edit$cur.env))
  if (!is.data.frame(dat)) {
    stop(paste0("I did not find a data frame with name ", dat.name, " that you specify in your use block."))
  }

  split.li = split.by(dat,by.vars)

  str.li = rep("",length(split.li))

  # Get glue results for all splits
  for (si in seq_along(split.li)) {
    edit = as.environment(as.list(org.edit))
    edit$cur.env = new.env(parent=org.edit$cur.env)
    edit$cur.env[[".DATA"]] = as.data.frame(split.li[[si]])
    #edit$cur.env[[dat.name]] = split.li[[si]]
    str.li[si] = glue.collapse.block(row,edit, bdf)
  }
  str = paste0(str.li, collapse=bdf$collapse[row])
  edit = org.edit
  replace.block.edit.txt(str,row,edit,bdf)
  invisible(str)
}

glue.collapse.block = function(row, edit, bdf) {
  restore.point("glue.collapse.block")

  # xglue children
  glue.all.with.parent(row, edit, bdf)

  dat.name = bdf$use[row]
  content = get.block.content(row, edit, bdf)

  if (is.na(dat.name)) {
    str = glue(content,.envir=edit$cur.env, .open=edit$open, .close=edit$close, .trim=FALSE)
  } else {
    df = get(".DATA", edit$cur.env)
    str = glue_data(df,content,.envir=edit$cur.env, .open=edit$open, .close=edit$close, .trim=FALSE)
  }
  str = paste0(str, collapse=bdf$collapse[row])
  replace.block.edit.txt(str,row,edit,bdf)
  invisible(str)
}


get.block.content = function(row, edit, bdf) {
  start = bdf$start[row]; end = bdf$end[row]
  lines = (start+1):(end-1)
  edit$txt.sep[max(lines)] = ""
  content = paste0(edit$txt[lines], edit$txt.sep[lines], collapse="")
  content
}

replace.block.edit.txt = function(str,row, edit, bdf) {
  start = bdf$start[row]; end = bdf$end[row]
  edit$txt[start:end] = ""
  edit$txt.sep[pmax(start-1,1):end] = ""
  edit$txt[start] = str
  edit$num.txt.change = edit$num.txt.change +1
  invisible(edit)
}

parse.xglue.blocks = function(txt, newline="<<newline>>") {
  restore.point("parse.xglue.blocks")
  txt = sep.lines(txt)
  bdf = rmdtools::find.rmd.nested(txt) %>% filter(form=="block")
  if (NROW(bdf)==0) return(NULL)
  
  bdf = bdf %>%
    mutate(head = paste0(type," ", arg.str))

  arg.li = strsplit(bdf$head,";")
  arg.mat = do.call(rbind,lapply(arg.li, function(args) {
    extract.block.args.str(args)
  }))
  bdf = do.call(cbind, list(bdf, arg.mat)) %>%
    mutate(collapse=remove.quotes(collapse)) %>%
    mutate(collapse = gsub(newline,"\n", collapse)) %>%
    mutate(collapse = ifelse(is.na(collapse),"",collapse))

  sep.rows = which(bdf$type=="sep")
  if (length(sep.rows)>0) {
    seps =  rmdtools::get.blocks.txt(txt, bdf[sep.rows,], inner=TRUE)
    bdf$collapse[bdf$parent[sep.rows]] = seps
    #bdf = bdf[-sep.rows,,drop=FALSE]
  }

  levels = sort(unique(bdf$level))

  bdf$parent.by = ""
  bdf$by = bdf$group_by
  for (lev in setdiff(levels, min(levels))) {
    rows = bdf$level == lev
    prows = bdf$parent[rows]
    ppby = bdf$parent.by[prows]
    pby = bdf$by[prows]
    comma = ifelse(ppby=="" | pby=="","",",")

    bdf$parent.by[rows] = paste0(ppby,comma, pby)
    bdf$use[rows] = ifelse(is.na(bdf$use[rows]), bdf$use[prows], bdf$use[rows])
  }

  bdf$parent.by = ifelse(bdf$parent.by=="", NA, bdf$parent.by)

  bdf$content = get.blocks.txt(txt, bdf, inner=TRUE)

  bdf[,c("start", "end", "type","form", "level", "parent", "head", "use", "by", "collapse", "parent.by","content")
]
}

extract.block.args.str = function(args, types=c("use","group_by", "collapse")) {
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


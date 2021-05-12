#' Find nested blocks in a text
#' 
#' Blocks must start and end with separate header
#' and trailer lines, respectively e.g.
#' 
#' #< collapse ", "
#' 
#' #>
#' 
#' @param txt a character vector
#' @param open start of a block's header line, by default "#<"
#' @param close start of a block's trailer line by default "#>".
#' @export
find.nested.blocks = function(txt, open = "#<", close="#>") {
  restore.point("find.nested.blocks")

  df = find.blocks(txt, open, close)
  if (is.null(df)) {
    return(fast_df(start=integer(0), end=integer(0), type=character(0), arg.str=character(0), level=integer(0), parent=integer(0)))
  }
  
  df = df %>% 
    arrange(start,-end)
  df$level = get.start.end.levels(df$start, df$end)
  df$parent = get.levels.parents(df$level)
  df
}

get.levels.parents = function(levels, is.parent.type=NULL) {
  n = length(levels)
  if (n==0) return(NULL)
  parents = rep(0,n)
  min.level = levels[1]
  if (min.level != 1)
    levels = levels-min.level+1

  num.levels = max(levels)

  if (is.null(is.parent.type)) {
    level.parent = rep(NA_integer_,num.levels)
    for (i in 1:n) {
      if (levels[i]>1) {
        parents[i] = level.parent[levels[i]-1]
      }
      level.parent[levels[i]] = i
    }
  } else {
    level.parent = rep(0,num.levels)
    # using is.parent.type
    parent.level = 0
    for (i in 1:n) {
      if (levels[i]-1 < parent.level) {
        parent.level= levels[i]-1
        level.parent[(parent.level+1):num.levels] = 0
      }
      if (parent.level>0) {
        parents[i] = level.parent[parent.level]
      }
      if (is.parent.type[i]) {
        parent.level = levels[i]
        level.parent[parent.level] = i
      }
    }
  }

  parents
}

get.start.end.levels = function(start, end, start.level = 1L) {
  n = length(start)
  if (n==0) return(NULL)
  levels = rep(start.level,n)
  end.stack = rep(NA_integer_,n)
  end.stack.ind = 0

  level = start.level
  for (i in 1:n) {
    if (end.stack.ind >0) {
      while (start[i]>end.stack[end.stack.ind]) {
        level = level-1L
        end.stack.ind = end.stack.ind -1L
        if (end.stack.ind == 0) break
      }
    }
    levels[i] = level
    level = level+1L
    end.stack.ind = end.stack.ind+1L
    end.stack[end.stack.ind] = end[i]
  }
  levels
}

find.blocks = function(txt, open = "#<", close="#>") {
  restore.point("find.blocks")

  start = which(startsWith(txt,open))
  end = which(startsWith(txt,close))

  res = unmatched.blocks.diagnostics(txt, start=start, end=end, open=open, close=close)
  if (!res$ok) {
    stop(res$msg)
  }

  if (length(start)==0) return(NULL)

  blocks = match.blocks.start.end(start, end)
  start_row = start
  end_row = end[blocks[,2]]
  #cbind(start_row, end_row)

  str = txt[start]
  str = str.trim(str.right.of(txt[start],"#< "))

  #type = str.right.of(str,dot.start) %>% str.trim %>% str.left.of(" ")
  #arg.str = str.right.of(str,dot.start) %>%

  type = str.trim(str.left.of(str," "))
  arg.str = str.right.of(str,type) %>% str.trim

  if (any(is.na(type))) {
    msg = paste0(start[is.na(type)],": ", txt[start[is.na(type)]], collapse = "\n")
    stop(paste0("Could not parse types in the following lines:\n\n",msg))
  }

  fast_df(start=start_row, end=end_row, type=type, arg.str=arg.str)
}


unmatched.blocks.diagnostics = function(txt, start = which(startsWith(txt,open)), end = which(startsWith(txt,close)), open="#<", close="#>") {
  restore.point("unmatched.blocks.diagnostics")

  # first check if a block closes somewhere
  # witihout blocks being open
  n = min(length(start),length(end))
  line = NULL
  if (n > 0) {
    prev.closed = which(end[1:n]<start[1:n])
    if (length(prev.closed)>0) {
      line = min(end[prev.closed])
    }
  }
  if (!is.null(line)) {
    msg = paste0("On line ", line, " you close a block while no block is open")
    return(list(ok=FALSE, msg=msg, lines=line))
  }

  if (length(start)>length(end)) {
    extra.end = length(txt)+1:(length(end)-length(start))
    blocks = as_data_frame(match.blocks.start.end(start, c(end,extra.end)))

    bi = blocks %>%
      mutate(unclosed = end_ind > length(end)) %>%
      filter(unclosed) %>%
      mutate(start = start[start_ind], title=txt[start])

    msg = paste0("You have the following unclosed block(s):\n", paste0("\n\tline ",bi$start, ": ", bi$title, collapse=""))
    return(list(ok=FALSE, msg=msg, lines=start))
  } else if (length(end)>length(start)) {
    n = length(start)
    lines = end[(n+1):length(end)]

    msg = paste0("On the following line(s) you close a block while no block is open:\n", paste0(lines, collapse=", "))
    return(list(ok=FALSE, msg=msg, lines=lines))
  }

  return(list(ok=TRUE,msg="",lines=NULL))
}

get.blocks.txt = function(txt, block.df, inner=FALSE) {
  if (NROW(block.df)==0) return(character(0))
  sapply(1:NROW(block.df), function(row) {
    paste0(txt[(block.df$start[row]+inner):(block.df$end[row]-inner)], collapse="\n")
  })
}


match.blocks.start.end = function(start, end) {
  restore.point("match.blocks.start.end")

  end_pos = start_stack = rep(NA, length(start))
  start_stack_ind = 1
  start.i = 1
  end.i= 1
  start_stack[1] = 1
  start = c(start, Inf)
  while (TRUE) {
    top_ind = start_stack[start_stack_ind]

    # Add next start.i to start stack
    start.i = start.i+1

    # Try to clear start_stack
    while (end[end.i]<start[start.i]) {

      end_pos[top_ind] = end.i
      if (start[top_ind]>end[end.i]) {
        stop(paste0("A block closes in position (line) ", end[end.i], " but there is no open block."))
      }
      start_stack_ind = start_stack_ind-1
      end.i = end.i+1

      #cat("\ndel start_stack: ", paste0(start_stack[1:start_stack_ind],
      #"(",start[start_stack[1:start_stack_ind]],")"))

      if (start_stack_ind == 0) break

      top_ind = start_stack[start_stack_ind]
    }

    if (start.i >= length(start)) break

    start_stack_ind = start_stack_ind+1
    start_stack[start_stack_ind] = start.i
    #cat("\nadd start_stack: ", paste0(start_stack[1:start_stack_ind],
    #  "(",start[start_stack[1:start_stack_ind]],")"))

  }
  cbind(start_ind=seq_along(start[-length(start)]), end_ind=end_pos)
}


parse.block.args = function(header, arg.str=NULL, add.type = TRUE, type = "", allow.unquoted.title=FALSE) {
  restore.point("parse.block.args")

  if (is.null(arg.str)) {
    str = header
    str = str.trim(str.right.of(str,"#< "))
    type = str.left.of(str," ")
    arg.str = str.right.of(str," ")
  }

  if (allow.unquoted.title) {
    arg.str = str.trim(arg.str)
    first = substring(arg.str,1,1)
    is.list = (grepl(",",arg.str,fixed=TRUE) & grepl("=",arg.str,fixed=TRUE))
    is.quoted = first == "'" | first == '"'
    if (is.list & !is.quoted) {
      stop(paste0("If your ", type, ' title contains the character "," and "=" you must quote it, like "my title", to distinguish it from a parameter list.'))
    }
    if (!is.list & !is.quoted) {
      return(list(name=arg.str, type=type))
    }


  }

  code = paste0("alist(",arg.str,")")
  li = try(eval(base::parse(text=code,srcfile=NULL)), silent=TRUE)
  if (is(li,"try-error")) {
    # check if there is a , and a = suggesting a list
    if (!allow.unquoted.title | (grepl(",",code,fixed=TRUE) & grepl(",",code,fixed=TRUE))) {
      stop("I cannot parse your block arguments ", arg.str, " as a list in R. Perhaps you have to add quotes around some arguments, like the title.")
    }
    # if not, just treat the whole argument as title
    li = list(name = arg.str)
  } else {
    li =  lapply(li, function(el) {
        res = try(eval(el, envir=baseenv()), silent=TRUE)
        if (is(res,"try-error")) return(as.character(el))
        res
    })
  }

  if (add.type) {
    if (length(li)==0) return(list(name=NULL, type=type))

    if (is.null(names(li))) {
      return(list(type=type,name=li[[1]]))
    } else if (nchar(names(li)[1]) == 0) {
      return(c(list(type=type,name=li[[1]]),li[-1]))
    }

  } else {
    if (length(li)==0) return(list(name=NULL))

    if (is.null(names(li))) {
      return(list(name=li[[1]]))
    } else if (nchar(names(li)[1]) == 0) {
      return(c(list(name=li[[1]]),li[-1]))
    }
  }
  li
}

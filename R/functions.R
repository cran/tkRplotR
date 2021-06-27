#' @export
.selectPngType <- function(){

  #TODO
  #catchError <-  try(grDevices::png(
  # filename = "tkRplotR_test.png",
  #       type ="error",
  #       width = 100,
  #       height = 100
  #     ), silent = TRUE)
  #
  # strsplit(catchError[[1]],",")[[1]][-1]
  #
  selectedPngType <- getOption("bitmapType")
  pngTypes <- c(NULL,"Xlib", "cairo","cairo-png")

  switch(Sys.info()[1],
         Darwin = pngTypes <-
           c(NULL, "quartz",  "Xlib","cairo"),
         windows = pngTypes <-
           c(NULL, "windows","Xlib","cairo","cairo-png"))

  try(
  for (pngType in pngTypes) {
    fp <- tempfile(pattern = "tkRplotR.",
                   tmpdir = tempdir(),
                   fileext = ".png")

    grDevices::png(
      filename = fp,
      type = pngType,
      width = 100,
      height = 100
    )

    par(oma = rep(0, 4))
    par(mar = rep(0, 4))
    plot(
      1,
      type = "n",
      ann = FALSE,
      axes = FALSE,
      xlim = c(0, 1),
      ylim = c(0, 1)
    )
   suppressWarnings(rect(.5, -1, 2, 2, col = rgb(1, 0, 0, .5), border = NA))
    dev.off()
    imageId <- paste0("TkRplot")
    image <- tcltk::tkimage.create("photo", imageId , file = fp)

    if ((tcltk::tclvalue(tcltk::.Tcl("TkRplot get 0 0")) != tcltk::tclvalue(tcltk::.Tcl("TkRplot get 50 50")))) {
      selectedPngType <- pngType
      break
    }
  }, silent=TRUE)

  return(selectedPngType)
}

#' @export
.tcl2num <- function(x) {
  as.numeric(unlist(strsplit(tclvalue(x), split = " ")))
}
#' @export
.tcl2str <- function(x) {
  toString(unlist(strsplit(tclvalue(x), split = " ")))
}
#' @export
.tcl2String <- function(x) {
  toString(tclvalue(x))
}


#' @import grDevices
#' @import tcltk
#' @importFrom graphics par plot rect
#'
#'
#' @export
.tkRplotNewId <- local({
  id <- 0
  function() {
    id <<- id + 1
    return(id)
  }
})

#' @export .is.string
.is.string <- function(x)
  is.character(x) && length(x) == 1

#' @export .isTclImgOk
.isTclImgOk <- function() {
  if (as.numeric(R.version$major) < 4 &&
      as.numeric(R.version$minor) < 4 |
      as.character(tcltk::tcl("info", "tclversion")) <= "8.5") {
    packageStartupMessage("Please install 'Img' extension for tcltk",
                          tcltk::tclVersion())
    return(FALSE)
  }
  return(TRUE)
}
# .isTclImgOk <- function() {
#   if (as.numeric(R.version$minor) < 4 |
#       as.character(tcltk::tcl("info", "tclversion")) <= "8.5") {
#     packageStartupMessage("Please install 'Img' extension for tcltk ",
#                           tcltk::tclVersion())
#     return(FALSE)
#   }
#   # packageStartupMessage(paste("tcltk", as.character(tcltk::tcl(
#   #   "info", "tclversion"
#   # ))))
#   return(TRUE)
# }

#' @export .tclFun0
#'
.tclFun0 <- function(f,
                      name = deparse(substitute(f)))
{
  name <- sub(".", 'dot_', name)
  name <- paste("R", make.names(name[1]), sep = "_")
  res <-
    paste(strsplit(.Tcl.callback(f), " ")[[1]][1:2], collapse = " ")

  if (length(grep("R_call ", res) > 0)) {
    .Tcl(paste("proc ",
               name,
               " args {",
               res,
               "}",
               sep = ""))
  }
  return(res)
}

#' @export .tclFun1
.tclFun1 <- function(f,
                     name = deparse(substitute(f)),
                     arg1 = names(formals(f))[1])
{
  name <- sub(".", 'dot_', name)
  name <- paste("R", make.names(name[1]), sep = "_")
  #res <- .Tcl.callback(f)
  #res <- strtrim(res, 16)
  res <- paste(strsplit(.Tcl.callback(f), " ")[[1]][1:2], collapse = " ")
  if (length(grep("R_call ", res) > 0)) {
    .Tcl(paste(
      "proc ",
      name,
      " {",
      paste(arg1, collapse = " "),
      "} {",
      res,
      " ",
      paste0("$", arg1, collapse = " "),
      "}",
      sep = ""
    ))
  }
  return(res)
}


#' @export .tclFun
.tclFun <- function(f,
                     name = deparse(substitute(f)),
                     args = names(formals(f)))
{
  name <- sub(".", 'dot_', name)
  name <- paste("R", make.names(name[1]), sep = "_")
  res <- paste(strsplit(.Tcl.callback(f), " ")[[1]][1:2], collapse = " ")
  if (length(grep("R_call ", res) > 0)) {
    .Tcl(paste(
      "proc ",
      name,
      " {",
      paste(args, collapse = " "),
      "} {",
      res,
      " ",
      paste0("$", args, collapse = " "),
      "}",
      sep = ""
    ))
  }
  return(res)
}

#' @export
.tclProcExist <- local({
  .Tcl('proc ProcExists p {
       return uplevel 1 [expr {[llength [info procs $p]] > 0}]
}')
     function(what) {
       .Tcl(paste('ProcExists', what))
       }
  })

#' @export
# .tclTraceAddVariableWrite <- function(var, fun=.tclFun(.tkRreplot), args='', funName = deparse(substitute(fun))){
#   IsFunction <- try(is.function(fun), silent = TRUE)
#   if (IsFunction) {
#     tclFun <- .tclFun(fun)
#   } else{
#     tclFun <- fun
#   }
#
#   .Tcl(paste0('proc trace_', funName ,' {name element op} {
# puts $arg
#             # In case of array variable tracing, the "element" variable will specify the array index
#             # For scalar variables, it will be empty
#             if {$element != ""} {
#             set name ${name}($element)
#             }
#             upvar $name x\n' , tclFun, '
#             }
#             # Adding tracing
#             trace add variable ', var, ' { write } trace_', funName)
#   )
# }
.tclTraceAddVariableWrite <- function(var, fun=.tkRreplot, arg='', funName = deparse(substitute(fun))){
  IsFunction <- try(is.function(fun), silent = TRUE)
  if (IsFunction) {
    tclFun <- .tclFun(get(funName), name = funName)
    funName <- sub(".", 'dot_', funName)
    funName <- paste("R", make.names(funName[1]), sep = "_")
  } else{
    funName <- tclFun <- fun
  }
  #message(tclFun)
  .Tcl(paste0('proc trace_', funName ,' {arg name element op} {
              # puts "$arg $name $op $element"
              # In case of array variable tracing, the "element" variable will specify the array index
              # For scalar variables, it will be empty
              if {$element != ""} {
              set name ${name}($element)
              }
              upvar $name x
              ' , funName, ' $arg
}
# Adding tracing
trace add variable ', var, ' { write } "trace_', funName,' ', arg,'"')
   )
   }


#' @export
.addTraceTclVariables <- function(tt, tcl_vars = NULL) {
  if (is.null(tcl_vars)) {
    vars <- all.vars(body(tt$env$fun), functions = FALSE)
    tcl_vars <- vars[which(lapply(vars, function(x) {
      if (exists(x))
        class(get(x))
    }) == 'tclVar')]
  }
  tt$env$tclVars <- tcl_vars
  invisible(lapply(tcl_vars, function(x)
    .tclTraceAddVariableWrite(get(x), arg = tt)))
}
#' @export
.getToplevelID <- function(win) {
  if (is.tkwin(win)) {
    win <- win$ID
  }
  return(paste0(".", strsplit(win, split = "\\.")[[1]][2]))
  #tclvalue(tkwinfo("toplevel", win))
}

#' @export
.getToplevel <- function(win) {
  getVariable(.getToplevelID(win))
}

# getTime <- function() {
#   #as.numeric(tclvalue(.Tcl("clock milliseconds")))
#   as.numeric(Sys.time()) * 1000
# }

#' @title Define Tk Binds To Allow Automatic Resizing
#' @description
#' Add binds to automatically resize the graph
#' @param parent parent Tk toplevel window
#' @param expose if TRUE update graph when the window is expose
#' @param configure if TRUE update the graph when the window is update
#' @details
#' This function adds the binds needed to automatically resize the graph
#' @return NULL
#' @export
#' @examples
#' \dontshow{
#' if (.isTclImgOk()){
#' tkbb <- tclVar(1)
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function() {
#' b <- .tcl2num(tkbb)
#'  x <- 1:20 / 20
#'    plot(
#'    x,
#'    x ^ b,
#'    col = "#0000ff50",
#'        xlab = "x",
#'            ylab = paste0("x^", b),
#'                type = "l",
#'                    axes = FALSE,
#'                        lwd = 4)
#'    title(main = b)
#'      points(x,
#'       x ^ b,
#'       col = "#ff000050",
#'       pch = 19,
#'       cex = 2)
#'         axis(1)
#'         axis(2)
#'           box()
#'           })
#'
#'      s <-
#'        tkscale(
#'        tt,
#'        from = 0.05,
#'        to = 2.00,
#'        variable = tkbb,
#'        showvalue = FALSE,
#'        resolution = 0.05,
#'        orient = "horiz"
#'        )
#'
#'        tkpack(s,
#'        side = "bottom",
#'        before = tt$env$canvas,
#'        expand = FALSE,
#'        fill = "both")
#'
#'  # to disable the automatic resizing of the graph
#'    tkBinds(parent = tt, expose = FALSE, configure = FALSE)
#'  Sys.sleep(1)
#'  # to enable again the automatic resising
#'  # tkBinds(parent = tt, expose = TRUE, configure = TRUE)
#'  Sys.sleep(1)
#'  tkdestroy(tt)
#'}
#' }
#' \dontrun{
#' tkbb <- tclVar(1)
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function() {
#' b <- .tcl2num(tkbb)
#'  x <- 1:20 / 20
#'    plot(
#'    x,
#'    x ^ b,
#'    col = "#0000ff50",
#'        xlab = "x",
#'            ylab = paste0("x^", b),
#'                type = "l",
#'                    axes = FALSE,
#'                        lwd = 4)
#'    title(main = b)
#'      points(x,
#'       x ^ b,
#'       col = "#ff000050",
#'       pch = 19,
#'       cex = 2)
#'         axis(1)
#'         axis(2)
#'           box()
#'           })
#'
#'        s <-
#'        tkscale(
#'        tt,
#'        from = 0.05,
#'        to = 2.00,
#'        variable = tkbb,
#'        showvalue = FALSE,
#'        resolution = 0.05,
#'        orient = "horiz"
#'        )
#'
#'        tkpack(s,
#'        side = "bottom",
#'        before = tt$env$canvas,
#'        expand = FALSE,
#'        fill = "both")
#'
#'  # to disable the automatic resizing of the graph
#'    tkBinds(parent = tt, expose = FALSE, configure = FALSE)
#'
#'  # to enable again the automatic resising
#'  # tkBinds(parent = tt, expose = TRUE, configure = TRUE)
#' }
###### tkBinds #####
tkBinds <- function(parent,
                    expose = TRUE,
                    configure = TRUE) {
  if (configure) {
    tkbind(parent$env$canvas, "<Configure>", function() {
      #print("Configure")
      width <-
        as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
      height <-
        as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
      widthPrevious <- parent$env$width
      heightPrevious <- parent$env$height
      if (any(isTRUE(widthPrevious != width),
              isTRUE(heightPrevious != height)))  {
        #print("ConfigureDone")
        .tkRreplot(parent)
      }
    })
  } else {
    tkbind(parent$env$canvas, "<Configure>", "")
  }

  if (expose) {
    tkbind(parent$env$canvas, "<Expose>", function() {
      width <-
        as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
      height <-
        as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
      widthPrevious <- parent$env$width
      heightPrevious <- parent$env$height
      if (any(isTRUE(widthPrevious != width),
              isTRUE(heightPrevious != height)))  {
        # print("ExposeDone")
        .tkRreplot(parent)
      }
    })
  } else {
    tkbind(parent$env$canvas, "<Expose>", "")
  }

  tkbind(parent, "<FocusIn>", function() {
    width <-
      as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
    height <-
      as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
    widthPrevious <- parent$env$width
    heightPrevious <- parent$env$height
    if (any(isTRUE(widthPrevious != width),
            isTRUE(heightPrevious != height)))  {
      tkRreplot(parent)
    }
  })

  tkbind(parent, "<Enter>", function(W) {
    setVariable("tkRplotR_currentToplevel", W)
  })
  tkbind(parent$env$canvas, "<Enter>", function(W) {
    setVariable("tkRplotR_currentToplevel", W)
    width <- as.numeric(.Tcl(paste("winfo width", W)))
    height <- as.numeric(.Tcl(paste("winfo height", W)))
    widthPrevious <- parent$env$width
    heightPrevious <- parent$env$height
    if (any(isTRUE(widthPrevious != width),
            isTRUE(heightPrevious != height)))  {
      tkRreplot(parent)
    }
    setCoef(parent)
  })
}


#' @title Add Tk Binds
#' @description
#' Add binds to previous defined bindings
#' @param win window
#' @param event event
#' @param fun a function
#' @details
#' This function adds a new bind while keeping the previous defined binds.
#' @return NULL
#' @export
#' @examples
#' \dontshow{
#' if (.isTclImgOk()){
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function () plot(1:10))
#' FUN <- local({
#'   canPos <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'     function (x, y) {
#'         x <- as.numeric(x)
#'         y <- as.numeric(y)
#'        tkdelete(tt$env$canvas, tclvalue(canPos))
#'            xy <- formatC(tk2usr(x, y),
#'                    digits = 2,
#'                      format = "f",
#'                        width = 5)
#'     canPos <<- .Tcl(
#'      paste(tt$env$canvas, "create text 40 10 -fill blue -justify left -text { ",
#'             xy[1], " ", xy[2],
#'             "} -font {Helvetica -10}"))
#'   }})
#'
#' tkbind(tt$env$canvas, "<Motion>", FUN)
#' tkbind(tt$env$canvas, "<Motion>") #to give current bidings
#' FUN1 <- function (x,y) print(tk2usr(x,y))
#' addTkBind(tt$env$canvas, "<Motion>", FUN1)
#' tkbind(tt$env$canvas, "<Motion>") #to give current bidings
#' Sys.sleep(1)
#' tkdestroy(tt)
#' }
#' }
#' \dontrun{
#'
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function () plot(1:10))
#' FUN <- local({
#'   canPos <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'     function (x, y) {
#'         x <- as.numeric(x)
#'         y <- as.numeric(y)
#'        tkdelete(tt$env$canvas, tclvalue(canPos))
#'            xy <- formatC(tk2usr(x, y),
#'                    digits = 2,
#'                      format = "f",
#'                        width = 5)
#'     canPos <<- .Tcl(
#'      paste(tt$env$canvas, "create text 40 10 -fill blue -justify left -text { ",
#'             xy[1], " ", xy[2],
#'             "} -font {Helvetica -10}"))
#'   }})
#'
#' tkbind(tt$env$canvas, "<Motion>", FUN)
#' tkbind(tt$env$canvas, "<Motion>") #to give current bidings
#' FUN1 <- function (x,y) print(tk2usr(x,y))
#' addTkBind(tt$env$canvas, "<Motion>", FUN1)
#' tkbind(tt$env$canvas, "<Motion>") #to give current bidings
#' }

###### addTkBind #####
addTkBind <- function(win, event, fun = NULL) {
  if (is.null(fun))
    tkbind(win, event, "")
  if (is.function(fun))
    fun <- .Tcl.callback(fun)
  if (tclvalue(tkbind(win, event)) == "") {
    tkbind(win, event, fun)
  } else{
    tkbind(win, event, paste("+", fun))
  }
}


###### globalVariable #####
# globalVariable <- local({
#   globalEnv <- new.env(parent = emptyenv())
#   globalVars <- NULL
#   function(type = NULL,
#            name = NULL,
#            value) {
#     if (!is.null(name)) {
#       if (type == "set") {
#         assign(name, value, envir = globalEnv)
#         assign("globalVars", sort(unique(c(
#           globalVars, name
#         ))), envir = environment(globalVariable))
#         return(invisible(value))
#       }
#       if (type == "get") {
#         if (!name %in% globalVars){
#           return(message("First define variable"))}
#         return(get(name, envir = globalEnv))
#       }
#       if (type == "rm") {
#         assign("globalVars", globalVars[!globalVars %in% name],
#                envir = environment(globalVariable))
#         assign(name, NULL, envir = globalEnv)
#       }
#     }
#
#     if (is.null(type))
#       return(ls(envir = environment(globalVariable), all.names = TRUE))
#     if (type == "ls")
#       return(globalVars)
#     if (type == "env")
#       return(globalEnv)
#     # if (type == "all2r")
#     #   invisible(lapply(
#     #     globalVars,
#     #     FUN = function(x)
#     #       assign(x, get(x, envir = globalEnv), envir = .GlobalEnv)
#     #   ))
#   }
# })

# @export
#' @keywords internal
globalVariable <- local({
  globalEnv <- new.env(parent = emptyenv())

  assign("tkRplotR_pngType", .selectPngType(), envir = globalEnv)
  globalVars <- "tkRplotR_pngType"
  function(type = NULL,
           name = NULL,
           value,
           where = .GlobalEnv) {
    switch(
      type,
      get = {
        if (!name %in% globalVars) {
          globalVariable("set", name, value)
          return(value)
        }
        return(get(name, envir = globalEnv))
      },
      set = {
        assign(name, value, envir = globalEnv)
        assign("globalVars", sort(unique(c(
          globalVars, name
        ))), envir = environment(globalVariable))
        return(invisible(value))
      },
      rm = {
        nameIndex <- globalVars %in% name
        globalVars <<- globalVars[!nameIndex]
        if (any(nameIndex))
        return(rm(list = name, envir = globalEnv))
      },
      ls = {
        return(globalVars)
      },
      env = {
        return(globalEnv)
      },
      all2R = {
        type <- "all2r"
      }
    )
    if (type == "all2r") {
      return(invisible(lapply(
        globalVars,
        FUN = function(x)
          assign(x, get(x, envir = globalEnv), envir = where)
      )))
    }
  }
})


#' @title Set, Get, and Remove Variables
#' @description
#' Define, get, and remove variables
#' @aliases
#' setVariable
#' getVariable
#' rmVariable
#' @usage
#' setVariable(name, value = NULL)
#' getVariable(name, value = NULL)
#' rmVariable(name)
#' @param name name of the variable
#' @param value the value of the variable
#' @export
#' @examples
#' setVariable("var1", 1)
#' exists("var1")
#' getVariable("var1")
#' rmVariable("var1")
#' getVariable("var1")
#' getVariable("tkRplotR_pngType")
#'

###### setVariable  #####
setVariable <- function(name, value = NULL) {
  globalVariable(type = "set", name, value = value)
}

#' @export

###### getVariable #####
getVariable <- function(name, value = NULL) {
   return(globalVariable(type = "get", name, value))
}


#' @export
###### rmVariable #####
rmVariable <- function(name) {
  globalVariable(type = "rm", name)
}


getEnv <- function(){
  globalVariable("env")
  }

getAllVariables <- function(all.names = TRUE){
  ls(getEnv(), all.names = all.names)
  }


#' @title Tk Rplot With Resizing
#' @description
#' Displays a plot in a Tk toplevel window.
#' @aliases tkRreplot .tkRreplot
#' @usage tkRplot(W, fun, width = 490, height = 490, ...)
#' tkRreplot(W, fun, width, height,  ...)
#' .tkRreplot(W)
#' @param W Tk toplevel window
#' @param fun function to produce the plot
#' @param width image width
#' @param height image height
#' @param ... additional arguments
#' @export
#' @examples
#' \dontshow{
#'  if (.isTclImgOk()){
#' tkbb <- tclVar(1)
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function() {
#' bb <- .tcl2num(tkbb)
#'  x <- 1:20 / 20
#'    plot(
#'    x,
#'    x ^ bb,
#'    col = "#0000ff50",
#'        xlab = "x",
#'            ylab = paste0("x^", bb),
#'                type = "l",
#'                    axes = FALSE,
#'                        lwd = 4)
#'    title(main = bb)
#'      points(x,
#'       x ^ bb,
#'       col = "#ff000050",
#'       pch = 19,
#'       cex = 2)
#'         axis(1)
#'         axis(2)
#'           box()
#'           })
#'
#'      s <-
#'        tkscale(
#'        tt,
#'        from = 0.05,
#'        to = 2.00,
#'        variable = tkbb,
#'        showvalue = FALSE,
#'        resolution = 0.05,
#'        orient = "horiz"
#'        )
#'
#'        tkpack(s,
#'        side = "bottom",
#'        expand = FALSE,
#'        before = tt$env$canvas,
#'        fill = "both")
#'
#'        Sys.sleep(1)
#'        tkdestroy(tt)
#'        }
#' }
#' \dontrun{
#' #Example 1 without using tkReplot function (tkRplotR version > 0.1.6)
#' tk_b <- tclVar(1)
#' tk_x <- tclVar(10)
#' tk_main <- tclVar('...')
#'
#' tt0 <- tktoplevel()
#' tt0 <- tkRplot(tt0, function(...) {
#' # get values of tclvariables
#'   x <- .tcl2num(tk_x)
#'   x <- 1:x
#'   b <- .tcl2num(tk_b)
#'   main <- .tcl2String(tk_main)
#'
#'   plot(
#'     x,
#'     x ^ b ,
#'     col = "#0000ff50",
#'     xlab = "x",
#'     ylab = expression(x^b),
#'     type = "l",
#'     axes = FALSE,
#'     lwd = 4)
#'   title(main = main)
#'   points(x,
#'          x ^ b,
#'          col = "#ff000050",
#'          pch = 19,
#'          cex = 2)
#'   axis(1)
#'   axis(2)
#'   box()
#' })
#'
#' s01 <- tkscale(
#'   tt0,
#'   #command = function(...) .tkRreplot(tt0),
#'   from = 10,
#'   to = 60,
#'   label = 'x',
#'   variable = tk_x,
#'   showvalue = TRUE,
#'   resolution = 1,
#'   repeatdelay = 200,
#'   repeatinterval = 100,
#'   orient = "hor"
#' )
#'
#' s02 <- tkscale(
#'   tt0,
#'   #command = function(...) .tkRreplot(tt0),
#'   from = 0.05,
#'   to = 2.00,
#'   label = 'b',
#'   variable = tk_b,
#'   showvalue = TRUE,
#'   resolution = 0.01,
#'   repeatdelay = 200,
#'   repeatinterval = 100,
#'   orient = "ver"
#' )
#'
#'
#' e01 <- tkentry(tt0,
#'               textvariable = tk_main,
#'               validate = 'all', validatecommand="")
#' tkpack(s02,
#'        side = "left",
#'        expand = FALSE,
#'        #'anchor = "c",
#'        before = tt0$env$canvas,
#'        fill = "both")
#'
#' tkpack(s01,
#'        side = "bottom",
#'        expand = FALSE,
#'        #'anchor = "c",
#'        before = tt0$env$canvas,
#'        fill = "both")
#'
#' tkpack(e01,
#'        side = "top",
#'        expand = FALSE,
#'        #'anchor = "c",
#'        before = tt0$env$canvas,
#'        fill = "both")
#'
#'
#' #Example 2 using tkReplot function (tkRplotR version < 0.1.7)
#' bb <- 1
#' tkbb <- tclVar(1)
#' tt <- tktoplevel()
#' f <- function(...) {
#'  b <- as.numeric(tclvalue(tkbb))
#'    if (b != bb) {
#'        bb <<- b
#'            tkRreplot(tt)
#'              }
#'      }
#'
#' tt <- tkRplot(tt, function() {
#'  x <- 1:20 / 20
#'    plot(
#'    x,
#'    x ^ bb,
#'    col = "#0000ff50",
#'        xlab = "x",
#'            ylab = paste0("x^", bb),
#'                type = "l",
#'                    axes = FALSE,
#'                        lwd = 4)
#'    title(main = bb)
#'      points(x,
#'       x ^ bb,
#'       col = "#ff000050",
#'       pch = 19,
#'       cex = 2)
#'         axis(1)
#'         axis(2)
#'           box()
#'           })
#'
#'   s <- tkscale(
#'        tt,
#'        command = f,
#'        from = 0.05,
#'        to = 2.00,
#'        variable = tkbb,
#'        showvalue = TRUE,
#'        resolution = 0.01,
#'        repeatdelay = 50,
#'        repeatinterval = 100,
#'        orient = "horiz"
#'        )
#'
#'        tkpack(s,
#'        side = "bottom",
#'        expand = FALSE,
#'        before = tt$env$canvas,
#'        fill = "both")
#'
#'}

###### tkRplot #####


tkRplot <- function(W,
                    fun,
                    width = 490,
                    height = 490,
                    ...) {
  if (.getToplevelID(W) != W$ID) {
    stop("Please use a toplevel widget!")
  }
  setVariable("tkRplotR_currentToplevel", W)
  setVariable(.Tk.ID(W), W)
  fp <-
    tempfile(pattern = "tkRplotR.",
             tmpdir = tempdir(),
             fileext = ".png")
  on.exit(unlink(fp))
  png(
    filename = fp ,
    type = getVariable("tkRplotR_pngType"),
    width = width,
    height = height
  )
  plotOk <- try(fun(...),  silent = TRUE)
  if (inherits(plotOk, "try-error"))
    return(dev.off())
  W$env$usr <- setVariable("tkRplotR_usr" , par("usr"))
  W$env$plt <- setVariable("tkRplotR_plt" , par("plt"))
  dev.off()
  imageId <- paste0("TkRplot", W$ID)
  image <- tkimage.create("photo", imageId , file = fp)
  W$env$canvas <-
    tkcanvas(W,
             relief = "flat",
             borderwidth = 0,
             background =  "white")
  tkconfigure(W$env$canvas, width = width, height = height)
  tkpack(W$env$canvas,
         anchor = "center",
         fill = "both",
         expand = 1)
  tcl(W$env$canvas,
      "create",
      "image",
      0,
      0,
      image = image  ,
      anchor = "nw")

  tkbind(W$env$canvas, "<Destroy>", function() {
    .Tcl(paste("image delete", image))
  })
  W$env$imageId <- imageId
  W$env$image <- image
  W$env$fun <- fun
  #tkconfigure(W$env$canvas, width = width - 2, height = height - 2)
  tkBinds(W)
  #setCoef(W)
  .tkRreplot(W)
  .addTraceTclVariables(W)
  invisible(W)
}


###### tkRreplot #####
#' @export
tkRreplot <- function(W,
                      fun,
                      width,
                      height,
                      ...) {
  if (missing(W)){
    W <- getVariable("tkRplotR_currentToplevel")
   # message(W)
    }
  parent <- .getToplevel(W)
  parentGeometry <-
    as.numeric(c(tclvalue(tkwinfo("width", parent)),
                 tclvalue(tkwinfo("height", parent))))
  heightPrevious <- parent$env$height
  widthPrevious <- parent$env$width
  diffWidth <- parentGeometry[1] - widthPrevious
  diffHeight <- parentGeometry[2] - heightPrevious

  if (missWidth <- missing(width)) {
    width <-
      as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
  }

  if (missHeight <- missing(height)) {
    height <-
      as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
  }

  if (any(!missWidth, !missHeight)) {
    tkwm.geometry(parent, paste0(c(width, height) + c(diffWidth, diffHeight), collapse = "x"))
    return(invisible())
  }

  if (missing(fun)) {
    fun <- parent$env$fun
  }

  fp <-
    tempfile(pattern = "tkRplotR.",
             tmpdir = tempdir(),
             fileext = ".png")
  on.exit(unlink(fp))
  png(
    filename = fp ,
    type = getVariable("tkRplotR_pngType"),
    width = width,
    height = height
  )
  plotOk <- try(fun(...),  silent = TRUE)
  if (inherits(plotOk, "try-error"))
    return(dev.off())
  parent$env$usr <- setVariable("tkRplotR_usr" , par("usr"))
  parent$env$plt <- setVariable("tkRplotR_plt" , par("plt"))
  dev.off()
  tcl(parent$env$imageId, "blank")
  # tkpack.forget(parent$env$canvas)
  tkimage.create("photo", parent$env$imageId, file = fp)
  # tkpack(parent$env$canvas,
  #        expand = 1,
  #        fill = "both")
  # parent$env$height <- height
  # parent$env$width <- width
  parent$env$width <- setVariable("tkRplotR_canvasWidth", width)
  parent$env$height <- setVariable("tkRplotR_canvasHeight", height)
  #setVariable("tkRplotR_usr" , parent$env$usr)
  #setVariable("tkRplotR_plt" , parent$env$plt)
  invisible(setCoef(parent))
  # parent$env$canvas$coef <- getCoef()
  #.Tcl("update")
}

#tclTkRreplot <- .tclFun1(tkRreplot)

#' @export
.TclCallback <- function(x) {
  paste(strsplit(.Tcl.callback(x), " ")[[1]][1:2], collapse = " ")
}

#' @export
##### .tkRreplot #####
.tkRreplot <- function(W) {
  if (missing(W)) {
    W <- getVariable("tkRplotR_currentToplevel")
   # message(W)
  }
  W <- .getToplevelID(W)
  .Tcl("global tkRreplotRdoResize")
  if (tclvalue(.Tcl("info exists tkRreplotRdoResize")) == 1)
    .Tcl("after cancel $tkRreplotRdoResize")
  funCallBack <- .TclCallback(tkRreplot)
  cmd <-
    paste0("set tkRreplotRdoResize [after 0 ",  funCallBack, W, "]")[[1]]
  return(invisible(.Tcl(cmd)))
}

#' @title Functions to Convert Tk and User Coordinates
#' @description
#' Convert Tk coordinates from/to user coordinates.
#' @aliases getCoef tk2usr usr2tk
#' @usage setCoef(W, width, height)
#' getCoef(W)
#' tk2usr(W, x = NULL, y = NULL)
#' usr2tk(W, x = NULL, y = NULL)
#' @param W the window (toplevel). If W is missing the getCoef function returns the coefficients for the last toplevel visited.
#' @param width width of the canvas (image)
#' @param height height of the canvas (image)
#' @param x x position.
#' @param y y position.
#' @export
#' @examples
#' \dontshow{
#' if (.isTclImgOk()){
#' bb <- 1
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function() {
#'  x <- 1:20 / 20
#'    plot(
#'    x,
#'    x ^ bb,
#'    col = "#0000ff50",
#'        xlab = "x",
#'            ylab = paste0("x^", bb),
#'                type = "l",
#'                    axes = FALSE,
#'                        lwd = 4)
#'    title(main = bb)
#'      points(x,
#'       x ^ bb,
#'       col = "#ff000050",
#'       pch = 19,
#'       cex = 2)
#'         axis(1)
#'         axis(2)
#'           box()
#'           })
#'
#'  getCoef()
#'
#'  tkbind(tt$env$canvas, "<Button-1>", function(x, y)
#'  print(tk2usr(x, y)))
#'
#'  # A more complex example
#'  local({
#'  canPos <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'  canPosX <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'  canPosY <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'  lineVertical <- .Tcl(paste(tt$env$canvas, "create line 0 0 0 0"))
#'  lineHorizontal<-.Tcl(paste(tt$env$canvas, "create line 0 0 0 0"))
#'  tkbind(tt, "<Motion>", function (x, y) {
#'    x <- as.numeric(x)
#'      y <- as.numeric(y)
#'        for (i in c(canPos, lineVertical, lineHorizontal,canPosX,canPosY))
#'        tkdelete(tt$env$canvas, tclvalue(i))

#'             xy <- formatC(tk2usr(x, y),
#'                             digits = 2,
#'                              format = "f",
#'                               width = 5)
#'
#' xRange <- tt$env$plt[1:2] * tt$env$width
#'   yRange <- (1 - tt$env$plt[4:3]) * tt$env$height
#'     canPos <<- .Tcl(
#'  paste(tt$env$canvas, "create text 40 10 -fill blue -justify left -text { ",
#'      xy[1], " ", xy[2],
#'      "} -font {Helvetica -10}"))
#'       if (x < xRange[1] | x > xRange[2])
#'          return()
#'          if (y < yRange[1] | y > yRange[2])
#'            return()
#' canPosX <<- .Tcl(paste(tt$env$canvas, "create text ", x, yRange[1]-10,
#'            " -fill blue -justify center -text { ",xy[1],
#'            "} -font {Helvetica -10}"))
#' canPosY <<- .Tcl(paste(tt$env$canvas, "create text ",xRange[2]+10, y,
#'    " -fill blue -justify center -text { ",xy[2], "} -font {Helvetica -10}"))
#'            lineVertical <<- .Tcl(paste(tt$env$canvas, "create line ",
#'                  x,      yRange[1],      x,      yRange[2],
#'                     "-fill blue -dash 4"))
#'            lineHorizontal <<- .Tcl(paste(tt$env$canvas,
#'                  "create line ",
#'                   xRange[1],  y, xRange[2], y,
#'                         "-fill blue -dash 4"))})
#'      tkbind(tt$env$canvas, "<Leave>", function (x, y)
#'      {tkdelete(tt$env$canvas, tclvalue(canPos))})
#'      } )
#'      Sys.sleep(1)
#'      tkdestroy(tt)
#' }
#' }
#' \dontrun{
#'
#' bb <- 1
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function() {
#'  x <- 1:20 / 20
#'    plot(
#'    x,
#'    x ^ bb,
#'    col = "#0000ff50",
#'        xlab = "x",
#'            ylab = paste0("x^", bb),
#'                type = "l",
#'                    axes = FALSE,
#'                        lwd = 4)
#'    title(main = bb)
#'      points(x,
#'       x ^ bb,
#'       col = "#ff000050",
#'       pch = 19,
#'       cex = 2)
#'         axis(1)
#'         axis(2)
#'           box()
#'           })
#'
#'  getCoef()
#'
#'  tkbind(tt$env$canvas, "<Button-1>", function(x, y)
#'  print(tk2usr(x, y)))
#'
#'  # A more complex example
#'  local({
#'  canPos <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'  canPosX <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'  canPosY <-.Tcl(paste(tt$env$canvas, "create text 0 0 "))
#'  lineVertical <- .Tcl(paste(tt$env$canvas, "create line 0 0 0 0"))
#'  lineHorizontal<-.Tcl(paste(tt$env$canvas, "create line 0 0 0 0"))
#'  tkbind(tt, "<Motion>", function (x, y) {
#'    x <- as.numeric(x)
#'      y <- as.numeric(y)
#'        for (i in c(canPos, lineVertical, lineHorizontal,canPosX,canPosY))
#'        tkdelete(tt$env$canvas, tclvalue(i))
#'
#'             xy <- formatC(tk2usr(x, y),
#'                             digits = 2,
#'                              format = "f",
#'                               width = 5)
#'
#' xRange <- tt$env$plt[1:2] * tt$env$width
#'   yRange <- (1 - tt$env$plt[4:3]) * tt$env$height
#'     canPos <<- .Tcl(
#'  paste(tt$env$canvas, "create text 40 10 -fill blue -justify left -text { ",
#'      xy[1], " ", xy[2],
#'      "} -font {Helvetica -10}"))
#'       if (x < xRange[1] | x > xRange[2])
#'          return()
#'          if (y < yRange[1] | y > yRange[2])
#'            return()
#' canPosX <<- .Tcl(paste(tt$env$canvas, "create text ", x, yRange[1]-10,
#'            " -fill blue -justify center -text { ",xy[1],
#'            "} -font {Helvetica -10}"))
#' canPosY <<- .Tcl(paste(tt$env$canvas, "create text ",xRange[2]+10, y,
#'    " -fill blue -justify center -text { ",xy[2], "} -font {Helvetica -10}"))
#'            lineVertical <<- .Tcl(paste(tt$env$canvas, "create line ",
#'                  x,      yRange[1],      x,      yRange[2],
#'                     "-fill blue -dash 4"))
#'            lineHorizontal <<- .Tcl(paste(tt$env$canvas,
#'                  "create line ",
#'                   xRange[1],  y, xRange[2], y,
#'                         "-fill blue -dash 4"))})
#'      tkbind(tt$env$canvas, "<Leave>", function (x, y)
#'      {tkdelete(tt$env$canvas, tclvalue(canPos))})
#'      } )
#'
#'  }

#setCoef
setCoef <- function(W, width, height) {
  parent <- .getToplevel(W)
  if (missing(width)) {
    width <-
      as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
  } else{
    if (!is.numeric(width)) {
      width <-
        as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
    }
  }
  if (missing(height)) {
    height <-
      as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
  } else{
    if (!is.numeric(height)) {
      height <-
        as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
    }
  }
  setVariable("tkRplotR_usr" , usr <- parent$env$usr)
  setVariable("tkRplotR_plt" , plt <- parent$env$plt)
  parent$env$height <- setVariable("tkRplotR_canvasHeight",
                                   height)
  parent$env$width <- setVariable("tkRplotR_canvasWidth",
                                  width)
  xCoef <-
    setVariable("tkRplotR_xCoef", .getCoef(plt[1:2] * (
      getVariable("tkRplotR_canvasWidth") + 0.5
    ), usr[1:2]))
  yCoef <-
    setVariable("tkRplotR_yCoef", .getCoef((1 - plt[3:4]) * (
      getVariable("tkRplotR_canvasHeight") + 0.5
    ), usr[3:4]))
  out <- list(x = xCoef, y = yCoef)
  parent$env$canvas$coef <- out
  return(out)
}

#' @export
###### getCoef #####

getCoef <- function(W) {
  if (!missing(W)) {
    parent <- .getToplevel(W)
    width <-
      as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
    height <-
      as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
    widthPrevious <- parent$env$width
    heightPrevious <- parent$env$height
    if (any(isTRUE(widthPrevious != width),
            isTRUE(heightPrevious != height)))  {
      tkRreplot(parent)
    }
    setVariable("tkRplotR_canvasWidth", width)
    setVariable("tkRplotR_canvasHeight", height)
    setVariable("tkRplotR_usr" , parent$env$usr)
    setVariable("tkRplotR_plt" , parent$env$plt)
    newCoef <- getCoef()
    parent$env$canvas$coef <- newCoef
    return(newCoef)
  }
  plt <- getVariable("tkRplotR_plt")
  usr <- getVariable("tkRplotR_usr")
  setVariable("tkRplotR_xCoef", xCoef <-
                .getCoef(plt[1:2] * (
                  getVariable("tkRplotR_canvasWidth") + 0.5
                ), usr[1:2]))
  setVariable("tkRplotR_yCoef",   yCoef <-
                .getCoef((1 - plt[3:4]) * (
                  getVariable("tkRplotR_canvasHeight") + 0.5
                ),
                usr[3:4]))
  return(list(x = xCoef, y = yCoef))
}

#' @export
###### .getCoef #####
.getCoef <- function(plt, usr) {
  a <- diff(usr) / diff(plt)
  c(a, usr[1] - a * plt[1])
}

#' @export
###### tk2usr #####

tk2usr <- function(W, x = NULL, y = NULL) {
  if (inherits(W, "tkwin")) {
    parent <- .getToplevel(W)
    coef <- parent$env$canvas$coef
    return(c(
      x = as.numeric(x) * coef$x[1] + coef$x[2],
      y = as.numeric(y) * coef$y[1] + coef$y[2]
    ))
  } else{
    y <- x
    x <- W
    coef <- list(x = getVariable("tkRplotR_xCoef"), y = getVariable("tkRplotR_yCoef"))
    return(c(
      x = as.numeric(x) * coef$x[1] + coef$x[2],
      y = as.numeric(y) * coef$y[1] + coef$y[2]
    ))
  }
}

#' @export
###### usr2tk #####
# usr2tk <- function(x = NULL, y = NULL) {
#   c(
#     x = (x - getVariable("tkRplotR_xCoef")[2]) / getVariable("tkRplotR_xCoef")[1] - 0.5,
#     y = (y - getVariable("tkRplotR_yCoef")[2]) / getVariable("tkRplotR_yCoef")[1] - 0.5
#   )
# }

usr2tk <- function(W, x = NULL, y = NULL) {
  if (inherits(W, "tkwin")) {
    parent <- .getToplevel(W)
    coef <- parent$env$canvas$coef
    return(c(
      x = (x - coef$x[2]) / coef$x[1] - 0.5,
      y = (y - coef$y[2]) / coef$y[1] - 0.5
    ))
  } else{
    y <- x
    x <- W
    return(c(
      x = (x - getVariable("tkRplotR_xCoef")[2]) / getVariable("tkRplotR_xCoef")[1] - 0.5,
      y = (y - getVariable("tkRplotR_yCoef")[2]) / getVariable("tkRplotR_yCoef")[1] - 0.5
    ))
  }
}


#' @title Gives the Position
#' @description
#' Gives the position when the left mouse button is pressed + "Ctrl" button.
#' @usage tkLocator(parent, n = 1)
#' @param parent Tk toplevel window
#' @param n the number of points to locate
#' @return A list with x and y components which are the coordinates of the identified points.
#' @export
#' @examples
#' \dontrun{
#' bb <- 1
#' tt <- tktoplevel()
#' tt <- tkRplot(tt, function() {
#'  x <- 1:20 / 20
#'    plot(
#'    x,
#'    x ^ bb,
#'    col = "#0000ff50",
#'        xlab = "x",
#'            ylab = paste0("x^", bb),
#'                type = "l",
#'                    axes = FALSE,
#'                        lwd = 4)
#'    title(main = bb)
#'      points(x,
#'       x ^ bb,
#'       col = "#ff000050",
#'       pch = 19,
#'       cex = 2)
#'         axis(1)
#'         axis(2)
#'           box()
#'           })
#'  tkLocator(tt, 2)
#'  }
#'

###### tkLocator #####
tkLocator <- function(parent, n = 1) {
  if (n < 1)
    n <- 1
  done <- tclVar(0)
  out <- data.frame(x = rep(NA, n), y = rep(NA, n))
  i = 0
  tkbind(parent, "<Destroy>", function()
    tclvalue(done) <- 2)
  tkbind(parent, "<Escape>", function()
    tclvalue(done) <<- 1)
  tkbind(parent$env$canvas, "<Control-Button-1>", function(W, x, y) {
    W <- .getToplevel(W)
    i <<- i + 1
    out[i,] <<- tk2usr(W, x, y)
    if (i == n)
      tclvalue(done) <<- 1
  })
  tkwait.variable(done)

  if (tclvalue(done) != "2")
    tkbind(parent$env$canvas, "<Control-Button-1>", function() {

    })
  if (i > 0)
    return(out[1:i, ])
}

#' @export
.rmVariable <- function(name){
  globalVariable(type = "rm", name)
}

#' @export
.lsVariable <- function(){
  globalVariable("ls")
}


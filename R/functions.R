
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
  if (as.numeric(R.version$minor) < 4 |
      as.character(tcltk::tcl("info", "tclversion")) <= "8.5") {
    packageStartupMessage("Please install 'Img' extension for tcltk ", tcltk::tclVersion())
    return(FALSE)
  }
  # packageStartupMessage(paste("tcltk", as.character(tcltk::tcl(
  #   "info", "tclversion"
  # ))))
  return(TRUE)
}

#' @export .tclFun1
.tclFun1 <- function(f,
                     name = deparse(substitute(f)),
                     arg1 = names(formals(f))[1])
{
  name <- paste("R", make.names(name[1]), sep = "_")
  res <- .Tcl.callback(f)
  res <- strtrim(res, 16)
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

#' @export
.getToplevelID <- function(win) {
  if (is.tkwin(win)) {
    win <- win$ID
    }
    return(paste0(".", strsplit(win, split = "\\.")[[1]][2]))
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
#' bb <- 1
#' tkbb <- tclVar(1)
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
#'  f <- function(...) {
#'  b <- as.numeric(tclvalue(tkbb))
#'    if (b != bb) {
#'        bb <<- b
#'            tkRreplot(tt)
#'              }
#'      }
#'
#'      s <-
#'        tkscale(
#'        tt,
#'        command = f,
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
#' bb <- 1
#' tkbb <- tclVar(1)
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
#'  f <- function(...) {
#'  b <- as.numeric(tclvalue(tkbb))
#'    if (b != bb) {
#'        bb <<- b
#'            tkRreplot(tt)
#'              }
#'      }
#'
#'      s <-
#'        tkscale(
#'        tt,
#'        command = f,
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
      if (any(isTRUE(widthPrevious!=width), isTRUE(heightPrevious!=height)))  {
       #print("ConfigureDone")
        .tkRreplot(parent)
      }
    })
  } else {
    tkbind(parent$env$canvas, "<Configure>", "")
  }

  if (expose) {
    tkbind(parent$env$canvas, "<Expose>", function() {
      #print("Expose")
      width <-
        as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
      height <-
        as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
      widthPrevious <- parent$env$width
      heightPrevious <- parent$env$height
      if (any(isTRUE(widthPrevious!=width), isTRUE(heightPrevious!=height)))  {
       # print("ExposeDone")
        .tkRreplot(parent)
      }
    })
  } else {
    tkbind(parent$env$canvas, "<Expose>", "")
  }

  tkbind(parent, "<FocusIn>", function() {
    #print("FocusIn")
    width <-
      as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
    height <-
      as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
    widthPrevious <- parent$env$width
    heightPrevious <- parent$env$height
    if (any(isTRUE(widthPrevious!=width), isTRUE(heightPrevious!=height)))  {
    #  print("FocusInDone")
      tkRreplot(parent)
    }
  })

  tkbind(parent$env$canvas, "<Enter>", function(W) {
    setVariable("tkRplotRcurrentToplevel", W)
    #print("enter in canvas")
    width <- as.numeric(.Tcl(paste("winfo width", W)))
    height <- as.numeric(.Tcl(paste("winfo height", W)))
    widthPrevious <- parent$env$width
    heightPrevious <- parent$env$height
    if (any(isTRUE(widthPrevious!=width), isTRUE(heightPrevious!=height)))  {
     # print("enter in canvas")
      tkRreplot(parent)
    }
    # setVariable("tkRplotRcanvasWidth", width)
    # setVariable("tkRplotRcanvasHeight", height)
    # setVariable("usr" , parent$env$usr)
    # setVariable("plt" , parent$env$plt)
    # parent$env$canvas$coef <- getCoef()
    setCoef(parent)
    #print(getCoef())
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

globalVariable <- local({
  globalEnv <- new.env(parent = emptyenv())
  globalVars <- NULL
  function(type = NULL,
           name = NULL,
           value,
           where = .GlobalEnv) {
    switch(
      type,
      set = {
        assign(name, value, envir = globalEnv)
        assign("globalVars", sort(unique(c(
          globalVars, name
        ))), envir = environment(globalVariable))
        return(invisible(value))
      },
      get = {
        if (!name %in% globalVars) {
          return(message("First define variable"))
        }
        return(get(name, envir = globalEnv))
      },
      rm = {
        assign("globalVars", globalVars[!globalVars %in% name],
               envir = environment(globalVariable))
        return(assign(name, NULL, envir = globalEnv))
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


#' @title Set and Get Variables
#' @description
#' Define and get variables
#' @aliases
#' setVariable
#' getVariable
#' @usage
#' setVariable(name, value = NULL)
#' getVariable(name)
#' @param name name of the variable
#' @param value the value of the variable
#' @export
#' @examples
#' setVariable("var1", 1)
#' exists("var1")
#' getVariable("var1")
#'
#' getVariable("tkRplotRpngType")
#'

###### setVariable  #####
setVariable <- function(name, value = NULL) {
  globalVariable(type = "set", name, value = value)
}

setVariable("tkRplotRpngType", "cairo-png")
# #' @title Get a Variable
# #' @description
# #' This function gets the value of a variable
# #' @param name name of the variable
#' @export

###### getVariable #####
getVariable <- function(name) {
  globalVariable(type = "get", name)
}

getEnv <- function()
  globalVariable("env")

getAllVariables <- function()
  ls(getEnv())



#' @title Tk Rplot With Resizing
#' @description
#' Dispaly a plot in a Tk toplevel window.
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
#'  bb <- 1
#' tkbb <- tclVar(1)
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
#'  f <- function(...) {
#'  b <- as.numeric(tclvalue(tkbb))
#'    if (b != bb) {
#'        bb <<- b
#'            tkRreplot(tt)
#'              }
#'      }
#'
#'      s <-
#'        tkscale(
#'        tt,
#'        command = f,
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
  setVariable("tkRplotRcurrentToplevel", W)
  setVariable(.Tk.ID(W), W)
  #setVariable("tkRplotRcanvasWidth", width)
  #setVariable("tkRplotRcanvasHeight", height)
  fp <-
    tempfile(pattern = "tkRplotR.",
             tmpdir = tempdir(),
             fileext = ".png")
  on.exit(unlink(fp))
  png(
    filename = fp ,
    type = getVariable("tkRplotRpngType"),
    width = width,
    height = height
  )
  plotOk <- try(fun(...),  silent = TRUE)
  if (inherits(plotOk, "try-error"))
    return(dev.off())
  W$env$usr <- setVariable("usr" , par("usr"))
  W$env$plt <- setVariable("plt" , par("plt"))
  dev.off()
  #
  imageId <- paste0("TkRplot", W$ID)
  image <- tkimage.create("photo", imageId , file = fp)
  W$env$canvas <-
    tkcanvas(
      W,
      relief = "flat",
      borderwidth = 0,
      background =  "white"
    )
  tkconfigure(W$env$canvas, width = width, height = height)
  tkpack(
    W$env$canvas,
    anchor = "center",
    fill = "both",
    expand = 1
  )
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
  #W$env$height <- setVariable("tkRplotRcanvasHeight", height)
  #W$env$width <- setVariable("tkRplotRcanvasWidth", width)
  #W$env$canvas$coef <- getCoef()
  tkconfigure(W$env$canvas, width = width - 2, height = height - 2)
  tkBinds(W)
  setCoef(W)
  invisible(W)
}


###### tkRreplot #####
#' @export
tkRreplot <- function(W,
                      fun,
                      width,
                      height,
                      ...) {
   if (missing(W))
     W <- getVariable("tkRplotRcurrentToplevel")
  parent <- .getToplevel(W)

  if (missing(fun)) {
    fun <- parent$env$fun
  }

  if (missing(width)) {
    width <-
      as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
  }

  if (missing(height)) {
    height <-
      as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
  }

  fp <-
    tempfile(pattern = "tkRplotR.",
             tmpdir = tempdir(),
             fileext = ".png")
  on.exit(unlink(fp))
  png(
    filename = fp ,
    type = getVariable("tkRplotRpngType"),
    width = width,
    height = height
  )
  plotOk <- try(fun(...),  silent = TRUE)
  if (inherits(plotOk, "try-error"))
    return(dev.off())
parent$env$usr <-   setVariable("usr" , par("usr"))
parent$env$plt <-  setVariable("plt" , par("plt"))
  dev.off()
  tcl(parent$env$imageId, "blank")
  # tkpack.forget(parent$env$canvas)
  tkimage.create("photo", parent$env$imageId, file = fp)
  # tkpack(parent$env$canvas,
  #        expand = 1,
  #        fill = "both")
  # parent$env$height <- height
  # parent$env$width <- width
  parent$env$width <- setVariable("tkRplotRcanvasWidth", width)
  parent$env$height <- setVariable("tkRplotRcanvasHeight", height)
  #setVariable("usr" , parent$env$usr)
  #setVariable("plt" , parent$env$plt)
  setCoef(parent)

 # parent$env$canvas$coef <- getCoef()


  #.Tcl("update")
}

#tclTkRreplot <- .tclFun1(tkRreplot)

#' @export
##### .tkRreplot #####
.tkRreplot <- function(W) {
  .Tcl("global tkRreplotRdoResize")
  if (tclvalue(.Tcl("info exists tkRreplotRdoResize")) == 1)
    .Tcl("after cancel $tkRreplotRdoResize")
  funCallBack <- substr(.Tcl.callback(tkRreplot), 1, 17)
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
  setVariable("usr" , usr <- parent$env$usr)
  setVariable("plt" , plt <- parent$env$plt)
  parent$env$height <- setVariable("tkRplotRcanvasHeight",
                                   height)
  parent$env$width <- setVariable("tkRplotRcanvasWidth",
                                  width)
  xCoef <-
    setVariable("xCoef", .getCoef(plt[1:2] * (
      getVariable("tkRplotRcanvasWidth") + 0.5
    ), usr[1:2]))
  yCoef <-
    setVariable("yCoef", .getCoef((1 - plt[3:4]) * (
      getVariable("tkRplotRcanvasHeight") + 0.5
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
    if (any(isTRUE(widthPrevious!=width), isTRUE(heightPrevious!=height)))  {
      tkRreplot(parent)
    }
    setVariable("tkRplotRcanvasWidth", width)
    setVariable("tkRplotRcanvasHeight", height)
    setVariable("usr" , parent$env$usr)
    setVariable("plt" , parent$env$plt)
    newCoef <- getCoef()
    parent$env$canvas$coef <- newCoef
    return(newCoef)
  }
  plt <- getVariable("plt")
  usr <- getVariable("usr")
  setVariable("xCoef", xCoef <-
                .getCoef(plt[1:2] * (
                  getVariable("tkRplotRcanvasWidth") + 0.5
                ), usr[1:2]))
  setVariable("yCoef",   yCoef <-
                .getCoef((1 - plt[3:4]) * (
                  getVariable("tkRplotRcanvasHeight") + 0.5
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

# tk2usr <- function(x, y) {
#   c(
#     x = as.numeric(x) * getVariable("xCoef")[1] + getVariable("xCoef")[2],
#     y = as.numeric(y) * getVariable("yCoef")[1] + getVariable("yCoef")[2]
#   )
# }

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
    coef <- list(x = getVariable("xCoef"), y = getVariable("yCoef"))
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
#     x = (x - getVariable("xCoef")[2]) / getVariable("xCoef")[1] - 0.5,
#     y = (y - getVariable("yCoef")[2]) / getVariable("yCoef")[1] - 0.5
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
      x = (x - getVariable("xCoef")[2]) / getVariable("xCoef")[1] - 0.5,
      y = (y - getVariable("yCoef")[2]) / getVariable("yCoef")[1] - 0.5
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
    out[i, ] <<- tk2usr(W, x, y)
    if (i == n)
      tclvalue(done) <<- 1
  })
  tkwait.variable(done)

  if (tclvalue(done) != "2")
    tkbind(parent$env$canvas, "<Control-Button-1>", function() {

    })
  if (i > 0)
    return(out[1:i,])
}

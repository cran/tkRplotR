
#' @import grDevices
#' @import tcltk
#' @importFrom graphics par


globalEnv <- new.env(parent = emptyenv())


#' @export .isTclImgOk
.isTclImgOk <- function(){
if (as.numeric(R.version$minor) < 4 | as.character(tcltk::tcl("info", "tclversion")) <= "8.5"){
  message("Please install 'Img' extension for tcltk\n tcltk::tclVersion('Img')")
  return(FALSE)}
  message(paste("tcltk", as.character(tcltk::tcl("info", "tclversion"))))
  return(TRUE)
}


.isTclImgOk()

if (!capabilities("png"))
  stop(message("Your R has no capability for png."))

if (!capabilities("tcltk"))
  stop(message("Your R has no capability for tcltk."))

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
tkBinds <- local({
  n <- 0           # number of events
  t0 <- NULL       # time0 (just before plot)
  tDiff <-
    200      # time between time0 and replot image in Tk canvas widget
  function(parent,
           expose = TRUE,
           configure = TRUE) {
    getTime <- function()
      as.numeric(Sys.time()) * 1000

    if (configure) {
      tkbind(parent$env$canvas, "<Configure>", function() {
        #print(tDiff)
        if (is.null(t0))
          t0 <<- getTime()
        n <<- n + 1
        width <-
          as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
        height <-
          as.numeric(.Tcl(paste(
            "winfo height", parent$env$canvas
          )))
        widthPrevious <- parent$env$width
        heightPrevious <- parent$env$height
        if (abs(widthPrevious - width) > 10 |
            abs(heightPrevious  - height) > 10) {
          if (n > 50 | (getTime() - t0) > tDiff) {
            t0 <<-   getTime()
            parent$env$height <- height
            parent$env$width <- width
            tkpack.forget(parent$env$canvas)
            tkRreplot(parent)
            n <<- 0
            tkpack(parent$env$canvas,
                   expand = 1,
                   fill = "both")
            tDiff <<- 10 * (getTime() - t0)

            t0 <<- NULL
          }
        }
      })
    } else {
      tkbind(parent$env$canvas, "<Configure>", "")
    }
    if (expose) {
      tkbind(parent$env$canvas, "<Expose>", function() {
        #print(tDiff)
        if (is.null(t0))
          t0 <<- getTime()
        n <<- n + 1
        width <-
          as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
        height <-
          as.numeric(.Tcl(paste(
            "winfo height", parent$env$canvas
          )))
        widthPrevious <- parent$env$width
        heightPrevious <- parent$env$height
        if (abs(widthPrevious - width) > 10 |
            abs(heightPrevious  - height) > 10) {
          if (n > 50 | (getTime() - t0) > tDiff) {
            t0 <<-   getTime()
            parent$env$height <-  height
            parent$env$width <-  width
            tkpack.forget(parent$env$canvas)
            tkRreplot(parent)
            n <<- 0
            tkpack(parent$env$canvas,
                   expand = 1,
                   fill = "both")
            tDiff <<- 10 * (getTime() - t0)

            t0 <<- NULL
          }
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
      if ((abs(width - widthPrevious) > 0) |
          (abs(height - heightPrevious) > 0))  {
        parent$env$height <- height
        parent$env$width <- width
        tkpack.forget(parent$env$canvas)
        tkRreplot(parent)
        tkpack(parent$env$canvas,
               expand = 1,
               fill = "both")
      }
    })

    tkbind(parent$env$canvas, "<Enter>", function(W) {
      width <- as.numeric(.Tcl(paste("winfo width", W)))
      height <- as.numeric(.Tcl(paste("winfo height", W)))
      widthPrevious <- parent$env$width
      heightPrevious <- parent$env$height
      if (abs(width - widthPrevious) > 0 |
          abs(height - heightPrevious) > 0) {
        parent$env$height <- height
        parent$env$width <- width
        tkpack.forget(W)
        tkRreplot(parent)
        tkpack(W, expand = 1, fill = "both")
      }
      setVariable("tkRplotRcanvasWidth", width)
      setVariable("tkRplotRcanvasHeight", height)
      setVariable("usr" , parent$env$usr)
      setVariable("plt" , parent$env$plt)
      parent$env$canvas$coef <- getCoef()

    })

    # tkbind(parent, "<Leave>", function() {
    #   width <- as.numeric(.Tcl(paste("winfo width", parent$env$canvas)))
    #   height <- as.numeric(.Tcl(paste("winfo height", parent$env$canvas)))
    #   widthPrevious <- parent$env$width
    #   heightPrevious <- parent$env$height
    #   if (abs(width - widthPrevious) > 0 | abs(height - heightPrevious) > 0) {
    #     parent$env$height <- height
    #     parent$env$width <- width
    #     tkpack.forget(parent$env$canvas)
    #     tkRreplot(parent)
    #     tkpack(parent$env$canvas, expand = 1, fill = "both")
    #   }
    # })
    #
  }
})

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
addTkBind <- function(win, event, fun){
  if (is.function(fun)) fun <- .Tcl.callback(fun)
  tkbind(win, event, paste("+", fun))
}

###### globalVariable #####
globalVariable <- local({
  globalEnv <- new.env(parent = emptyenv())
  globalVars <- NULL
  function(type = NULL,
           name = NULL,
           value) {
    if (!is.null(name)) {
      if (type == "set") {
        assign(name, value, envir = globalEnv)
        assign("globalVars", sort(unique(c(
          globalVars, name
        ))), envir = environment(globalVariable))
      }
      if (type == "get") {
        if (!name %in% globalVars)
          return(message("First define variable"))
        return(get(name, envir = globalEnv))
      }
      if (type == "rm") {
        assign("globalVars", globalVars[!globalVars %in% name],
               envir = environment(globalVariable))
        assign(name, NULL, envir = globalEnv)
      }
    }

    if (is.null(type))
      return(ls(envir = environment(globalVariable), all.names = TRUE))
    if (type == "ls")
      return(globalVars)
    if (type == "env")
      return(globalEnv)
    # if (type == "all2r")
    #   invisible(lapply(
    #     globalVars,
    #     FUN = function(x)
    #       assign(x, get(x, envir = globalEnv), envir = .GlobalEnv)
    #   ))
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

###### setVariable  #####
setVariable <- function(name, value = NULL) {
  globalVariable(type = "set", name, value = value)
}

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
#' @aliases tkRreplot
#' @usage tkRplot(parent, fun, height = 490, width = 490, ...)
#' tkRreplot(parent, fun = parent$env$fun, ...)
#' @param parent Tk toplevel window
#' @param fun function to produce the plot
#' @param height image height
#' @param width image width
#' @param ... aditional arguments
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
#'        fill = "both")
#'        Sys.sleep(1)
#'        tkdestroy(tt)
#'        }
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
#'        expand = FALSE,
#'        fill = "both")
#'
#'}

###### tkRplot #####
tkRplot <- function(parent,
                    fun,
                    height = 490,
                    width = 490,
                    ...) {
  setVariable("tkRplotRcanvasWidth", width)
  setVariable("tkRplotRcanvasHeight", height)
  fp <-
    tempfile(pattern = "tkRplotR.",
             tmpdir = tempdir(),
             fileext = ".png")
  on.exit(unlink(fp))
  png(filename = fp ,
      width = width,
      height = height)
  try(fun(...))
  setVariable("usr" , parent$env$usr <- par("usr"))
  setVariable("plt" , parent$env$plt <- par("plt"))
  dev.off()
  image <- tkimage.create("photo", "TkRplot", file = fp)
  parent$env$canvas <-
    tkcanvas(
      parent,
      relief = "flat",
      borderwidth = 0,
      background =  "white"
    )
  tkconfigure(parent$env$canvas, width = width, height = height)
  tkpack(
    parent$env$canvas,
    anchor = "center",
    fill = "both",
    expand = 1
  )
  tcl(parent$env$canvas,
      "create",
      "image",
      0,
      0,
      image = image  ,
      anchor = "nw")

  tkbind(parent$env$canvas, "<Destroy>", function()
    .Tcl(paste("image delete",
               image)))

  parent$env$image <- image
  parent$env$fun <- fun
  parent$env$height <-  height
  parent$env$width <-  width
  parent$env$canvas$coef <- getCoef()
  tkBinds(parent)
  tkconfigure(parent$env$canvas, width = width - 2, height = height - 2)
  invisible(parent)
}

###### tkRreplot #####
#' @export
tkRreplot <- function(parent,
                      fun = parent$env$fun,
                      ...) {
  height <- parent$env$height
  width <- parent$env$width
  fp <-
    tempfile(pattern = "tkRplotR.",
             tmpdir = tempdir(),
             fileext = ".png")
  on.exit(unlink(fp))
  png(filename = fp ,
      width = width,
      height = height)
  plotOk <- try(fun(...),  silent = TRUE)
  if (inherits(plotOk, "try-error"))
    return(dev.off())
  setVariable("usr" , parent$env$usr <- par("usr"))
  setVariable("plt" , parent$env$plt <- par("plt"))
  dev.off()
  tcl("TkRplot","blank")
  tkimage.create("photo", "TkRplot", file = fp)
}



#' @title Functions to Convert Tk and User Coordinates
#' @description
#' Functions to convert coordinates.
#' @aliases tk2usr
#' @usage getCoef()
#' tk2usr(x,y)
#' @param x x position in the tk canvas
#' @param y y position in the tk canvas
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

###### getCoef #####
getCoef <- function() {
  #x = NULL, y = NULL) {
  #if (is.null(x))
  x <- getVariable("plt")
  #if (is.null(y))
  y <- getVariable("usr")
  X <- x[1:2] * (getVariable("tkRplotRcanvasWidth") + 1)
  Y <- y[1:2]
  a <- diff(Y) / diff(X)
  xCoef <- c(a, Y[1] - a * X[1])
  setVariable("xCoef", xCoef)
  X <- (1 - x[4:3]) * (getVariable("tkRplotRcanvasHeight") + 1)
  Y <- y[4:3]
  a <- diff(Y) / diff(X)
  yCoef <- c(a, Y[1] - a * X[1])
  setVariable("yCoef", yCoef)
  return(list(x = xCoef, y = yCoef))
}


#' @export
###### tk2usr #####
tk2usr <- function(x, y) {
  c(
    as.numeric(x) * getVariable("xCoef")[1] + getVariable("xCoef")[2],
    (as.numeric(y) - 1) * getVariable("yCoef")[1] + getVariable("yCoef")[2]
  )
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
  tkbind(parent$env$canvas, "<Control-Button-1>", function(x, y) {
    i <<- i + 1
    out[i, ] <<- tk2usr(x, y)
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

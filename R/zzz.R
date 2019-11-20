#' @import grDevices
#' @import tcltk
#' @importFrom graphics par plot rect
#'


# .OnLoad <- function(libname, pkgname) {
#   tvb <- packageDescription('tkRplotR')
#   packageStartupMessage(
#     c("\n", tvb$Package, "\n"),
#     c("Version: ", tvb$Version, "\n")
#     #,c("Compiled:", tvb$Built, "\n\n")
#     )
# }


.onAttach <- function(libname, pkgname) {

  ### select the correct "type" for the png function

  for (pngType in c("cairo-png", "cairo", "Xlib", "quartz", NULL)) {
    fp <- tempfile(pattern = "tkRplotR.",
                   tmpdir = tempdir(),
                   fileext = ".png")

    grDevices::png(
      filename = fp ,
      type = pngType,
      width = 10,
      height = 10
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
    rect(.5, -1, 2, 2, col = rgb(1, 0, 0, .5), border = NA)
    dev.off()
    imageId <- paste0("TkRplot")
    image <- tkimage.create("photo", imageId , file = fp)

    if ((tclvalue(.Tcl("TkRplot get 0 0")) != tclvalue(.Tcl("TkRplot get 5 5")))) {
      break
    }
  }


  switch(
    pngType,
    "cairo" = {
      setVariable("tkRplotRpngType", "cairo")
    },
    "cairo-png" = {
      setVariable("tkRplotRpngType", "cairo-png")
    },
    "Xlib" = {
      setVariable("tkRplotRpngType", "Xlib")
    },
    "quartz" = {
      setVariable("tkRplotRpngType", "quartz")
    },
    NULL =  {
      packageStartupMessage("The transparency does not work!!!")
    }
  )

  if (!capabilities("png"))
  stop(packageStartupMessage("Your R has no capability for png."))

  if (!capabilities("tcltk"))
  stop(packageStartupMessage("Your R has no capability for tcltk."))

  .isTclImgOk()
}


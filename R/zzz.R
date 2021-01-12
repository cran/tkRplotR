#' @import grDevices
#' @import tcltk
#' @importFrom graphics par plot rect
#'

 # .OnLoad <- function(libname, pkgname) {
 #  packageStartupMessage(packageDescription('tkRplotR')$Version)
 #   setVariable("tkRplotR_pngType", .selectPngType())
 #  if(!identical(didLoad,FALSE)){
 #  packageStartupMessage(didLoad)
 # }
 # }


.onAttach <- function(libname, pkgname) {

   packageStartupMessage("Version ",
                         utils::packageDescription(pkg = pkgname, lib.loc = libname, field="Version"))
  if(as.numeric(tcltk::.Tcl("info tclversion")) < 8.5) {
    packageStartupMessage(" *** tkRplotR needs tcl/tk version 8.5 or newer ***\n")
  }

  ## load Img tk extension if available
  sysname <- Sys.info()[1]

  switch(sysname,
         Darwin = addTclPath("/System/Library/Tcl"),
         Windows = addTclPath(gsub("\\\\", "/", "C:/ActiveTcl/lib")))

 didLoad <- tcltk::tclRequire('Img', warn = FALSE)
  if(identical(didLoad, FALSE) && ! .isTclImgOk()) {
    packageStartupMessage("Please install 'Img' extension for tcltk ",
                          tcltk::tclVersion())
    }

  ### select the correct "type" for the png function

 # setVariable("tkRplotR_pngType", .selectPngType())
  # for (pngType in c("cairo-png", "cairo", "Xlib", "quartz", NULL)) {
  #   fp <- tempfile(pattern = "tkRplotR.",
  #                  tmpdir = tempdir(),
  #                  fileext = ".png")
  #
  #   grDevices::png(
  #     filename = fp ,
  #     type = pngType,
  #     width = 10,
  #     height = 10
  #   )
  #
  #   par(oma = rep(0, 4))
  #   par(mar = rep(0, 4))
  #   plot(
  #     1,
  #     type = "n",
  #     ann = FALSE,
  #     axes = FALSE,
  #     xlim = c(0, 1),
  #     ylim = c(0, 1)
  #   )
  #   rect(.5, -1, 2, 2, col = rgb(1, 0, 0, .5), border = NA)
  #   dev.off()
  #   imageId <- paste0("TkRplot")
  #   image <- tcltk::tkimage.create("photo", imageId , file = fp)
  #
  #   if ((tcltk::tclvalue(.Tcl("TkRplot get 0 0")) != tcltk::tclvalue(.Tcl("TkRplot get 5 5")))) {
  #     break
  #   }
  # }
  #
  # switch(
  #   pngType,
  #   "cairo" = {
  #     setVariable("tkRplotR_pngType", "cairo")
  #   },
  #   "cairo-png" = {
  #     setVariable("tkRplotR_pngType", "cairo-png")
  #   },
  #   "Xlib" = {
  #     setVariable("tkRplotR_pngType", "Xlib")
  #   },
  #   "quartz" = {
  #     setVariable("tkRplotR_pngType", "quartz")
  #   },
  #   NULL =  {
  #     packageStartupMessage("The transparency does not work!!!")
  #   }
  # )

  if (!capabilities("png"))
  stop(packageStartupMessage("Your R has no capability for png."))

  if (!capabilities("tcltk"))
  stop(packageStartupMessage("Your R has no capability for tcltk."))

  .isTclImgOk()
}


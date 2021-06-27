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
         Darwin = tcltk::addTclPath("/System/Library/Tcl"),
         Windows = tcltk::addTclPath(gsub("\\\\", "/", "C:/ActiveTcl/lib")))

 didLoad <- tcltk::tclRequire('Img', warn = FALSE)
  if(identical(didLoad, FALSE) && ! .isTclImgOk()) {
    packageStartupMessage("Please install 'Img' extension for tcltk ",
                          tcltk::tclVersion())
    }


  if (!capabilities("png"))
  stop(packageStartupMessage("Your R has no capability for png."))

  if (!capabilities("tcltk"))
  stop(packageStartupMessage("Your R has no capability for tcltk."))

  .isTclImgOk()
}


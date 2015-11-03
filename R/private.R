.onLoad <- function(libname, pkgname) {

}

applyStrictnessLevel <- function(
  strict = 0:3,
  msg = "invalid",
  cl = sys.call(-1)
) {
  msg <- paste0(as.character(cl)[1], ": ", msg)
  strict <- as.numeric(match.arg(as.character(strict),
    as.character(0:3)))
  if (strict <= 2) {
    if (strict == 1) {
      message(msg)
    } else if (strict == 2) {
      warning(msg)
    }
  } else if (strict == 3) {
    stop(msg)
  }
  NULL
}

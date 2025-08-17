# zzz.R -------------------------------------------------------------------

# Package-level environment (if you ever need to stash state)
.FraNchEstYN <- new.env(parent = emptyenv())

# Default options without clobbering user settings
.fran_default_options <- function() {
  op <- options()
  defaults <- list(
    FraNchEstYN.startup_banner = TRUE,  # show banner on attach
    FraNchEstYN.banner_emoji   = TRUE   # add a little emoji flair
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
  invisible()
}

# Optional color helpers (no hard dep)
.fran_col <- local({
  if (requireNamespace("crayon", quietly = TRUE)) {
    list(
      accent = crayon::green,
      faint  = crayon::silver,
      strong = crayon::bold
    )
  } else {
    list(
      accent = identity,
      faint  = identity,
      strong = identity
    )
  }
})

# ASCII banner (kept compact to be polite on attach)
.fran_ascii_logo <- function() {
  c(
    "╔═╗┬─┐┌─┐╔╗╔┌─┐┬ ┬╔═╗┌─┐┌┬┐╦ ╦╔╗╔",
    "╠╣ ├┬┘├─┤║║║│  ├─┤║╣ └─┐ │ ╚╦╝║║║",
    "╚  ┴└─┴ ┴╝╚╝└─┘┴ ┴╚═╝└─┘ ┴  ╩ ╝╚╝"
  )
}

.fran_tip_lines <- function(pkgname) {
  v <- tryCatch(utils::packageVersion(pkgname), error = function(...) "?.?.?")
  emj <- if (isTRUE(getOption("FraNchEstYN.banner_emoji", TRUE))) {
    sample(c("🧪","💀","🦇","🕷","🎃","👻","🪓","🕸","🐀"), 1)
  } else ""

  c(
    sprintf("%s v%s", emj, v),
    "Tip: run help(franchestyn) for required inputs and examples."
  )
}

# Should we stay silent?
.fran_quiet <- function() {
  opt_off <- isTRUE(getOption("FraNchEstYN.startup_banner", TRUE) == FALSE)
  env_val <- tolower(trimws(Sys.getenv("FRANCHESTYN_QUIET", "")))
  env_off <- nchar(env_val) > 0 && env_val %in% c("1","true","yes","on")
  in_check <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", ""))
  opt_off || env_off || !interactive() || in_check
}

.onLoad <- function(libname, pkgname) {
  # Absolutely no file reads / compressed-data ops here
  # Just set defaults quietly.
  try(.fran_default_options(), silent = TRUE)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  if (.fran_quiet()) return(invisible())

  col  <- .fran_col
  logo <- col$accent(paste(.fran_ascii_logo(), collapse = "\n"))
  tips <- .fran_tip_lines(pkgname)

  packageStartupMessage(logo)
  packageStartupMessage(col$strong(tips[1L]))
  packageStartupMessage(col$faint(tips[2L]))
  packageStartupMessage(col$faint(tips[3L]))
  packageStartupMessage(
    col$faint("Silence this banner: options(FraNchEstYN.startup_banner = FALSE) or Sys.setenv(FRANCHESTYN_QUIET = '1')")
  )
  invisible()
}

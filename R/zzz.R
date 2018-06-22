set_pkg_opts <- function(..., opt_list=NULL) {
  pkg_opts <- c(pairlist(...), as.pairlist(opt_list))

  if (is.null(pkg_opts)) {
    return(invisible())
  }

  opts <- names(options())
  pkg_opt_names <- names(pkg_opts)
  new_opts <- nzchar(pkg_opt_names) & !pkg_opt_names %in% opts

  if (any(new_opts)) {
    options(pkg_opts[pkg_opt_names[new_opts]])
  } else {
    invisible()
  }
}

.onLoad <- function(libname, pkgname) {
  set_pkg_opts(
    ggdistribute.font="", ggdistribute.geom.defaults=list(edited=FALSE),
    ggdistribute.geom.edits=list())
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("ggdistribute loaded")
}


.onLoad <- function (libname, pkgname) {
    # TODO:
    # Check whether symengine is available, and give verbose message if not.
    # Also check gmp library or MPFR library.
    logo <- symengine_logo()
    if (requireNamespace("crayon")) {
        logo  <- strsplit(logo, "\n")[[1]]
        pres  <- substring(logo, 1, 16)
        posts <- substring(logo, 17)
        
        pres  <- crayon::red(pres)
        posts <- crayon::green(posts)
        
        com <- crayon::`%+%`(pres, posts)
        com <- paste(com, collapse = "\n")
        message(com)
    }
    else
        message(logo)
    
    invisible(TRUE)
}

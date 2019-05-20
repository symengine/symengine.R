
## We do not use "useDynLib" in NAMESPACE but control it in
## .onLoad() and .onUnload() manually.
## This gives us the flexibility to behave differently on windows
## and unix. On windows, we put additional dlls to the package
## folder and load them via `library.dynam` at run time. On Unix,
## we assume that the dependencies such as gmp are in system path.

.onLoad <- function(libname, pkgname) {
    if (.Platform$OS.type == "windows")
        return(.onLoad.windows(libname, pkgname))
    
    library.dynam("symengine", "symengine", libname)
}

.onUnload <- function(libpath) {
    if (.Platform$OS.type == "windows")
        return(.onUnload.windows(libpath))
    
    library.dynam.unload("symengine", libpath)
}

.onLoad.windows <- function(libname, pkgname) {
    library.dynam("libgmp-10"   , "symengine", libname)
    ## Currently using static library as workaround
    #library.dynam("libsymengine", "symengine", libname)
    library.dynam("symengine"   , "symengine", libname)
}

.onUnload.windows <- function(libpath) {
    library.dynam.unload("symengine"   , libpath)
    ## Currently using static library as workaround
    #library.dynam.unload("libsymengine", libpath)
    library.dynam.unload("libgmp-10"   , libpath)
}

.onAttach <- function (libname, pkgname) {
    # TODO:
    # Check whether symengine is available, and give verbose message if not.
    # Also check gmp library or MPFR library.
    
    version <- symengine_version()
    msg <- sprintf("SymEngine Version: %s", version)
    if (requireNamespace("crayon", quietly = TRUE))
        msg <- crayon::yellow(msg)
    packageStartupMessage(msg)
    
    logo <- symengine_ascii_art()
    if (requireNamespace("crayon", quietly = TRUE)) {
        pres  <- substring(logo, 1, 16)
        posts <- substring(logo, 17)
        
        pres  <- crayon::red(pres)
        posts <- crayon::green(posts)
        
        com <- crayon::`%+%`(pres, posts)
        com <- paste(com, collapse = "\n")
        packageStartupMessage(com)
    }
    else
        packageStartupMessage(logo)
    
    invisible(TRUE)
}


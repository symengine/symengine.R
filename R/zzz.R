
## We do not use "useDynLib" in NAMESPACE but control it in
## .onLoad() and .onUnload() manually.
## This gives us the flexibility to behave differently on windows
## and unix. On windows, we put additional dlls to the package
## folder and load them via `library.dynam` at run time. On Unix,
## we assume that the dependencies such as gmp are in system path.

library.dynam_2 <- function(chname, package, lib.loc, ...) {
    ## A wrapper around library.dynam that allows it to fail.
    ## Roxygen will try to load the package for generating documentation,
    ## which leads to
    ## > Error in find.package(package, lib.loc, verbose = verbose) : 
    ## >   there is no package called ‘symengine’
    pkgs <- find.package(package, lib.loc, quiet = TRUE)
    if (!length(pkgs)) {
        warning(sprintf("There is no package called %s", package))
        return()
    }
    library.dynam(chname, package, lib.loc, ...)
}

.onLoad <- function(libname, pkgname) {
    ## TODO: use assignInMyNamespace to update constants?
    if (.Platform$OS.type == "windows")
        return(.onLoad.windows(libname, pkgname))
    
    library.dynam_2("symengine", pkgname, libname)
}

.onUnload <- function(libpath) {
    if (.Platform$OS.type == "windows")
        return(.onUnload.windows(libpath))
    
    library.dynam.unload("symengine", libpath)
}

.onLoad.windows <- function(libname, pkgname) {
    library.dynam_2("libgmp-10"   , pkgname, libname)
    ## Currently using static library as workaround
    #library.dynam_2("libsymengine", pkgname, libname)
    library.dynam_2("symengine"   , pkgname, libname)
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
    
    ## Try to confirm the shared object is loaded, but allow it to fail
    if (!is.loaded("_symengine_compilation_notes")) {
        warning("The shared library is not loaded")
        return(invisible(FALSE))
    }

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


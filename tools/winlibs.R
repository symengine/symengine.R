
## Ensure we are in the correct directory
stopifnot(file.exists("../DESCRIPTION"))

## Links
symengine_win32 <- "https://github.com/Marlin-Na/symengine/raw/rdep-4/symengine-32.zip"
symengine_win64 <- "https://github.com/Marlin-Na/symengine/raw/rdep-4/symengine-64.zip"
gmp_win32       <- "https://github.com/Marlin-Na/symengine/raw/rdep-4/gmp-32.zip"
gmp_win64       <- "https://github.com/Marlin-Na/symengine/raw/rdep-4/gmp-64.zip"

is_32 <- function () {
    machine <- Sys.info()[["machine"]]
    
    if (machine %in% c("x86_64", "x86-64"))
        use_32 <- FALSE
    else if (machine == "x86")
        use_32 <- TRUE
    else
        stop("Unrecognized type of machine ", sQuote(machine))
}

## These functions will download the binaries to package directory and move
## the dll files to `inst/libs/{r_arch}/`.
download_lib <- function(link, download_dist) {
    ## Clean and create directory for compiling
    if (dir.exists(download_dist))
        unlink(download_dist, recursive = TRUE)
    dir.create(download_dist)
    
    ## Create directory to put DLLs
    r_arch <- .Platform$r_arch
    lib_dist <- sprintf("../inst/mylibs/%s", r_arch)
    if (!dir.exists(lib_dist))
        dir.create(lib_dist, recursive = TRUE)
    
    ## Download files
    download.file(link, destfile=file.path(download_dist, "download.zip"), quiet=FALSE)
    unzip(file.path(download_dist, "download.zip"), exdir=download_dist)
    unlink(file.path(download_dist, "download.zip"))
    
    ## TODO: do we need to rename libsymengine.dll?
    ## Put DLLs
    for (dll in list.files(file.path(download_dist, "bin"), full.names=TRUE)) {
        file.rename(dll, file.path(lib_dist, basename(dll)))
        message(sprintf("Put %s", basename(dll)))
    }
    invisible(TRUE)
}



download_symengine <- function() {
    link <- if(is_32()) symengine_win32 else symengine_win64
    download_dist <- "../win-symengine"
    download_lib(link, download_dist)
}

download_gmp <- function() {
    link <- if(is_32()) gmp_win32 else gmp_win64
    download_dist <- "../win-gmp"
    download_lib(link, download_dist)
}


download_symengine()
download_gmp()

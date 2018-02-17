
stopifnot(file.exists("../DESCRIPTION"))

if (!file.exists("../windows/usr/include/symengine/cwrapper.h")) {
    machine <- Sys.info()[["machine"]]

    if (machine == "x86_64")
        link <- "https://github.com/symengine/symengine/releases/download/v0.3.0/binaries-msvc-x86_64.tar.bz2"
    else if (machine == "x86")
        link <- "https://github.com/symengine/symengine/releases/download/v0.3.0/binaries-msvc-x86.tar.bz2"
    else
        stop("Unrecognized type of machine, which should be either 'x86' or 'x86_64' ??")

    download.file(link, "lib.tar.bz2", quiet = FALSE)
    dir.create("../windows")
    untar("lib.tar.bz2", exdir = "../windows", tar = "internal")
    unlink("lib.tar.bz2")
}



ask <- function(name, options) {
    opt_str <- options
    opt_str[1] <- sprintf("[%s]", opt_str[1])
    opt_str <- paste0(opt_str, collapse = "/")
    ask_str <- sprintf("%s %s: ", name, opt_str)
    
    if (interactive()) {
        user_input <- readline(ask_str)
        if (user_input == "")
            user_input <- options[1]
    }
    else
        user_input <- options[1]
    user_input
}

check_bin <- function(name) {
    if (nchar(Sys.which(name)) == 0L) {
        stop(sprintf("%s is not installed on your system", name))
    }
}

##========= Main ==========================

default_install_dir <- file.path("~/.local/rdeplibs-symengine")

check_bin("git")
check_bin("cmake")

if (!dir.exists(INSTALL_DIR))
    dir.create(path = INSTALL_DIR, recursive = TRUE)

cmake_options <- list(
    list(name = "CMAKE_INSTALL_PREFIX"  , options = c(default_install_dir)),
    list(name = "CMAKE_BUILD_TYPE"      , options = c("Release", "Debug")),
    list(name = "WITH_BFD"              , options = c("no", "yes")),
    list(name = "WITH_SYMENGINE_ASSERT" , options = c("no", "yes")),
    list(name = "WITH_ECM"              , options = c("no", "yes")),
    list(name = "WITH_PRIMESIEVE"       , options = c("no", "yes")),
    list(name = "WITH_FLINT"            , options = c("no", "yes")),
    list(name = "WITH_ARB"              , options = c("no", "yes")),
    list(name = "WITH_TCMALLOC"         , options = c("no", "yes")),
    list(name = "WITH_OPENMP"           , options = c("no", "yes")),
    list(name = "WITH_PIRANHA"          , options = c("no", "yes")),
    list(name = "WITH_LLVM"             , options = c("no", "yes")),
    list(name = "WITH_MPFR"             , options = c("yes", "no")),
    list(name = "WITH_MPC"              , options = c("yes", "no")),
    list(name = "INTEGER_CLASS", options = c("gmp", "gmpxx", "flint",
                                             "piranha", "boostmp"))
)

static_options <- list(
    list(name = "BUILD_TESTS"       , options = c("no")),
    list(name = "BUILD_BENCHMARKS"  , options = c("no")),
    list(name = "BUILD_SHARED_LIBS" , options = c("no"))
)

meta_options <- list(
    list(name = "SymEngine Commit" , options = c("master"))
)

for (i in seq_along(cmake_options)) {
    opt <- cmake_options[[i]]
    ans <- ask(opt$name, opt$options)
    cmake_options[[i]]$options <- ans
}

for (i in seq_along(meta_options)) {
    opt <- meta_options[[i]]
    ans <- ask(opt$name, opt$options)
    meta_options[[i]]$options <- ans
}

cmake_options  <- do.call(rbind, lapply(cmake_options,  as.data.frame, stringsAsFactors = FALSE))
static_options <- do.call(rbind, lapply(static_options, as.data.frame, stringsAsFactors = FALSE))
meta_options   <- do.call(rbind, lapply(meta_options,   as.data.frame, stringsAsFactors = FALSE))

rownames(cmake_options)  <- cmake_options$name
rownames(static_options) <- static_options$name
rownames(meta_options)   <- meta_options$name

cmake_options$name  <- NULL
static_options$name <- NULL
meta_options$name   <- NULL


message("===== Summary of your options =====")
print(rbind(cmake_options, static_options, meta_options))



message("===== Downloading SymEngine Repository =====")

symengine_commit <- meta_options["SymEngine Commit", "options"]
tmp_dir <- tempdir()

dist_file <- file.path(tmp_dir, "symengine.zip")
download_url <- paste0("https://github.com/symengine/symengine/archive/", symengine_commit, ".zip")

message(sprintf("- Downloading %s to %s", download_url, dist_file))
download.file(download_url, dist_file)

tmp_src_dir <- file.path(tmp_dir, paste0("symengine-", symengine_commit))

if (dir.exists(tmp_src_dir))
    unlink(tmp_src_dir, recursive = TRUE)

setwd(tmp_dir)
unzip(zipfile = dist_file)


message("===== Running CMake =====")

setwd(tmp_src_dir)
cmake_args <- rbind(cmake_options, static_options)
cmake_args <- `names<-`(cmake_args[,"options"], rownames(cmake_args))
cmake_args <- paste0("-D", names(cmake_args), "=", cmake_args)
cmake_args <- c(cmake_args, ".")

system2("cmake", args = cmake_args, stdout = stdout(), stderr = stderr())

message("===== Compiling =====")

system2("make", stdout = stdout(), stderr = stderr())

message("===== Installing =====")

system2("make", "install", stdout = stdout(), stderr = stderr())



args = commandArgs(trailingOnly=TRUE)

print_usage_and_exit <- function() {
    message("Usage: translate_to_cmake_flags.R <cmake_flag_to_translate> <c_flags>")
    quit(save="no", status=1)
}

if (length(args) != 2) {
    print_usage_and_exit()
}

cmake_flag_to_translate <- args[1]
c_flags <- strsplit(args[2], "\\s+")[[1]]

get_isystem_paths <- function(c_flags) {
    cmake_paths <- list()
    ptr <- 1
    while (ptr <= length(c_flags)) {
        if (c_flags[ptr] == "-isystem") {
            cmake_paths <- append(cmake_paths, c_flags[ptr+1])
            ptr <- ptr+2
            next
        }
        ptr <- ptr + 1
    }
    cmake_paths
}

get_I_paths <- function(c_flags) {
    cmake_paths <- list()
    ptr <- 1
    while (ptr <= length(c_flags)) {
        if (c_flags[ptr] == "-I") {
            cmake_paths <- append(cmake_paths, c_flags[ptr+1])
            ptr <- ptr+2
            next
        }
        if (startsWith(c_flags[ptr], "-I")) {
            cmake_paths <- append(cmake_paths, gsub("^-I", "", c_flags[ptr]))
            ptr <- ptr + 1
            next
        }
        ptr <- ptr + 1
    }
    cmake_paths
}

get_L_paths <- function(c_flags) {
    cmake_paths <- list()
    ptr <- 1
    while (ptr <= length(c_flags)) {
        if (c_flags[ptr] == "-L") {
            cmake_paths <- append(cmake_paths, c_flags[ptr+1])
            ptr <- ptr+2
            next
        }
        if (startsWith(c_flags[ptr], "-L")) {
            cmake_paths <- append(cmake_paths, gsub("^-L", "", c_flags[ptr]))
            ptr <- ptr + 1
            next
        }
        ptr <- ptr + 1
    }
    cmake_paths
}

if (cmake_flag_to_translate == "CMAKE_INCLUDE_PATH") {
    cmake_paths <- c(get_I_paths(c_flags), get_isystem_paths(c_flags))
    cat(paste0(cmake_paths, collapse=";"))
} else if (cmake_flag_to_translate == "CMAKE_LIBRARY_PATH") {
    cmake_paths <- get_L_paths(c_flags)
    cat(paste0(cmake_paths, collapse=";"))
} else {
    message("Supported <cmake_flag_to_translate> values:
  - CMAKE_INCLUDE_PATH
  - CMAKE_LIBRARY_PATH")
    quit(save="no", status=1)
}

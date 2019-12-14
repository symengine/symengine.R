#!/usr/bin/env Rscript

build_doc_site <- function(dist) {
    ## Check current directory
    description_file <- list.files(pattern = "^DESCRIPTION$")
    stopifnot(length(description_file) == 1)
    stopifnot(read.dcf(description_file)[, "Package"] == "symengine")
    
    if (dir.exists(dist)) {
        unlink(dist, recursive=TRUE, force=TRUE)
    }
    
    dist_man <- file.path(dist, "man")
    dist_doc <- file.path(dist, "doc")
    
    dir.create(dist)
    dir.create(dist_man)
    dir.create(dist_doc)
    
    vignettes <- list.files("vignettes", pattern = "Rmd$", full.names = TRUE)
    res_files <- list()
    for (file in vignettes) {
        res_file <- rmarkdown::render(file)
        res_files <- append(res_files, res_file)
    }
    res_files <- unlist(res_files)
    on.exit(unlink(res_files))
    
    ## Copy vignettes to doc/
    for (res_file in res_files) {
        file.copy(res_file, file.path(dist_doc, basename(res_file)))
    }
    
    ## Convert Rd files to html files in man/
    rdfiles <- list.files("man", pattern = "Rd$", full.names = TRUE)
    for (rdfile in rdfiles) {
        rd <- tools::parse_Rd(rdfile)
        rd_dist <- file.path(dist_man,  gsub("\\.Rd$", "\\.html", basename(rdfile)))
        tools::Rd2HTML(rd, out = rd_dist)
    }
    
    ## Build and copy index.html
    index_rmd <- "tools/build_doc_site_asset/index.Rmd"
    index_html <- rmarkdown::render(index_rmd)
    file.copy(index_html, file.path(dist, "index.html"))
    file.remove(index_html)
    
    return(TRUE)
}

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
    cat("Usage: ./tools/build_doc_site.R directory\n")
    quit(save = "no", status = 1)
}
build_doc_site(args)


#!/usr/bin/env Rscript --vanilla
get_char <- function(filename) {
    readChar(filename, file.info(filename)$size)
}

get_yaml <- function(x) {
    regmatches(x, regexec("---\\n(.*?)\\n---", x))[[1]][2]
}

get_rest <- function(x)
    gsub("---\\n.*?\\n---", "", x)

rmd2spin <- function(x) {
    spin <- gsub("```\\{r(.*?)}", "```#+\\1", x)
    split <- strsplit(spin, "\\n```|```\\n")[[1]]
    trimmed <- as.character(lapply(split, trimws))
    trimmed[trimmed != ""]
}

sunder <- function(input_filename) {
    fname <- tools::file_path_sans_ext(input_filename)
    text <- get_char(input_filename)
    code_and_text <- rmd2spin(get_rest(text))
    n_digits <- floor(log10(length(code_and_text))) + 1
    n <- 0
    ending <- sprintf(paste0("%0", n_digits, "d"), n)
    cat(get_yaml(text), file = paste0(fname, "_", ending, ".yaml"))
    for (i in code_and_text) {
        n = n + 1
        ending <- sprintf(paste0("%0", n_digits, "d"), n)
        if (startsWith(i, "#+")) {
            cat(i, file = paste0(fname, "_", ending, ".R"))
        } else {
            cat(i, file = paste0(fname, "_", ending, ".md"))
        }
    }
}

sunder("Day1_Theoph.Rmd")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1) {
    sunder(args)
} else if (length(args) > 1) {
    sapply(args, sunder)
}

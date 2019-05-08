get_char <- function(filename) {
    readChar(filename, file.info("example.Rmd")$size)
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
    cat(get_yaml(text), file = paste0(fname, "_00.yaml"))
    n <- 0
    for (i in rmd2spin(get_rest(text))) {
        n = n + 1
        if (startsWith(i, "#+")) {
            cat(i, file = paste0(fname, "_", sprintf("%02d", n), ".R"))
        } else {
            cat(i, file = paste0(fname, "_", sprintf("%02d", n), ".md"))
        }
    }
}

sunder("example.Rmd")

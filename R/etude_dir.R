#' Make a listing of an etude directory
#'
#' Etude files are stored in a directory, typically "Exercises". This  function
#' scans through  the RMD files in such  a directory and constructs a data frame
#' based  on  the YAML. The data frame can be useful, for instance, for including
#' in a parent file using `etude_list()`.
#'
#' @param where the name of the folder containing the etude RMD files
#'
#' @export
etude_dir <- function(where) {
  Rmd_files <- list.files(where, pattern="\\.[Rr]md$",
                   full.names = TRUE, recursive = TRUE)
  Result <- list()
  for (k  in 1:length(Rmd_files)) {
    yaml_head <- read_yaml_head(Rmd_files[k])
    yaml_head$file <- Rmd_files[k]
    Result[[k]] <- yaml_head
  }
  return(dplyr::bind_rows(Result))
}


read_yaml_head <- function(file) {
  contents <- readLines(file)
  fences <- grep("^---\\s?$",  contents)
  if (  (!(1 %in% fences)) || length(fences) < 2)
    return(list(status = "No YAML"))
  yaml_lines <- 2:(fences[2] - 1)

  Rform <- yaml::yaml.load(contents[yaml_lines])
  names(Rform)[names(Rform) == "topics"] <- "tags"
  Rform$status <- "OK"
  if ("version" %in% names(Rform))
    Rform$version <- as.character(Rform$version)
  for (field in  names(Rform))
    Rform[field] <- paste(Rform[[field]], collapse  = ":::")

  Rform
}

createListIndex <- function(x, sep = c("[['", "']][['", "']]"),
  type = c("compose", "decompose")
) {
  if (type == "compose") {
    paste0(sep[1], paste(x, collapse = sep[2]), sep[3])
  } else if (type == "decompose")  {
    tmp <- strsplit(x, "/")
    sapply(tmp, function(ii) {
      paste0(sep[1], paste(ii, collapse = sep[2]), sep[3])
    })
  }
}

mapTreeFormat <- function(
  values = list(),
  include_root = TRUE,
  overwrite = FALSE
) {
  branches <- names(values)
  branches <- gsub("\\$", "/", branches)
  names(values) <- branches
  leafs <- values

  branches <- branches[branches != ""]
  leafs <- leafs[leafs != ""]

  splitted <- strsplit(branches, "/")
  #   level_1=splitted[[1]]
  #   level_2=2
  out <- list()
  for(level_1 in splitted) {
    for(level_2 in 1:length(level_1)) {
      idx <- createListIndex(level_1[1:level_2])
      path <- createListIndex(level_1[1:level_2], sep = c("", "/", ""))
      expr_get <- sprintf("%s%s", "out", idx)
      expr_set <- if (path %in% names(leafs)) {
        if (is.null(leafs[[path]])) {
          leafs[[path]] <- list()
        }
        sprintf("%s%s <- leafs[[path]]", "out", idx)
      } else {
        # sprintf("%s%s <- list()", "out", idx)
        sprintf("%s%s <- new.env(parent = emptyenv())", "out", idx)
      }
      tmp <- eval(parse(text = expr_get))
      if (is.null(tmp)) {
        eval(parse(text = expr_set))
      } else {
        if (overwrite) {
          eval(parse(text = expr_set))
        } else {
          message("Exists:")
          print(path)
          print(expr_get)
          print(expr_set)
        }
      }
    }
  }
  if (!include_root) {
    out <- out[[1]]
  }
  out
}

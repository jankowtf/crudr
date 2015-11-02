library(R6)
#' @title
#' Interface Class for CRUD Operations
#'
#' @description
#' Defines the interface for CRUD operations.
#'
#' @template interface_section
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{\code{getMain}} {
#'
#'    Get main field value
#'  }
#'  \item{\code{setMain}} {
#'
#'    Set main field value
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{init} {
#'
#'    Has
#'  }
#'  \item{has} {
#'
#'    Has
#'  }
#'  \item{create} {
#'
#'    Create
#'  }
#'  \item{read} {
#'
#'    Read
#'  }
#'  \item{update} {
#'
#'    Update
#'  }
#'  \item{delete} {
#'
#'    Delete
#'  }
#'  \item{reset} {
#'
#'    Reset
#'  }
#' }
#'
#' @section Known interface implementations:
#'
#' \itemize{
#'  \item{\code{\link[crudr]{Crud}}}
#'  \item{\code{\link[crudr]{Crud.Settings}}}
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-ICrud.R
#' @import R6
#' @export
ICrud <- R6Class(
  classname = "ICrud",
  portable = TRUE,
  public = list(
    getMain = function() {
      private$stopIfInterface()
    },
    setMain = function(...) {
      private$stopIfInterface()
    },
    init = function(...) {
      private$stopIfInterface()
    },
    has = function(...) {
      private$stopIfInterface()
    },
    create = function(...) {
      private$stopIfInterface()
    },
    read = function(...) {
      private$stopIfInterface()
    },
    update = function(...) {
      private$stopIfInterface()
    },
    delete = function(...) {
      private$stopIfInterface()
    },
    reset = function(...) {
      private$stopIfInterface()
    }
  ),
  private = list(
    stopIfInterface = function(cl = sys.call(-1)) {
      stop(paste0(class(self)[1], ": ",
        gsub(".*\\$", "", as.character(cl)[1]),
        ": this is the interface"))
    }
  )
)

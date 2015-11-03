#' @title
#' Create access expression
#'
#' @description
#' Creates an access expression. This either is an expression for value
#' extraction involving the syntactic sugars for extraction (\code{[[} or
#' \code{$}; see \code{\link[base]{Extract}}) or one that also involves
#' the syntactic sugar for assignments
#' (\code{<-}; see \code{\link[base]{assignOps}})
#'
#' @details
#' Known methods:
#'
#' \itemize{
#'  \item{\code{\link[crudr]{createAccessExpression.default}}}
#' }
#'
#' For a faster but less comprehensive version see:
#' \code{\link[crudr]{createAccessExpressionPlain}}
#'
#' @param inst Object for method dispatch.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-createAccessExpression.R
#' @seealso \code{\link[base]{Extract}, \link[base]{assignOps}}
#' @template authors
#' @template references
#' @export
createAccessExpression <- function(inst, ...) {
  UseMethod("createAccessExpression", inst)
}

#' @title
#' Create access expression
#'
#' @description
#' Default method of \code{\link[crudr]{createAccessExpression}}.
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{list}}.
#'  TODO
#' @param idx \code{\link[base]{character}}.
#'  TODO
#' @param value \code{ANY} (\code{\link[methods]{BasicClasses}}).
#'  Optional assignment value.
#' @param name_obj \code{\link[base]{character}}.
#'  Name of the actual object in the expression that values are extracted from
#'  or assigned to.
#' @param name_value \code{\link[base]{character}}.
#'  TODO
#' @param evaluate \code{\link[base]{logical}}.
#'  TODO
#' @param sugar \code{\link[base]{character}}.
#'  Syntactic sugar function to use.
#' @param as_name_obj \code{\link[base]{logical}}.
#'  TODO
#' @param as_name_value \code{\link[base]{logical}}.
#'  TODO
#' @param eval_in_parent \code{\link[base]{logical}}.
#'  TODO
#' @param return_conventional \code{\link[base]{logical}}.
#'  Only relevant if \code{!is.null(value)}. Return conventional return value
#'  of assignments via \code{<-} or more plausible information in typical
#'  contexts where this functions might be used. These are:
#'  \itemize{
#'    \item{updated object \code{inst} if \code{eval_in_parent = FALSE}}
#'    \item{\code{NULL} if \code{eval_in_parent = TRUE} in order to stress the
#'    point that an object has been altered in the parent frame}
#'  }
#' @template threedots
#' @return \code{\link[base]{expression}} or evaluated get expression
#'  if \code{evaluate = TRUE}. Exact value also depends on
#'  \code{return_conventional}
#'  (in interaction with \code{value} and \code{eval_in_parent})
#' @example inst/examples/example-createAccessExpression.R
#' @seealso \code{\link[crudr]{createAccessExpression}}
#' @template authors
#' @template references
#' @export
createAccessExpression.default <- function(
  inst,
  idx = character(),
  value = NULL,
  name_obj = "inst",
  name_value = "value",
  evaluate = FALSE,
  sugar = c("[[", "$"),
  as_name_obj = TRUE,
  as_name_value = FALSE,
  eval_in_parent = FALSE,
  return_conventional = TRUE,
  ...
) {
  envir <- if (!eval_in_parent) {
    envir <- environment()
  } else {
    parent.frame()
  }
  sugar <- match.arg(sugar, c("[[", "$"))

  name_obj <- if (as_name_obj) as.name(name_obj) else inst
  name_value <- if (as_name_value) as.name(name_value) else value

  if (sugar == "[[") {
    if (is.null(value)) {
      expr <- substitute(FUN(X, INDEX),
        list(FUN = as.name('[['), X = name_obj,
          INDEX = idx))
    } else {
      expr <- substitute(FUN(X, INDEX) <- VALUE,
        list(FUN = as.name('[['), X = name_obj, INDEX = idx,
          VALUE = name_value)
      )
    }
  } else if (sugar == "$") {
    #       expr <- substitute(FUN(X, INDEX),
    #         list(FUN = as.name('$'), X = as.name(name_obj), INDEX = idx))
    for (ii in 1:length(idx)) {
      if (ii == 1) {
        X <- name_obj
      } else {
        X <- expr
      }
      INDEX <- idx[ii]
      expr <- substitute(FUN(X, INDEX),
        list(FUN = as.name('$'), X = X, INDEX = INDEX))
    }
    if (!is.null(value)) {
      expr <- substitute(OBJ <- VALUE,
        list(OBJ = expr, VALUE = name_value)
      )
    }
    expr
  }
  if (evaluate) {
    out <- eval(expr, envir = envir)
    if (!is.null(value) && !return_conventional) {
      out <- if (eval_in_parent) {
        NULL
      } else {
        inst
      }
    }
    out
  } else {
    expr
  }
}

#' @title
#' Create access expression
#'
#' @description
#' Plain and thus faster version of
#' \code{\link[crudr]{createAccessExpression}} for
#' scenarios where computational costs really matter.
#'
#' @details
#' Known methods:
#'
#' \itemize{
#'  \item{\code{\link[crudr]{createAccessExpressionPlain.default}}}
#' }
#'
#' For a more comprehensive but slower version see:
#' \code{\link[crudr]{createAccessExpression}}
#'
#' @param inst Object for method dispatch.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-createAccessExpressionPlain.R
#' @seealso \code{\link[base]{Extract}, \link[base]{assignOps}}
#' @template authors
#' @template references
#' @export
createAccessExpressionPlain <- function(inst, ...) {
  UseMethod("createAccessExpressionPlain", inst)
}

#' @title
#' Create access expression
#'
#' @description
#' Default method of \code{\link[crudr]{createAccessExpressionPlain}}.
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{list}}.
#'  TODO
#' @param idx \code{\link[base]{character}}.
#'  TODO
#' @param value \code{ANY} (\code{\link[methods]{BasicClasses}}).
#'  Optional assignment value.
#' @param name \code{\link[base]{character}}.
#'  Name of the actual object in the expression that values are extracted from
#'  or assigned to.
#' @template threedots
#' @return \code{\link[base]{expression}}.
#' @example inst/examples/example-createAccessExpressionPlain.R
#' @seealso \code{
#'  \link[crudr]{createAccessExpressionPlain},
#'  \link[crudr]{createAccessExpression}
#' }
#' @template authors
#' @template references
#' @export
createAccessExpressionPlain.default <- function(
  inst,
  idx = character(),
  value = NULL,
  name = "inst",
  ...
) {
  if (is.null(value)) {
    substitute(FUN(X, INDEX),
      list(FUN = as.name('[['), X = as.name(name), INDEX = idx))
  } else {
    substitute(FUN(X, INDEX) <- VALUE,
      list(FUN = as.name('[['), X = as.name(name), INDEX = idx,
        VALUE = value)
    )
  }
}

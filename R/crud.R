
# createCrudExpression --------------------------------------------------

#' @title
#' Create CRUD expression
#'
#' @description
#' Creates an CRUD expression. This either is an expression for value
#' extraction involving the syntactic sugars for extraction (\code{[[} or
#' \code{$}; see \code{\link[base]{Extract}}) or one that also involves
#' the syntactic sugar for assignments
#' (\code{<-}; see \code{\link[base]{assignOps}})
#'
#' @details
#' Known methods:
#'
#' \itemize{
#'  \item{\code{\link[crudr]{createCrudExpression.default}}}
#' }
#'
#' For a faster but less comprehensive version see:
#' \code{\link[crudr]{createCrudExpressionPlain}}
#'
#' @param inst Object for method dispatch.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-createCrudExpression.R
#' @seealso \code{\link[base]{Extract}, \link[base]{assignOps}}
#' @template authors
#' @template references
#' @export
createCrudExpression <- function(inst, ...) {
  UseMethod("createCrudExpression", inst)
}

#' @title
#' Create CRUD expression
#'
#' @description
#' Default method of \code{\link[crudr]{createCrudExpression}}.
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{list}}.
#'  TODO
#' @param id \code{\link[base]{character}}.
#'  TODO
#' @param value \code{ANY} (\code{\link[methods]{BasicClasses}}).
#'  Optional assignment value.
#' @param name_obj \code{\link[base]{character}}.
#'  Name of the actual object in the expression that values are extracted from
#'  or assigned to.
#' @param name_value \code{\link[base]{character}}.
#'  TODO
#' @param as_name_value \code{\link[base]{logical}}.
#'  TODO
#' @param in_parent \code{\link[base]{logical}}.
#'  TODO
#' @param evaluate \code{\link[base]{logical}}.
#'  TODO
#' @param sugar \code{\link[base]{character}}.
#'  Syntactic sugar function to use.
#' @param as_name_obj \code{\link[base]{logical}}.
#'  TODO
#' @param return_conventional \code{\link[base]{logical}}.
#'  Only relevant if \code{!is.null(value)}. Return conventional return value
#'  of assignments via \code{<-} or more plausible information in typical
#'  contexts where this functions might be used. These are:
#'  \itemize{
#'    \item{updated object \code{inst} if \code{in_parent = FALSE}}
#'    \item{\code{NULL} if \code{in_parent = TRUE} in order to stress the
#'    point that an object has been altered in the parent frame}
#'  }
#' @param strict \code{\link[base]{numeric}}.
#'  Strictness levels:
#'  \itemize{
#'    \item{0: }{no condition is signaled}
#'    \item{1: }{message is signaled}
#'    \item{2: }{warning is signaled}
#'    \item{3: }{error is signaled}
#'  }
#' @param fail_safe \code{\link[base]{logical}}.
#'  Wrap with \code{\link[base]{try}}.
#' @param use_tree \code{\link[base]{logical}}.
#'  Use expression tree as returned by
#'    \code{\link[crudr]{createCrudExpressionTree}}.
#' @param allow_null \code{\link[base]{logical}}.
#'  Allows \code{value = NULL}. Useful for deleting values.
#' @param affect_branch \code{\link[base]{logical}}.
#'  Only relevant when \code{value = NULL} in which case a value will be
#'  deleted. Unless we use \code{use_tree = TRUE}, the function has no means
#'  of distinguishing the provided \code{id} belongs to a branch or a leaf in
#'  the hierarchy. Setting this to \code{TRUE} will delete the entire branch
#'  while \code{FALSE} will keep the branch with a value of
#'  \code{structure(list(), names = character())}.
#' @template threedots
#' @return \code{\link[base]{expression}} or evaluated CRUD expression
#'  if \code{evaluate = TRUE}. Exact value also depends on
#'  \code{return_conventional}
#'  (in interaction with \code{value} and \code{in_parent})
#' @example inst/examples/example-createCrudExpression.R
#' @seealso \code{\link[crudr]{createCrudExpression}}
#' @template authors
#' @template references
#' @export
createCrudExpression.default <- function(
  inst,
  id = character(),
  value = NULL,
  # value,
  name_obj = "inst",
  name_value = "value",
  as_name_obj = TRUE,
  as_name_value = FALSE,
  evaluate = FALSE,
  sugar = c("[[", "$"),
  in_parent = FALSE,
  return_conventional = TRUE,
  strict = 0:3,
  fail_safe = FALSE,
  use_tree = FALSE,
  allow_null = FALSE,
  affect_branch = FALSE,
  ...
) {
  envir <- if (!in_parent) {
    envir <- environment()
  } else {
    parent.frame()
  }
  sugar <- match.arg(sugar, c("[[", "$"))

  ## Automatic convenvience overwrites
  if (in_parent) {
    evaluate <- TRUE
  }
  if (!missing(value)) {
    allow_null <- TRUE
  }
#   } else {
#     value <- NULL
#     ## TODO 2015-11-04: investigate why this is necessary
#   }
  if (is.null(value) && allow_null && !affect_branch) {
    value <- structure(list(), names = character())
  }

  ## Ensure vectorized index //
  id <- unlist(strsplit(id, "/"))

  name_obj <- if (as_name_obj) as.name(name_obj) else inst
  name_value <- if (as_name_value) as.name(name_value) else value

  if (sugar == "[[") {
    # if (is.null(value)) {
    if (is.null(value) && !allow_null) {
      expr <- substitute(FUN(X, INDEX),
        list(FUN = as.name('[['), X = name_obj,
          INDEX = id))
      print(expr)
    } else {
      expr <- substitute(FUN(X, INDEX) <- VALUE,
        list(FUN = as.name('[['), X = name_obj, INDEX = id,
          VALUE = name_value)
      )
    }
  } else if (sugar == "$") {
    for (ii in 1:length(id)) {
      if (ii == 1) {
        X <- name_obj
      } else {
        X <- expr
      }
      INDEX <- id[ii]
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
    # if (is.null(value)) {
    if (is.null(value) && !allow_null) {
      ## --> read scenario
      out <- if (!fail_safe) {
        eval(expr, envir = envir)
      } else {
        try(eval(expr, envir = envir), silent = TRUE)
      }
    } else {
      ## --> create scenario
      if (!use_tree) {
        out <- try(eval(expr, envir = envir), silent = TRUE)
      } else {
        out <- structure(NULL, class = "try-error")
      }
      if (inherits(out, "try-error")) {
        applyStrictnessLevel(strict, "bridging required")

        tree <- createCrudExpressionTree(
          inst = inst,
          id = id,
          value = name_value,
          name_obj = name_obj,
          name_value = name_value,
          fail_safe = TRUE
        )
        exists_idx <- applyCrudExpressionTree(tree, "has",
          fail_safe = TRUE)
        # exists_idx <- applyCrudExpressionTree(tree, "read")
        # print(exists_idx)
        missing_idx <- sapply(exists_idx, function(ii) {
          ## If via type = "has":
          identical(ii, FALSE) || inherits(ii, "try-error")
          ## If via type = "read":
          # is.null(ii) || inherits(ii, "try-error")
        })
        tree <- tree[missing_idx]
        out <- applyCrudExpressionTree(tree, "create", envir = envir)
        out <- out[[length(out)]]
      }

      if (!return_conventional) {
        out <- if (in_parent) {
          NULL
        } else {
          inst
        }
      }
    }
    out
  } else {
    expr
  }
}

# createCrudExpressionPlain ---------------------------------------------

#' @title
#' Create access expression
#'
#' @description
#' Plain and thus faster version of
#' \code{\link[crudr]{createCrudExpression}} for
#' scenarios where computational costs really matter.
#'
#' @details
#' Known methods:
#'
#' \itemize{
#'  \item{\code{\link[crudr]{createCrudExpressionPlain.default}}}
#' }
#'
#' For a more comprehensive but slower version see:
#' \code{\link[crudr]{createCrudExpression}}
#'
#' @param inst Object for method dispatch.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-createCrudExpressionPlain.R
#' @seealso \code{\link[base]{Extract}, \link[base]{assignOps}}
#' @template authors
#' @template references
#' @export
createCrudExpressionPlain <- function(inst, ...) {
  UseMethod("createCrudExpressionPlain", inst)
}

#' @title
#' Create access expression
#'
#' @description
#' Default method of \code{\link[crudr]{createCrudExpressionPlain}}.
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{list}}.
#'  TODO
#' @param id \code{\link[base]{character}}.
#'  TODO
#' @param value \code{ANY} (\code{\link[methods]{BasicClasses}}).
#'  Optional assignment value.
#' @param name \code{\link[base]{character}}.
#'  Name of the actual object in the expression that values are extracted from
#'  or assigned to.
#' @param allow_null \code{\link[base]{logical}}.
#'  Allows \code{value = NULL}. Useful for deleting values.
#' @template threedots
#' @return \code{\link[base]{expression}}.
#' @example inst/examples/example-createCrudExpressionPlain.R
#' @seealso \code{
#'  \link[crudr]{createCrudExpressionPlain},
#'  \link[crudr]{createCrudExpression}
#' }
#' @template authors
#' @template references
#' @export
createCrudExpressionPlain.default <- function(
  inst,
  id = character(),
  value = NULL,
  name = "inst",
  allow_null = FALSE,
  ...
) {
  ## Ensure vectorized index //
  id <- unlist(strsplit(id, "/"))

  if (is.null(value) && !allow_null) {
    substitute(FUN(X, INDEX),
      list(FUN = as.name('[['), X = as.name(name), INDEX = id))
  } else {
    substitute(FUN(X, INDEX) <- VALUE,
      list(FUN = as.name('[['), X = as.name(name), INDEX = id,
        VALUE = value)
    )
  }
}

# createCrudExpressionTree ----------------------------------------------

#' @title
#' Create access expression tree
#'
#' @description
#' Creates a tree of access expression.
#' See \code{\link[crudr]{createCrudExpression}}.
#'
#' @details
#' Known methods:
#'
#' \itemize{
#'  \item{\code{\link[crudr]{createCrudExpressionTree.default}}}
#' }
#'
#' @param inst Object for method dispatch.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-createCrudExpressionTree.R
#' @seealso \code{\link[base]{Extract}, \link[base]{assignOps}}
#' @template authors
#' @template references
#' @export
createCrudExpressionTree <- function(inst, ...) {
  UseMethod("createCrudExpressionTree", inst)
}

#' @title
#' Create access expression tree
#'
#' @description
#' Default method of \code{\link[crudr]{createCrudExpressionTree}}.
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{list}}.
#'  TODO
#' @param id \code{\link[base]{character}}.
#'  TODO
#' @param value \code{ANY} (\code{\link[methods]{BasicClasses}}).
#'  Optional assignment value.
#' @param name_obj \code{\link[base]{character}}.
#'  Name of the actual object in the expression that values are extracted from
#'  or assigned to.
#' @param name_value \code{\link[base]{character}}.
#'  TODO
#' @param as_name_obj \code{\link[base]{logical}}.
#'  TODO
#' @param as_name_value \code{\link[base]{logical}}.
#'  TODO
#' @param fail_safe \code{\link[base]{logical}}.
#'  Wrap with \code{\link[base]{try}}.
#' @template threedots
#' @return \code{\link[base]{expression}}.
#' @example inst/examples/example-createCrudExpressionTree.R
#' @seealso \code{
#'  \link[crudr]{createCrudExpressionTree}
#' }
#' @template authors
#' @template references
#' @export
createCrudExpressionTree.default <- function(
  inst,
  id = character(),
  value = NULL,
  name_obj = "inst",
  name_value = "value",
  as_name_obj = TRUE,
  as_name_value = FALSE,
  fail_safe = FALSE,
  ...
) {
  name_obj <- if (as_name_obj) as.name(name_obj) else inst
  name_value <- if (as_name_value) as.name(name_value) else value

  out <- lapply(1:length(id), function(ii) {
    this <- id[1:ii]
    WHERE <- if (ii == 1) {
      name_obj
    } else {
      substitute(FUN(X, INDEX),
        list(FUN = as.name('[['), X = name_obj,
          INDEX = id[1:(ii - 1)])
      )
    }
    value <- if (ii < length(id)) {
      # list()
      list()
    } else {
      #       if (wrap_in_list) {
      #         list(name_value)
      #       } else {
      name_value
      # }
    }
    expr_list <- list(
      has = if (!fail_safe) {
        substitute(
          FUN(X, WHERE, inherits = FALSE),
          list(
            FUN = as.name('exists'),
            X = id[ii],
            WHERE = WHERE
          )
        )
      } else {
        substitute(
          try(FUN(X, WHERE, inherits = FALSE), silent = TRUE),
          list(
            FUN = as.name('exists'),
            X = id[ii],
            WHERE = WHERE
          )
        )
      },
      read = if (!fail_safe) {
        substitute(
          FUN(X, INDEX),
          list(
            FUN = as.name('[['),
            X = name_obj,
            INDEX = this
          )
        )
      } else {
        substitute(
          try(FUN(X, INDEX), silent = TRUE),
          list(
            FUN = as.name('[['),
            X = name_obj,
            INDEX = this
          )
        )
      },
      create = if (!is.null(value)) {
        if (!fail_safe) {
          if (ii < length(id)) {
            substitute(
              {
                FUN(X, INDEX) <- list(list())
                FUN(X, INDEX) <- VALUE
              },
              list(
                FUN = as.name('[['),
                X = name_obj,
                INDEX = this,
                VALUE = value
              )
            )
          } else {
            substitute(
              {
                FUN(X, INDEX) <- list(list())
                FUN(X, INDEX) <- VALUE
              },
              # FUN(X, INDEX) <- VALUE,
              list(
                FUN = as.name('[['),
                X = name_obj,
                INDEX = this,
                VALUE = value
              )
            )
          }
        } else {
          if (ii < length(id)) {
            substitute(
              {
                try(FUN(X, INDEX) <- list(list()), silent = TRUE)
                try(FUN(X, INDEX) <- VALUE, silent = TRUE)
              },
              list(
                FUN = as.name('[['),
                X = name_obj,
                INDEX = this,
                VALUE = value
              )
            )
          } else {
            substitute(
              {
                try(FUN(X, INDEX) <- list(list()), silent = TRUE)
                try(FUN(X, INDEX) <- VALUE, silent = TRUE)
              },
              # try(FUN(X, INDEX) <- VALUE, silent = TRUE),
              list(
                FUN = as.name('[['),
                X = name_obj,
                INDEX = this,
                VALUE = value
              )
            )
          }
        }
      } else {
        NULL
      },
      delete = if (!fail_safe) {
        substitute(
          FUN(X, INDEX) <- NULL,
          list(
            FUN = as.name('[['),
            X = name_obj,
            INDEX = this
          )
        )
      } else {
        substitute(
          try(FUN(X, INDEX) <- NULL, silent = TRUE),
          list(
            FUN = as.name('[['),
            X = name_obj,
            INDEX = this
          )
        )
      }
    )
    expr_list$update <- expr_list$create
    expr_list
  })
  names(out) <- sapply(1:length(id), function(ii) paste(id[1:ii], collapse = "/"))
  out
}

#' @title
#' Apply access expression tree
#'
#' @description
#' Applies a tree of access expressions.
#' See \code{\link[crudr]{applyCrudExpressionTree}}.
#'
#' @details
#' Known methods:
#'
#' \itemize{
#'  \item{\code{\link[crudr]{applyCrudExpressionTree.default}}}
#' }
#'
#' @param inst Object for method dispatch.
#' @template threedots
#' @return See respective methods.
#' @example inst/examples/example-applyCrudExpressionTree.R
#' @seealso \code{\link[crudr]{createCrudExpressionTree}}
#' @template authors
#' @template references
#' @export
applyCrudExpressionTree <- function(inst, ...) {
  UseMethod("applyCrudExpressionTree", inst)
}

#' @title
#' Apply access expression tree
#'
#' @description
#' Default method of \code{\link[crudr]{applyCrudExpressionTree}}.
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{list}}.
#'  TODO
#' @param type \code{\link[base]{character}}.
#'  Type of expressions to apply.
#' @param envir \code{\link[base]{environment}}.
#'  Environment in which to evaluate the expressions.
#' @param fail_safe \code{\link[base]{logical}}.
#'  Wrap with \code{\link[base]{try}}.
#' @template threedots
#' @return \code{\link[base]{expression}}.
#' @example inst/examples/example-applyCrudExpressionTree.R
#' @seealso \code{
#'  \link[crudr]{createCrudExpressionTree}
#' }
#' @template authors
#' @template references
#' @export
applyCrudExpressionTree.default <- function(
  inst,
  type = c("has", "create", "read", "update", "delete"),
  envir = parent.frame(),
  fail_safe = FALSE,
  ...
) {
  # print(ls(envir))
  type <- match.arg(type, c("has", "create", "read", "update", "delete"))
  if (type == "delete") {
    inst <- rev(inst)
  }
  # ii = inst[[2]]
  lapply(inst, function(ii) {
    if (!fail_safe) {
      eval(ii[[type]], envir = envir)
    } else {
      try(eval(ii[[type]], envir = envir), silent = TRUE)
    }
  })
}

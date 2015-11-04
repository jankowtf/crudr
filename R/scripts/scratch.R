inst <- Crud.Settings$new()
inst$init(github_user = "rappster", git_branch = "master")
options("myapp" = inst)

getOption("myapp")$has("github_user")
getOption("myapp")$read("github_user")
getOption("myapp")$read("github_user", "git_branch")

# Nested ------------------------------------------------------------------

createListIndex(letters)
values <- list("github/user" = "rappster", "git/branch" = "master")
x <- mapTreeFormat(values)
y <- do.call(settings::options_manager, x)
y("github")$user
y("git")$branch


values <- list("github/user" = "rappster", "git/branch" = "master",
  "github/user" = "horst")
x <- mapTreeFormat(values)
x <- mapTreeFormat(values, overwrite = TRUE)
x$github$user
y <- do.call(settings::options_manager, x)
y("github")$user
y("git")$branch


# Combine -----------------------------------------------------------------

inst <- Crud.Settings$new()
values <- list("github/user" = "rappster", "git/branch" = "master")
x <- mapTreeFormat(values)
do.call(inst$init, x)
options("myapp" = inst)

query <- "github/user"
foo <- function(query) {
  idx <- createListIndex(query, type = "decompose")
  core <- getOption("myapp")$getMain()()

  expr <- sprintf("%s%s", "core", idx)
  eval(parse(text = expr))
}
foo(query = "github/user")
foo(query = "git/branch")

##-----


## Source every function file.
## here::here() anchors on the .Rproj / .here file at the project root, so the
## working directory at launch does not matter.
local({
  root <- tryCatch(here::here(), error = function(e) ".")
  d    <- file.path(root, "R")
  if (!dir.exists(d)) d <- if (dir.exists("R")) "R" else file.path("..", "R")
  invisible(lapply(sort(list.files(d, pattern = "[.]R$", full.names = TRUE)), source))
})

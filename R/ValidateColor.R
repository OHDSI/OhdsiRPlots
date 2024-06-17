# Custom color validation function
validateColor <- function(color) {
  tryCatch({
    col2rgb(color)
  }, error = function(e) {
    stop(sprintf("Invalid color: %s", color))
  })
}
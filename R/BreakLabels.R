# Helper function to adjust labels with line breaks
breakLabels <- function(labels) {
  # Use sapply to apply the wrapping function to each label
  output <- sapply(labels, function(label) {
    # Wrap the text if it's longer than 40 characters
    if (nchar(label) > 40) {
      paste(stringr::str_wrap(label, 40), collapse = "\n")
    } else {
      label
    }
  })
  return(output)
}
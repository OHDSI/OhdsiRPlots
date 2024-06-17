#' Create an OHDSI-themed color palette
#'
#' This function generates a color palette based on the provided seed colors, which are then interpolated to create a specified number of colors.
#' The palette can either be darkened or lightened based on user preference.
#'
#' @param seedColors A character vector of colors to use as the seed for the palette. Defaults to a pre-defined set of colors.
#' @param numColors The number of colors to generate in the palette. The default is set to the number of unique age groups in a dataset.
#' @param adjustAmount The amount by which to adjust the brightness or saturation of the colors.
#' @param darken A logical value indicating whether to darken (TRUE) or lighten (FALSE) the colors.
#'
#' @return A character vector representing the color palette.
#' @export
#' @examples
#' createOhdsiPalette()  # Default usage with pre-defined colors and darkening effect
#' createOhdsiPalette(seedColors = c("#FF0000", "#00FF00", "#0000FF"), numColors = 5, darken = FALSE)
createOhdsiPalette <-
  function(seedColors = c("#336B91", "#69AED5", "#11A08A", "#FBC511", "#EB6622"),
           numColors = 5,  # Set number of colors based on unique age groups
           adjustAmount = 0.1,
           darken = TRUE) {
    
    # Checkmate validations
    checkmate::assertCharacter(seedColors, min.len = 1, any.missing = FALSE, add = FALSE)
    checkmate::assertIntegerish(numColors, lower = 1, len = 1, add = FALSE)
    checkmate::assertNumeric(adjustAmount, lower = 0, finite = TRUE, add = FALSE)
    checkmate::assertLogical(darken, len = 1, add = FALSE)
    
    # Validate numColors to ensure it's a positive integer
    if (!is.numeric(numColors) || numColors <= 0 || numColors != round(numColors)) {
      stop("numColors must be a positive integer.")
    }
    
    # Validate each color in seedColors
    lapply(seedColors, validateColor)
    
    # Function to interpolate colors
    colorInterpolate <- grDevices::colorRampPalette(seedColors)
    
    # Generate the palette
    extendedPalette <- colorInterpolate(numColors)
    
    # Adjust the brightness or saturation based on user input
    if (darken) {
      adjustedPalette <-
        colorspace::darken(extendedPalette, amount = adjustAmount)
    } else {
      adjustedPalette <-
        colorspace::lighten(extendedPalette, amount = adjustAmount)
    }
    
    return(adjustedPalette)
  }

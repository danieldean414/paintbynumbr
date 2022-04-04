
#' Remap: Helper function to scale a numeric vector to a user-provided range of values.
#'
#' @param input Numeric vector
#' @param input_start Lowest value in `input` for scaling (can manually set another value, e.g. for consistency across multiple vectors)
#' @param input_end Highest value in `input` for scaling (can manually set another value, e.g. for consistency across multiple vectors)
#' @param output_start Lower bound of scaled target range
#' @param output_end Upper bound of scaled target range
#'
#'
#' @return A vector the length of the input vector that returns original
#'     values scaled to be between \code{output_start} and \code{output_end}.
#'
#'
#'
#'
#'
#' @examples
#' example_vector <- rnorm(n = 10)
#' remap(input = example_vector, output_start = 0, output_end = 1)
#'
remap <- function(input,
                  input_start = min(input),
                  input_end = max(input),
                  output_start = 0,
                  output_end = 1){
  slope = (output_end - output_start) / (input_end - input_start)
  output = output_start + slope * (input - input_start)
  return(output)
}

# at least 5

# apparently relevat for external data: system.file("extdata", "iris.csv", package = "readr")
# Use data as *internal* data (example)
  # data.R <- to document data

# At least 1 vignette

# Must pass all CRAN tests





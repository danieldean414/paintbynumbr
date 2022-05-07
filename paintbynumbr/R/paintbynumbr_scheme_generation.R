

#' color_scale_2d_rgb: Generate "2-Dimensional" Color Scales in RGB Color Space.
#'
#' @param paint_by_1 Vector (class numeric or factor-ideally ordered) containing values for the "X"
#'    axis of the color gradient. Is a red-to-cyan gradient by default.
#'
#' @param paint_by_2 Vector (class numeric or factor--ideally ordered) containing values for the "Y"
#'     axis of the color gradient. Is a yellow-to-blue gradient by default
#'
#' @return Returns a dataframe with the original input vectors, with an added column of hexidecimal
#'     color codes ("color_id") to be plugged in to ggplot2 or other downstream functions.
#'     (For now, also includes columns of the rescaled values used for the color gradients)
#'
#' @export
#' @importFrom magrittr %>%
#' @examples
#' ex_paint_by_1  <- rnorm(n = 100)
#' ex_paint_by_2 <- rnorm(n = 100, mean = 5)
#'  color_scale_2d_rgb(ex_paint_by_1, ex_paint_by_2)
color_scale_2d_rgb <- function(paint_by_1, paint_by_2){
  color_x <- remap(input = paint_by_1, output_start =  0, output_end = 1)
  color_y <- remap(input = paint_by_2, output_start =  0, output_end = 1)
  data_schemed <- data.frame(
    paint_by_1 = paint_by_1,
    paint_by_2 = paint_by_2,
    color_x = color_x,
    color_y = color_y
  )
  data_schemed <- data_schemed %>%
    dplyr::mutate(color_id = colorspace::hex(colorspace::mixcolor(
      alpha = remap(input = (sin(pi * color_x) - sin(pi * color_y)), output_start = 0, output_end = 1 ),
      color1 = colorspace::mixcolor(
        alpha = 1 - sin(color_x),
        color1 = colorspace::RGB(1,0,0),
        color2 = colorspace::RGB(0,1,1)
      ), color2 = colorspace::mixcolor(
        alpha = 1 - sin(color_y),
        color1 = colorspace::RGB(1,1,0),
        color2 = colorspace::RGB(0,0,1)
      )

    )))
  return(data_schemed$color_id)
}

# Look into `assert that~ (~) function
  # lets you check inputs to a function, and return context-specific warnings
  # also make help file explicit about expected inputs (maybe explain that values are lost over some level?)
    # Not sure about best way to address default ordering behavior (e.g. alphabetic?)
      # Check if an existing order is preserved here...


## Current issues I've caught with this:
    # Probably a more stable way to ~iterate within the 'level 1' blocks;
      # currently pushes over into unintuitive colors (hues) for more than a few 'inner' levels,
        #breaks if added angle >360
    # Throws warnings with ordinal variables "'-' is not meaningful for ordered factors "

  # Maybe could go back to keeping hue constant, and then just iterate within that hue angle
      # HSV seems like it might be more distinctive?
        #With HSV, low-saturation/value all kind of blurs into greyish-brown colors
        # With HSL, low-saturation/light

  # Or instead of mixing, would it make sense to generate an "outer" and "inner" layer?

#' color_family_gen: Create Nested Color Schemes with Factors or Numbers
#'
#' @param level_1 Vector of numeric values, factors, or character (will be converted to factors) corresponding to "outer" layer of categorization (e.g. genus in genus-species pairs, or state in city-state pairs). Sets larger hue angle increments.
#' @param level_2 Vector of numeric values, factors, or character (will be converted to factors) corresponding to "inner" layer of categorization (e.g. species in genus-species pairs, or city in a city-state relationship). Adjsuts color value and saturation, with small changes in hue. Must be of equal length to `level_1`
#' @param start_angle single numeric value (0-365); used to set color hue angle of minimum value in `level_1` input. Must be smaller than `stop_angle.`
#' @param stop_angle Single numeric value (0-365); used to set color hue angle of maximum value in `level_1` input. Must be greater than `start_angle.`
#' @param val_start Single numeric value (0-1.0): used to set value of darkest point; lower values are closer to black
#' @param  sat_start  Single numeric value (0-1.0): used to set value of least saturated point; lower values are closer to grey
#'
#' @return Returns a vector of hexidecimal color codes of equal length of input vectors.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' ex_level_1 <- rep(c("A","B","C","D"), 25)
#' ex_level_2 <- rnorm(n = 100)
#   color_family_gen(ex_level_1, ex_level_2)

color_family_gen <- function(level_1, level_2, start_angle = 0, stop_angle = 180, val_start = 0.4, sat_start = 0.5){
   data_schemed <- data.frame(
     color_x = level_1,
    color_y = level_2
   )
 data_schemed <- data_schemed %>%
    dplyr::mutate(color_x = dplyr::case_when(!is.numeric(color_x) ~ remap(as.numeric(as.factor(color_x)), output_start = 0, output_end = 1),
                              is.numeric(color_x) ~ remap(as.numeric(color_x), output_start = 0, output_end = 1)),
           color_y = dplyr::case_when(!is.numeric(color_y) ~ remap(as.numeric(as.factor(color_y)), output_start = 0, output_end = 1),
                              is.numeric(color_y) ~ remap(as.numeric(color_y), output_start = 0, output_end = 1))
    )
  data_schemed <- data_schemed %>%
    dplyr::mutate(color_id = colorspace::hex(colorspace::HSV(
      H = remap(input = as.numeric((color_x)),
                output_start = start_angle,
                output_end = stop_angle), #+
        #remap(input = as.numeric((color_y)),
         #     output_start = 0,
          #    output_end = stop_angle/max(color_x * 4)),
      #S = 1,
      S = 0.5 + (remap(color_y, output_start = 0, output_end = 1))*0.5,
      #V = 0.9 + (factor_1_id %% 2)/5 - 0.3 * abs(cos(factor_2_id))
      V = val_start + (remap(color_y, output_start = 0, output_end = 1))*(1-val_start)
    )
    )
    )
  return(data_schemed$color_id)
}


#' Generates a HSV-Scale Two-Dimensional Color Scale from Input Vectors
#'
#' @param paint_by_1 Vector of first scale for color mixing; class numeric or factor; will convert factor or character values to numeric
#' @param paint_by_2 Vector of second scale for color mixing; class numeric or factor; will convert factor or character values to numeric. Must be equal length as coloy
#' @param grad_1_1 "Starting point" of color for first gradient (corresponding to minimum value of color_x)
#' @param grad_1_2 "Ending point" of color for first gradient (corresponding to maximum value of color_x)
#' @param grad_2_1 "Starting point" of color for second gradient (corresponding to minimum value of color_y)
#' @param grad_2_2 "Ending point" of color for second gradient (corresponding to minimum value of color_y)
#'
#' @return Vector of color Hex codes (character) of equal length to paint_by_1/_2. ggplot2-compatible with argument "scale_color_identity()"
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' data(iris)
#' iris_color_id <- paint_by_2d_scale_hsv(paint_by_1 = iris$Sepal.Length,
#'    paint_by_2= iris$Sepal.Width)
#' head(iris_color_id)
#'

paint_by_2d_scale_hsv <- function(paint_by_1, paint_by_2,
                           grad_1_1 = 360, grad_1_2 = 180,
                           grad_2_1 = 60, grad_2_2 = 240){

  data_schemed <- data.frame(
    color_x = paint_by_1,
    color_y = paint_by_2
  ) %>%
    dplyr::mutate(color_x = dplyr::case_when(is.character(color_x) ~ as.numeric(as.factor(color_x)),
                               is.numeric(color_x) ~ as.numeric(color_x),
                               TRUE ~ as.numeric(as.factor(color_x))),
           color_y = dplyr::case_when(is.character(color_y) ~ as.numeric(as.factor(color_y)),
                               is.numeric(color_y) ~ as.numeric(color_y),
                               TRUE ~ as.numeric(as.factor(color_y)))
    ) %>%
  dplyr::mutate(
    color_x = remap(color_x, output_start = 0, output_end = 1),
    color_y = remap(color_y, output_start = 0, output_end = 1)
  ) %>%
  dplyr::mutate(color_id = colorspace::hex(colorspace::mixcolor(
    alpha = remap(sin(pi * color_x) - sin(pi * color_y), output_start = 0, output_end = 1),
    color1 = colorspace::mixcolor(
      alpha = 1 - sin(color_x),
      color1 = (colorspace::HSV(H = grad_1_1, S = 1, V =1)),
      color2 = (colorspace::HSV(H = grad_1_2, S = 1, V = 1)), where = "RGB"
    ),
    color2 = colorspace::mixcolor(
      alpha = 1 - sin(color_y),
      color1 = (colorspace::HSV(H = grad_2_1, S = 1, V = 1)),
      color2 = (colorspace::HSV(H = grad_2_2, S = 1, V = 1)), where = "RGB"
    ), where = "RGB"
  )))
  return(data_schemed$color_id)
}


########################################3333

# Corresponding "key/legend" fuctions

#' color_scale_2d_rgb_legend: Legend for Generated "2-Dimensional" Color Scales in RGB Color Space.
#'
#' @param paint_by_1 Vector (class numeric or factor-ideally ordered) containing values for the "X"
#'    axis of the color gradient. Is a red-to-cyan gradient by default.
#'
#' @param paint_by_2 Vector (class numeric or factor--ideally ordered) containing values for the "Y"
#'     axis of the color gradient. It is a yellow-to-blue gradient by default
#'
#' @param resolution Integer to determine number of entries on x and y axis of the color "legend" grid. Default is 20x20
#'
#' @return Returns a dataframe with a square grid of user-specified resolution mapped to the original input vectors,
#'    with an added column of hexidecimal color codes ("color_id") to be plugged in to ggplot2
#'    or other downstream functions using the "scale_<color/fill>_identity()" argument.
#'    For now, x and y labels need to be provided manually during plotting.
#'
#'
#'
#' @export
#' @importFrom magrittr %>%
#' @examples
#' ex_paint_by_1  <- rnorm(n = 100)
#' ex_paint_by_2 <- rnorm(n = 100, mean = 5)
#'  color_scale_2d_rgb_legend(ex_paint_by_1, ex_paint_by_2)
color_scale_2d_rgb_legend <- function(paint_by_1, paint_by_2, resolution = 20){

  key_df <- expand.grid(scale_x = 1:resolution, scale_y = 1:resolution) %>%
    dplyr::mutate(color_id = color_scale_2d_rgb(scale_x, scale_y),
                  labels_x = remap(scale_x, output_start = min(paint_by_1), output_end = max(paint_by_1)),
                  labels_y = remap(scale_y, output_start = min(paint_by_2), output_end = max(paint_by_2))
                  ) %>%
    dplyr::select(-scale_x, -scale_y)


  return(key_df)
}



#' Generates a Legend for HSV-Scale Two-Dimensional Color Scale from Input Vectors
#'
#' @param paint_by_1 Vector of first scale for color mixing; class numeric or factor; will convert factor or character values to numeric
#' @param paint_by_2 Vector of second scale for color mixing; class numeric or factor; will convert factor or character values to numeric. Must be equal length as coloy
#' @param grad_1_1 "Starting point" of color for first gradient (corresponding to minimum value of color_x)
#' @param grad_1_2 "Ending point" of color for first gradient (corresponding to maximum value of color_x)
#' @param grad_2_1 "Starting point" of color for second gradient (corresponding to minimum value of color_y)
#' @param grad_2_2 "Ending point" of color for second gradient (corresponding to minimum value of color_y)
#' @param resolution Integer to determine number of entries on x and y axis of the color "legend" grid. Default is 20x20
#'
#'
#' @return Data frame describing a square grid (with user-defined resolution --20x20 by default) with hexidecimal color identifies
#' showing the full "spectrum" of the color_scale_2d_rgb mapping function with specified inputs.
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' data(iris)
#' iris_color_id <- paint_by_2d_scale_hsv(paint_by_1 = iris$Sepal.Length,
#'    paint_by_2= iris$Sepal.Width)
#' head(iris_color_id)
#'

paint_by_2d_scale_hsv_legend <- function(paint_by_1, paint_by_2,
                                  grad_1_1 = 360, grad_1_2 = 180,
                                  grad_2_1 = 60, grad_2_2 = 240,
                                  resolution = 20){

  key_df <- expand.grid(scale_x = 1:resolution, scale_y = 1:resolution) %>%
    dplyr::mutate(color_id = paint_by_2d_scale_hsv(scale_x, scale_y, grad_1_1, grad_1_2, grad_2_1, grad_2_2),
                  labels_x = remap(scale_x, output_start = min(paint_by_1), output_end = max(paint_by_1)),
                  labels_y = remap(scale_y, output_start = min(paint_by_2), output_end = max(paint_by_2))
    ) %>%
    dplyr::select(-scale_x, -scale_y)


  return(key_df)
}




#' color_family_gen: Create Nested Color Schemes with Factors or Numbers
#'
#' @param level_1 Vector of numeric values, factors, or character (will be converted to factors) corresponding to "outer" layer of categorization (e.g. genus in genus-species pairs, or state in city-state pairs). Sets larger hue angle increments.
#' @param level_2 Vector of numeric values, factors, or character (will be converted to factors) corresponding to "inner" layer of categorization (e.g. species in genus-species pairs, or city in a city-state relationship). Adjsuts color value and saturation, with small changes in hue. Must be of equal length to `level_1`
#' @param start_angle single numeric value (0-365); used to set color hue angle of minimum value in `level_1` input. Must be smaller than `stop_angle.`
#' @param stop_angle Single numeric value (0-365); used to set color hue angle of maximum value in `level_1` input. Must be greater than `start_angle.`
#' @param resolution Integer to determine number of entries on x and y axis of the color "legend" grid. Default is 20x20
#'
#' @return Returns a vector of hexidecimal color codes of equal length of input vectors.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' ex_level_1 <- rep(c("A","B","C","D"), 25)
#' ex_level_2 <- rnorm(n = 100)
#   color_family_gen(ex_level_1, ex_level_2)

color_family_gen_legend <- function(level_1, level_2, start_angle = 0, stop_angle = 180, resolution = 20){
  if(is.numeric(level_1) & is.numeric(level_2)){
  key_df <- expand.grid(scale_x = 1:resolution, scale_y = 1:resolution) %>%
    dplyr::mutate(color_id = color_family_gen(scale_x, scale_y, start_angle, stop_angle),
                  labels_x = remap(scale_x, output_start = min(level_1), output_end = max(level_1)),
                  labels_y = remap(scale_y, output_start = min(level_2), output_end = max(level_2))
    ) %>%
    dplyr::select(-scale_x, -scale_y)}
  else if(!is.numeric(level_1) & is.numeric(level_2)){
      key_df <- expand.grid(scale_x = unique(as.factor(level_1)), scale_y = 1:resolution) %>%
        dplyr::mutate(color_id = color_family_gen(scale_x, scale_y, start_angle, stop_angle),
                      labels_x = scale_x,
                      labels_y = remap(scale_y, output_start = min(level_2), output_end = max(level_2))
        )
    }
  else if(!is.numeric(level_2) & is.numeric(level_1)){
      key_df <- expand.grid(scale_y = unique(as.factor(level_2)), scale_x = 1:resolution) %>%
        dplyr::mutate(color_id = color_family_gen(scale_x, scale_y, start_angle, stop_angle),
                      labels_y = scale_y,
                      labels_x = remap(scale_x, output_start = min(level_1), output_end = max(level_1))
        )}
  else {
          key_df <- expand.grid(scale_y = unique(as.factor(level_2)), scale_x = unique(as.factor(level_1))) %>%
            dplyr::mutate(color_id = color_family_gen(scale_x, scale_y, start_angle, stop_angle),
                          labels_y = scale_y,
                          labels_x = scale_x
            )}


  return(key_df)
}


# change 'color_1' to e.g. 'paint_by_1'

#' Title
#'
#' @param color_1 Vector (class numeric or factor-ideally ordered) containing values for the "X"
#'    axis of the color gradient. Is a red-to-cyan gradient by default.
#'
#' @param color_2 Vector (class numeric or factor--ideally ordered) containing values for the "Y"
#'     axis of the color gradient. Is a yellow-to-blue gradient by default
#'
#' @return Returns a dataframe with the original input vectors, with an added column of hexidecimal
#'     color codes ("color_id") to be plugged in to ggplot2 or other downstream functions.
#'     (For now, also includes columns of the rescaled values used for the color gradients)
#'
#' @export
#' @examples
test_scheme_RGB <- function(color_1, color_2){
  color_x <- remap(input = color_1, output_start =  0, output_end = 1)
  color_y <- remap(input = color_2, output_start =  0, output_end = 1)
  data_schemed <- data.frame(
    color_1 = color_1,
    color_2 = color_2,
    color_x = color_x,
    color_y = color_y
  )
  data_schemed <- data_schemed %>%
    mutate(color_id = colorspace::hex(colorspace::mixcolor(
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
  return(data_schemed)
}

# Look into `assert that~ (~) function
  # lets you check inputs to a function, and return context-specific warnings
  # also make help file explicit about expected inputs (maybe explain that values are lost over some level?)
    # Not sure about best way to address default ordering behavior (e.g. alphabetic?)
      # Check if an existing order is preserved here...

#' Title
#'
#' @param level_1
#' @param level_2
#' @param start_angle
#' @param stop_angle
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
color_family_gen <- function(level_1, level_2, start_angle = 0, stop_angle = 180){
  data_schemed <- data.frame(
    color_x = level_1,
    color_y = level_2
  )
  data_schemed <- data_schemed %>%
    mutate(color_x = case_when(!is.numeric(color_x) ~ remap(as.numeric(as.factor(color_x)), output_start = 0, output_end = 1),
                               is.numeric(color_x) ~ remap(color_x, output_start = 0, output_end = 1)),
           color_y = case_when(!is.numeric(color_y) ~ remap(as.numeric(as.factor(color_y)), output_start = 0, output_end = 1),
                               is.numeric(color_y) ~ remap(color_y, output_start = 0, output_end = 1))
    )
  data_schemed <- data_schemed %>%
    mutate(color_id = colorspace::hex(colorspace::HSV(
      H = remap(input = as.numeric((color_x)),
                output_start = start_angle,
                output_end = stop_angle) +
        remap(input = as.numeric((color_y)),
              output_start = 0,
              output_end = stop_angle/max(color_x)),
      #S = 1,
      S = 0.3 + (remap(color_x, output_start = 0, output_end = 1))*0.7,
      #V = 0.9 + (factor_1_id %% 2)/5 - 0.3 * abs(cos(factor_2_id))
      V = 0.3 + (remap(color_y, output_start = 0, output_end = 1))*0.7
    )
    )
    )
  return(data_schemed$color_id)
}
  # OK, changing it to return just the color ID vector (seems more useful inside a dataframe)
    # Maybe see if there's a way to change hue/saturation?
      # digging into colorspace more, there are also built-in tools to e.g. lighten/darken or desaturate
# Curious if I could stick with HSV for 2(+)D approach as well
  # e.g. primaries all have a defined hue angle, I think
  # might be interesting to see if I can make it (optionally) extend to 3 axes; not sure of more would be readable
  # Also, takes vectors as inputs, so for now, could

color_2d_scale <- function(color_x, color_y,
                           grad_1_1 = 360, grad_1_2 = 180,
                           grad_2_1 = 60, grad_2_2 = 240){

  data_schemed <- data.frame(
    color_x = color_x,
    color_y = color_y
  ) %>%
    dplyr::mutate(color_x = case_when(is.character(color_x) ~ as.numeric(as.factor(color_x)),
                               is.numeric(color_x) ~ color_x),
           color_y = case_when(is.character(color_y) ~ as.numeric(as.factor(color_y)),
                               is.numeric(color_y) ~ color_y)
    ) %>%
  mutate(
    color_x = remap(color_x, output_start = 0, output_end = 1),
    color_y = remap(color_y, output_start = 0, output_end = 1)
  ) %>%
  mutate(color_id = hex(mixcolor(
    alpha = remap(sin(pi * color_x) - sin(pi * color_y), output_start = 0, output_end = 1),
    color1 = mixcolor(
      alpha = 1 - sin(color_x),
      color1 = (colorspace::HSV(H = grad_1_1, S = 1, V =1)),
      color2 = (colorspace::HSV(H = grad_1_2, S = 1, V = 1))
    ),
    color2 = mixcolor(
      alpha = 1 - sin(color_y),
      color1 = (colorspace::HSV(H = grad_2_1, S = 1, V = 1)),
      color2 = (colorspace::HSV(H = grad_2_2, S = 1, V = 1))
    )
  )))
  return(data_schemed$color_id)
}

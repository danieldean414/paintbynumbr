library(dplyr)
library(ggplot2)
library(colorspace)
library(tibble)
library(tidyr)

#range01 = function(x) {
#  if(length(x) > 1) {(x-min(x))/(max(x)-min(x))}
#  else {return(1)}
#}


#' Remap
#'
#' @param input numeric vector to be "rescaled" to a new trget range
#' @param input_start Lowest value for input vector; minimum by default, but can be set by default e.g. for unfiformity
#' @param input_end
#' @param output_start
#' @param output_end
#'
#' @return
#' @export
#'
#' @examples
#' example_vector <- rnorm(n = 10)
#' remap(input = example_vector, output_start = 0, output_end = 1)
#'
remap <- function(input, input_start = min(input),
                  input_end = max(input),
                  output_start,
                  output_end){
  slope = (output_end - output_start) / (input_end - input_start)
  output = output_start + slope * (input - input_start)
  return(output)
}


test_scheme_RGB <- function(color_1, color_2){
  color_x <- remap(input = color_1, output_start =  0, output_end = 1)
  color_y <- remap(input = color_2, output_start =  0, output_end = 1)
  data_schemed <- data.frame(
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


color_family_gen <- function(level_1, level_2, start_angle = 0, stop_angle = 180){
  color_x <- remap(input = level_1, output_start =  0, output_end = 1)
  color_y <- remap(input = level_2, output_start =  0, output_end = 1)
  data_schemed <- data.frame(
    color_x = color_x,
    color_y = color_y
  )
  data_schemed <- data_schemed %>%
    mutate(color_x = case_when(is.character(color_x) ~ as.numeric(as.factor(color_x)),
                        is.numeric(color_x) ~ color_x),
           color_y = case_when(is.character(color_y) ~ as.numeric(as.factor(color_y)),
                               is.numeric(color_y) ~ color_y)
           )
  data_schemed <- data_schemed %>%
    mutate(color_id = colorspace::hex(colorspace::HSV(
      #H = range01(as.numeric(as.factor(col_e)))*360,
      H = remap(input = as.numeric((color_x)),
                output_start = start_angle,
                output_end = stop_angle) +
        remap(input = as.numeric((color_y)),
              output_start = 0,
              output_end = stop_angle/max(color_x)),
      #S = 1,
      S = 0.2 + (range01(color_x))*0.8,
      #V = 0.9 + (factor_1_id %% 2)/5 - 0.3 * abs(cos(factor_2_id))
      V = 0.3 + (range01(color_y))*0.7
    )
    )
    )
  return(data_schemed)
}


# Keeping 'default' colors for now, but ideally make user-selectable
  # further, would have a final function that accomodates different 'models' automatically


count <- 200

test_df <- data.frame(
  index = seq(1:count),
  col_a = rnorm(n = count, mean = 1, sd = 10),
  col_b = rnorm(n = count, mean = 1),
                col_c = rnorm(n = count, mean = 1, sd = 10),
                col_d = rnorm(n = count, mean = 1, sd = 100),
  col_e = rep(c("cat", "dog", "shark", "hippo", "bird"), count/5),
  col_f = rep(c("big", "small", "medium", "large"), count/4)
)

test_df_colors <- test_df %>%
  mutate(col_a_scaled = range01(col_a),
         col_b_scaled = range01(col_b)) %>%
  mutate(color_id = hex(mixcolor(
    #alpha = 0.5,
    alpha = range01(sin(pi * col_a_scaled) - sin(pi * col_b_scaled)),
    #alpha = range01(col_a_scaled - col_b_scaled),
    color1 = mixcolor(
      alpha = 1 - sin(col_a_scaled),
      color1 = RGB(1,1,0),
      color2 = RGB(0,0,1)
    ),
    color2 = mixcolor(
      alpha = 1 - sin(col_b_scaled),
      color1 = RGB(1,0,0),
      color2 = RGB(0,1,1)
    )
  )
  )
  )


# trying rudimentary function:

test_df_function_test <- test_scheme_RGB(color_1 = test_df$col_a, color_2 = test_df$col_b)
test_df_function_family_test <- color_family_gen(level_1 = test_df$col_a, level_2 = test_df$col_b)

test_df_function_test %>%
  ggplot() +
  aes(
    x = color_x,
    y = color_y,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE) +
  scale_color_identity() +
  theme_classic()

test_df_function_family_test %>%
  ggplot() +
  aes(
    x = color_x,
    y = color_y,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE) +
  scale_color_identity() +
  theme_classic()

##############33

test_df_colors %>%
  ggplot() +
  aes(
    x = col_a,
    y = col_b,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE) +
  scale_color_identity() +
  theme_classic()



test_df_colors %>%
  ggplot() +
  aes(
    x = col_a,
    y = col_b,
    col = paste(color_id)
  ) +
  facet_wrap(. ~ col_e * col_f) +
  geom_point(show.legend = FALSE) +
  scale_color_identity() +
  theme_classic()



test_df_colors %>%
  ggplot() +
  aes(
    x = col_c,
    y = col_d,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE) +
  scale_color_identity() +
  theme_classic()


##### trying factor-based colors
  # removing conditional steps accounting addressing 'other' levels for now....


test_df_colors_factor <- test_df %>%
  mutate(factor_1_id = as.numeric(as.factor(col_e)),
         factor_2_id = as.numeric(as.factor(col_f))) %>%
  mutate(color_id = hex(HSV(
      H = ((factor_1_id) * 360/max(factor_1_id) + factor_2_id * 10) + 0,
      #S = 1,
      S = 0.8 + (range01(factor_2_id))*0.2,
      #V = 0.9 + (factor_1_id %% 2)/5 - 0.3 * abs(cos(factor_2_id))
      V = 0.5 + (range01(factor_2_id))*0.5
    )
  )
  )

  # wow, this was pretty ad hoc; any way to procedurally get ideal angles?
    # Oh, I think I'm remembering I tried this route;
      # ended up coming down to difference between geometric, ~discernible colors (e.g. 10 degrees from red to yellow a lot bigger than in the greens)





test_df_colors_factor %>%
  ggplot() +
  aes(
    x = col_e,
    y = col_f,
    fill = paste(color_id)
  ) +
  geom_tile(show.legend = FALSE) +
  scale_fill_identity() +
  theme_classic()

test_df_colors_factor %>%
  ggplot() +
  aes(
    x = col_c,
    y = col_d,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_color_identity() +
  theme_classic()


# Does this work for mixed cases?



test_df_colors_both <- test_df %>%
  mutate(color_id = hex(HSV(
    #H = range01(as.numeric(as.factor(col_e)))*360,
    H = remap(input = as.numeric(as.factor(col_e)), output_start = 30, output_end = 270) +
      remap(input = as.numeric(as.factor(col_b)), output_start = 0, output_end = 25),
    #S = 1,
    S = 0.2 + (range01(col_b))*0.8,
    #V = 0.9 + (factor_1_id %% 2)/5 - 0.3 * abs(cos(factor_2_id))
    V = 0.3 + (range01(col_b))*0.7
  )
  )
  )

test_df_colors_both %>%
  ggplot() +
  aes(
    x = col_a,
    y = col_b,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE) +
  scale_color_identity() +
  facet_wrap(. ~ col_e) +
  theme_classic()

test_df_colors_both %>%
  ggplot() +
  aes(
    x = col_e,
    y = col_f,
    fill = paste(color_id)
  ) +
  geom_tile(show.legend = FALSE) +
  scale_fill_identity() +
  theme_classic()

# Any way to map a range onto a ~stop/start 'key'? (e.g. start = 90, stop = 250, 0 = 90)

  # added `remap` function above

# oh, also remember that `colorspace` has more dedicated functions:
data.frame(count = 1:10, colors = colorspace::qualitative_hcl(n = 10, h = 30,  h2 = 220, c1 = 100)) %>% ggplot() + aes(x = count,y = count, fill = paste(colors)) + geom_tile() + scale_fill_identity()



# Trying three 'scales' (CMY?)

test_df %>%
  mutate(
  color_id = hex(mixcolor(

    color1=(

    alpha = 1 - sin(remap(input = col_a,  output_start = 0, output_end = 1)),
    color1 = RGB(1,1,0),
    color2 = RGB(0,0,1)
  )
  )
  )
    )

hex(mixcolor(alpha = 0.5, color1 = RGB(1,1,0), color2 = RGB(0,1,1)))
hex(mixcolor(alpha = 0.5, color1 = RGB(1,1,0), color2 = RGB(1,0,1)))

data.frame(
  count = 1:count,
)
hex(mixcolor(alpha = 0.5, color1 = RGB(1,1,0), color2 = RGB(1,0,1)))

# OK, here's the taxonomic part
  # First (maybe optional): clustering into top *n* + 'other'; not sure if that would be worth integrating



phylo_obj_ely_16s_net_colors_top_classes_both <- phylo_obj_ely_16s_net_both %>%
  tax_table() %>%
  as.data.frame() %>%
  rownames_to_column('otu') %>%
  dplyr::select(phylum, class, otu) %>%
  mutate(phylum = fct_explicit_na((phylum), na_level = "p__Unidentified"),
         class = fct_explicit_na((class), na_level = "c__Unidentified")) %>%
  mutate(phylum = str_replace(phylum, "p__", ""),
         class = str_replace(class, "c__", "")) %>%
  mutate(phylum_top = fct_lump(f = factor(phylum),
                               n = 6),
         class_top = fct_lump(f = factor(class),
                              n = 10)
  )



phylo_obj_ely_16s_net_colors_top_classes_both <- phylo_obj_ely_16s_net_both %>%
  tax_table() %>%
  as.data.frame() %>%
  rownames_to_column('otu') %>%
  dplyr::select(phylum, class, otu) %>%
  mutate(phylum = fct_explicit_na((phylum), na_level = "p__Unidentified"),
         class = fct_explicit_na((class), na_level = "c__Unidentified")) %>%
  mutate(phylum = str_replace(phylum, "p__", ""),
         class = str_replace(class, "c__", "")) %>%
  mutate(phylum_top = fct_lump(f = factor(phylum),
                               n = 6),
         class_top = fct_lump(f = factor(class),
                              n = 17)
  )  %>%
  mutate(phylum_top_id = as.numeric(as.factor(phylum_top))) %>%
  arrange(phylum) %>%
  group_by(class_top) %>%
  nest() %>%
  rownames_to_column('class_top_id') %>%
  mutate(class_top_id = as.numeric(class_top_id) - 1) %>%
  unnest() %>%
  group_by(phylum) %>%
  mutate(color_id = case_when(
    (class_top == "Other") ~ "#FFFFFF",
    !(class_top == "Other") ~ hex(HSV(
      H = ((phylum_top_id) * 30 + class_top_id * 10) + 60,
      S = 1,
      V = 0.7 + (phylum_top_id %% 2)/5 - 0.3 * abs(cos(class_top_id))
    ))
  )
  )

library(dplyr)
library(ggplot2)
library(colorspace)

range01 <- function(x) {
  if (length(x) > 1) {
    (x - min(x)) / (max(x) - min(x))
  } else {
    return(1)
  }
}

count <- 1000

test_df <- data.frame(
  index = seq(1:count),
  col_a = rnorm(n = count, mean = 1, sd = 10),
  col_b = rnorm(n = count, mean = 1),
  col_c = rnorm(n = count, mean = 1, sd = 10),
  col_d = rnorm(n = count, mean = 1)
)

test_df_colors <- test_df %>%
  mutate(
    col_a_scaled = range01(col_a),
    col_b_scaled = range01(col_b)
  ) %>%
  mutate(color_id = hex(mixcolor(
    alpha = range01(sin(pi * col_a_scaled) - sin(pi * col_b_scaled)),
    #alpha = range01(col_a_scaled - col_b_scaled),
    color1 = mixcolor(
      alpha = 1 - sin(col_a_scaled),
      color1 = RGB(1, 0, 0),
      color2 = RGB(0, 0, 0)
    ),
    color2 = mixcolor(
      alpha = 1 - sin(col_b_scaled),
      color1 = RGB(0, 0, 1),
      color2 = RGB(0, 1, 1)
    )
  )))



test_df_colors %>%
  ggplot() +
  aes(
    x = col_a,
    y = col_b,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE) +
  scale_fill_identity() +
  theme_classic()



test_df_colors %>%
  ggplot() +
  aes(
    x = col_c,
    y = col_d,
    col = paste(color_id)
  ) +
  geom_point(show.legend = FALSE) +
  scale_fill_identity() +
  theme_classic()

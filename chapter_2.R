install.packages(c("devtools", "roxygen2", "testthat", "knitr"))


library(devtools)
#> Loading required package: usethis
  # huh--looks like they might have implemented code blocks as '#>'--autoformats to comment when I hit enter


packageVersion("devtools")
#> [1] '2.4.3'

#adding
#dir.create("./02_code/ch2/")
#dir.create("./02_code/ch2/regexcite")
dir.create("C:/Users/Admin/Documents/erhs_s_2022/regexcite")

create_package("~Admin/Documents/erhs_s_2022/regexcite")

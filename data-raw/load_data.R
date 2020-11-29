## code to prepare `load_data` dataset goes here

# usethis::use_data(load_data, overwrite = TRUE)

getwd()

data1 <- readRDS(file = './data-raw/long_1128_1.RDS')
data2 <- readRDS(file = './data-raw/long_1128_2.RDS')

library(tibble)
data1 <- as_tibble(x=data1)
data2 <- as_tibble(x=data2)

## code to prepare `lm_patho` dataset goes here
lm_patho <- read.csv("lm_patho.csv")
dir.create("../data")
save(lm_patho, file = "../data/lm_patho.rda")

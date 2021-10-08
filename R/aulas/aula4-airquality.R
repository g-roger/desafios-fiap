set.seed(123)
sample(airquality, 10, )

dplyr::sample_frac(airquality, 0.065)

airquality[sample(1:nrow(airquality), 10),]


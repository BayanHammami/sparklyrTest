#install.packages('sparklyr')
#spark_install(version = "2.1.0")
#install.packages(c("nycflights13", "Lahman"))
library(sparklyr)
library(dplyr)
library(nycflights13)
library(ggplot2)

sc <- spark_connect(master = "local", version = "2.1.0")

mtcars_tbl <- copy_to(sc, mtcars, "mtcars")
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  sdf_mutate(cyl8 = ft_bucketizer(cyl, c(0,8,12))) %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 888)

# fit a linear mdoel to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(mpg ~ wt + cyl)

summary(fit)

pred <- sdf_predict(fit, partitions$test) %>%
  collect

ggplot(pred, aes(x = mpg, y = prediction)) +
  geom_abline(lty = "dashed", col = "red") +
  geom_point() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_fixed(ratio = 1) +
  labs(
    x = "Actual Fuel Consumption",
    y = "Predicted Fuel Consumption",
    title = "Predicted vs. Actual Fuel Consumption"
  )



library(here)
dane <- read.csv("US_macroeconomics.csv")


library(tidyverse)
summary(dane)
dane <- dane |>
	mutate(date = ymd(date))

test <- dane |>
	filter(date > ymd("2014-02-01"))

train <- dane |>
	filter(date <= ymd("2014-02-01"))

acf(train$personal_savings)
pacf(train$personal_savings)

p1 <- ggplot(train, aes(x = personal_savings, y = CPI)) +
	geom_point(alpha = 0.3) +
	theme_minimal()


p2 <- ggplot(train, aes(x = personal_savings, y = NASDAQ)) +
	geom_point(alpha = 0.3) +
	theme_minimal()

p3 <- ggplot(train, aes(x = personal_savings, y = disposable_income)) +
	geom_point(alpha = 0.3) +
	theme_minimal()

p4 <- ggplot(train, aes(x = personal_savings, y = Unemp_rate)) +
	geom_point(alpha = 0.3) +
	theme_minimal()

library(corrplot)
train |>
	select(-date) |>
	cor() |>
	corrplot()
library()
?plot_grid
plot_grid(p1, p2, p3, p4, nrow = 2)


temp <- train |>
	select(-c(date, personal_savings)) |>
	cor()
class(temp)
print(temp>0.7)

corr_matrix <- train |>
	select(-c(date, personal_savings)) |>
	cor()

pary_kor <- corr_matrix |>
	as.data.frame() |>
	mutate(var1 = rownames(corr_matrix)) |>
	pivot_longer(cols = -var1, names_to = "var2", values_to = "corr") |>
	filter(var1 < var2, abs(corr) > 0.7) |>
	arrange(desc(abs(corr)))

print(pary_kor)

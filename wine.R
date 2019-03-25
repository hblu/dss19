library(tidyverse)
library(rsample)
library(ggplot2)
library(corrplot)
library(broom)

red <- read_delim("paper/winequality/winequality-red.csv",delim = ";")
white <- read_delim("paper/winequality/winequality-white.csv", delim = ";")

white %>% 
  ggplot(aes(x = quality)) + geom_histogram(binwidth = 1)

cor(white) %>% 
  corrplot()


boots <- white %>% 
  bootstraps()

boots$splits[[3]] %>% assessment()


fit_nls_on_bootstrap <- function(split) {
  lm(quality ~ ., analysis(split))
}

boot_models <- boots %>% 
  mutate(
    model = splits %>% map(~lm(quality~., .)),
    coef = model %>% map(tidy)
    )

boot_models %>% 
  unnest(coef) %>% 
  group_by(term) %>% 
  summarise_if(is.numeric, funs(mean,sd)) %>% 
  View


white_split <- white %>% 
  initial_split()

train <- training(white_split)
test <- testing(white_split)

fold <- vfold_cv(white, v = 10, repeats = 20)

m <- fold %>% 
  mutate(
    model = splits %>% map(analysis) %>% map(~lm(quality~., .)),
    coef = model %>% map(tidy),
    aug = map2(model, splits %>% map(assessment), ~augment(.x, newdata = .y))
  )

m %>% 
  unnest(aug) %>% 
  group_by(id, id2) %>% 
  summarise(
    r2 = sum((quality - .fitted)^2)
  ) %>% 
  summarise(
    r2 = mean(r2)
  )


tibble(
  a = 1:10,
  b = seq(1e-3, 1e-2, length = 10)
) %>% 
  cross_df()

library(tidyverse)
library(MASS)
library(broom)

data("Boston")

Boston <- Boston %>% as_tibble()

Boston %>% glimpse()

vars <- Boston %>% colnames() %>% setdiff(c("crim", "chas"))

cross_df(list(
  var = vars,
  order = 1:3
)) %>% 
  mutate(
    formula = map2_chr(var, order, ~str_c("crim ~ poly(", .x, ",", .y, ")")), 
    model = formula %>% map(~(lm(., Boston))),
    glance = model %>% map(glance)
  ) %>% 
  unnest(glance) %>% 
  group_by(var) %>% 
  filter(BIC == min(BIC)) 

ggplot(Boston, aes(lstat, crim)) + geom_point()

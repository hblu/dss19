library(tidyverse)
library(broom)
library(glmnet)
library(nnet)
library(ROCR)

#读取数据
train <- read_csv("data/mnist_train.csv", col_names = FALSE)
test <- read_csv("data/mnist_test.csv", col_names = FALSE)

#可视化
pixels_gathered <- train %>% 
  rename(label = X1) %>% 
  sample_n(1000) %>% 
  mutate(id = row_number()) %>%
  gather(pixel, value, -label, -id) %>%
  extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 2,
         x = pixel %% 28,
         y = 28 - pixel %/% 28)


pixels_gathered %>%
  filter(id <= 10) %>%
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ label + id)

######
#PCA
#####

#降维:提取由前 k 个主成分构成的矩阵 X
component <- function(s, k){
  s$u[,1:k]%*%diag(s$d[1:k])%*%t(s$v[,1:k])
}

#奇异值分解
train_sample <- train %>% sample_n(1000)

pc <- train_sample %>% 
  select(-X1) %>% 
  mutate_all(
    ~(.-mean(.))/(sd(.)+1e-10) ## 加上1e-10防止除0操作
  ) %>% 
  svd()


pc$d %>% .^2 %>% {cumsum(.)/sum(.)}

#取前80个主成分得到的降维后图像
component(pc, 80) %>% 
  as.tibble() %>% 
  mutate(
    id = row_number(),
    label = train_sample$X1
    ) %>%
  filter(id <= 10) %>%
  gather(pixel, value,  -id, -label) %>%
  extract(pixel, "pixel", "(\\d+)", convert = TRUE) %>%
  mutate(pixel = pixel - 1,
         x = pixel %% 28,
         y = 28 - pixel %/% 28) %>% 
  ggplot(aes(x, y, fill = value)) +
  geom_tile() +
  facet_wrap(~ id + label)

########
# 用 logist 回归判断一副图片是否是0

data0 <- train_sample %>% 
  select(-X1) %>% 
  as.matrix() %>% 
  {. %*% pc$v[,1:80]} %>% #取前80个主方向
  as_tibble() %>% 
  mutate(
    y = train_sample$X1 == 0
  )

model <- glm(y~., data = data0, family = "binomial")

test0 <- test %>%  #创建测试数据
  select(-X1) %>% 
  as.matrix() %>% 
  {. %*% pc$v[,1:80]} %>% 
  as_tibble() %>% 
  mutate(
    y = test$X1 == 0
)

pred <- augment(model, newdata = test0) %>% 
  mutate(
    p = .fitted %>% {exp(.)/(1+exp(.))}
  ) %>% 
  select(p, y)

#ROC
prediction(pred$p, pred$y) %>% 
  performance("tpr", "fpr") %>% 
  plot()

#
pred %>%
  group_by(y) %>% 
  summarise(mean(p))

table(pred$y, pred$p >= 0.5)

# 练习: 
# 1. 尝试使用其他方法对问题"判断一张图片是否为0 (或其他数字)"进行建模,
# 选出最好的模型, 给出其在测试集 test 上的正确率.
# 
# 2. 尝试使用多类别分类对问题"判断一张图片上的数字到底是几"进行建模,
# 选出最好的模型, 给出其在测试集 test 上的正确率.
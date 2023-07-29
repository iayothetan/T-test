library(tidyverse)
library(car)

# Создаем датасет
set.seed(42)
df <- data.frame(
  var = rep(c('A', 'B'), each = 500),  # 50 значений A, затем 50 значений B
  money = runif(1000, min = 1, max = 30)  # 100 случайных чисел от 1 до 30
)

# Делаем его более "живым", накидываем выбросы и пропуски
df$money[c(10, 20, 30, 40, 50)] <- c(100, 110, 120, 130, 140)
df$money[sample(1:1000, 10)] <- NA

glimpse(df)
df <- df %>% mutate(var = as.factor(var))


# Теперь чистим
# Определяем выбросы
outliers_money <- boxplot.stats(df$money)$out

# Заменим выбросы, например, медианами
median_money <- median(df$money, na.rm = TRUE)
df$money[df$money %in% outliers_money] <- median_money

# Чистим пропуски, опять же, заменяем медианой
sum(is.na(df$money))
df$money[is.na(df$money)] <- median_money


# Переходим к тесту
# 1. Проверяем гипотезу о равенстве дисперсий
# Тест Левена на равенство дисперсий при нормальности распределения данных
leveneTest(money ~ var, df)

# Тест Флигнера-Килина при выбросах и сильном отклонении от нормальности
fligner.test(money ~ var, df)


# 2. Проверка нормальности распределения средних
set.seed(123)
generate_sample_means <- function(data, sample_size, n_samples) {
  sample_means <- numeric(n_samples)
  
  for (i in 1:n_samples) {
    sample <- sample(data, size = sample_size, replace = TRUE)
    sample_means[i] <- mean(sample)
  }
  
  return(sample_means)
}

# Генерируем 1000 выборочных средних для каждой группы
sample_size <- 50
n_samples <- 1000

group_a_sample_means <- generate_sample_means(df$money[df$var == "A"], sample_size, n_samples)
group_b_sample_means <- generate_sample_means(df$money[df$var == "B"], sample_size, n_samples)

# Проверяем выборочные средние на нормальность с помощью теста Шапиро-Уилка
shapiro.test(group_a_sample_means)
shapiro.test(group_b_sample_means)

# Графики фактического и теоритического нормального распределения
par(mfrow=c(1, 2))

hist(group_a_sample_means, main = "Group A", xlab = "Sample means", col = "cornflowerblue", border = "black", freq = FALSE)
curve(dnorm(x, mean = mean(group_a_sample_means), sd = sd(group_a_sample_means)), add = TRUE, col = "black", lwd = 2)

hist(group_b_sample_means, main = "Group B", xlab = "Sample means", col = "brown1", border = "black", freq = FALSE)
curve(dnorm(x, mean = mean(group_b_sample_means), sd = sd(group_b_sample_means)), add = TRUE, col = "black", lwd = 2)


# Выбираем t-тест
# var.equal можно пропустить, тогда R выберет использовать ли поправку, самостоятельно
# Расчет t-теста Стьюдента
t.test(money ~ var, data = df, var.equal = T)

# Расчет t-теста с поправкой Уэлча
t.test(money ~ var, data = df, var.equal = F)


# График распределения
ggplot(df, aes(x = money, fill = var)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Metric distribution", x = "Metric", y = "Frequency", fill = "Var") +
  theme_minimal()


# Вычисляем среднее, стандартное отклонение и количество наблюдений
df_summary <- df %>%
  group_by(var) %>%
  summarise(mean_money = mean(money),
            sd_money = sd(money),
            n = n(),
            .groups = "drop")

# Вычисляем стандартную ошибку и доверительные интервалы
df_summary <- df_summary %>%
  mutate(se = sd_money / sqrt(n),
         ci_low = mean_money - qt(0.975, df = n - 1) * se,
         ci_high = mean_money + qt(0.975, df = n - 1) * se)

# График с доверительными интервалами
ggplot(df_summary, aes(x = var, y = mean_money, ymin = ci_low, ymax = ci_high, col = var)) +
  geom_pointrange() +
  labs(x = "Var", y = "Metric", title = "Sig. Intervals") +
  theme_minimal()

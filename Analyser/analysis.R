

#Analysis script for IDR4000 exam


 --- Setup ---
rm(list = ls())


df <- read.csv("Data/hypertrophy.csv")


names(df)
dim(df)
head(df)

summary(df$VL_T1)
summary(df$VL_T4)


summary(df$VL_delta)

sum(is.na(df$VL_delta))


df$VL_baseline <- df$VL_T1
df$VL_post <- df$VL_T4
df$VL_delta <- df$VL_post - df$VL_baseline



sum(is.na(df$VL_delta))



summary(df$VL_delta)



df_complete <- df[!is.na(df$VL_delta), ] 


summary(df_complete$VL_delta)


nrow(df_complete)


sd(df_complete$VL_delta)




hist(
  df_complete$VL_delta,
  breaks = 10,
  main = "Distribution of change in Vastus Lateralis muscle thickness",
  xlab = "Change in muscle thickness (cm)",
  ylab = "Frequency",
  col = "lightgray",
  border = "black"
)


names(df_complete)


plot(
  df_complete$VL_T1,
  df_complete$VL_delta,
  xlab = "Baseline Vastus lateralis thickness (cm)",
  ylab = "Change in muscle thickness (cm)",
  main = "Baseline muscle thickness and hypertrophy response",
  pch = 19
)




abline(
  lm(df_complete$VL_delta ~ df_complete$VL_T1),
  col = "red",
  lwd = 2
)



cor.test(
  df_complete$VL_T1,
  df_complete$VL_delta,
  method = "pearson"
)



tabell1 <- data.frame(
  Variabel = "VL_delta",
  n = nrow(df_complete),
  Mean_cm = mean(df_complete$VL_delta),
  SD_cm = sd(df_complete$VL_delta),
  Median_cm = median(df_complete$VL_delta),
  Min_cm = min(df_complete$VL_delta),
  Max_cm = max(df_complete$VL_delta)
)


tabell1[, -1] <- round(tabell1[, -1], 2)

tabell1

tabel11 <- data.frame(
  variabel = "VL_delta",
  n = nrow(df_complete),
  Mean_cm = mean(df_complete$VL_delta),
  SD_cm = sd(df_complete$VL_delta),
  Median_cm = median(df_complete$VL_delta),
  Min_cm = min(df_complete$VL_delta),
  Max_cm = max(df_complete$VL_delta)
)


tabel11[, -1] <- round(tabel11[, -1], 2)



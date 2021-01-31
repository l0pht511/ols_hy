library(tidyverse)
install.packages("forecast")
library(forecast)
files <- list.files("data", recursive=T,full.names=T)
df <- tibble()
for (f in files) {
  data <- read_delim(
    f,
    delim="|",
    quote = "\"",
    comment = "[",
    col_names = c("Factor", "Return", "DataDate"),
    col_types = cols(Factor = col_character(),
                     Return = col_double(),
                     DataDate = col_date(format = "%Y%m%d")),
    skip = 3
  )
  df <- rbind(df, data)
}

df_wider <- df %>%
  pivot_wider(names_from = "Factor", values_from = "Return")

df_wider <- select(df_wider, -JP_CDT_DTS_SHINS)
df_wider <- select(df_wider, -NO_CDT_DTS_CORP)
df_wider <- select(df_wider, -ARS_GOV_1Y)
df_wider <- select(df_wider, -DOP_GOV_1Y)
df_wider <- select(df_wider, -UYU_GOV_1Y)
df_wider <- select(df_wider, -ARS_GOV_2Y)
df_wider <- select(df_wider, -DOP_GOV_2Y)
df_wider <- select(df_wider, -UYU_GOV_2Y)
df_wider <- select(df_wider, -ARS_GOV_5Y)
df_wider <- select(df_wider, -DOP_GOV_5Y)
df_wider <- select(df_wider, -UYU_GOV_5Y)
df_wider <- select(df_wider, -ARS_GOV_10Y)
df_wider <- select(df_wider, -DOP_GOV_10Y)
df_wider <- select(df_wider, -UYU_GOV_10Y)
df_wider <- select(df_wider, -ARS_GOV_30Y)
df_wider <- select(df_wider, -DOP_GOV_30Y)
df_wider <- select(df_wider, -UYU_GOV_30Y)
df_wider[is.na(df_wider)] <- 0


index_df <- read_csv("bloomberg_index.csv")
index_df <- mutate(index_df, US_High_Yield_return = log(US_High_Yield) - lag(log(US_High_Yield)))
index_df <- rename(index_df, DataDate = Date)
df_factor_return <- inner_join(df_wider, select(.data = index_df, DataDate, US_High_Yield_return), by="DataDate")
na.omit(df_factor_return)

lm2 <- lm(US_High_Yield_return~.,data=select(df_factor_return, -DataDate))
summary(lm2)

shapiro.test(residuals(lm2))
g <- ggplot(mapping = aes(residuals(lm2))) + geom_histogram()
plot(g)

g <- ggplot(df_factor_return, aes(DataDate, US_High_Yield_return)) + 
  geom_line()
plot(g)

g <- ggplot(index_df, aes(DataDate, US_High_Yield)) + 
  geom_line()
plot(g)

plot(residuals(lm2))

lm2_coeff <- coef(lm2)
lm2_coeff_tb<- tibble:::enframe(lm2_coeff, name = "factor", value = "coefficient")
lm2_coeff_tb <- lm2_coeff_tb %>% dplyr::arrange(desc(abs(coefficient)))
summary(lm2)
forecast::checkresiduals(lm2$residuals)

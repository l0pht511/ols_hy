library(tidyverse)
#install.packages("forecast")
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

df <- filter(df, str_detect(Factor, "^US"))
df <- filter(df, Factor != "USD_SWAP_SPREAD_50Y")
df <- filter(df, Factor != "USD_INFL_50Y")
df <- filter(df, Factor != "USD_GOV_50Y")
df <- filter(df, Factor != "USD_MBS_HYBRID")

df_wider <- df %>%
  pivot_wider(names_from = "Factor", values_from = "Return")

df_wider[is.na(df_wider)] <- 0

df_wider_mlt <- select(df_wider, US_CDT_DTS_AGNCY,
                       US_CDT_DTS_COND_AUTOM_IG,
                       US_CDT_DTS_COND_DURAP_IG,
                       US_CDT_DTS_COND_HY,
                       US_CDT_DTS_COND_IG,
                       US_CDT_DTS_COND_MEDIA_IG,
                       US_CDT_DTS_COND_RET_IG,
                       US_CDT_DTS_COND_SERV_IG,
                       US_CDT_DTS_CONS_FOOD_IG,
                       US_CDT_DTS_CONS_HY,
                       US_CDT_DTS_CONS_IG,
                       US_CDT_DTS_CONS_PROD_IG,
                       US_CDT_DTS_CONS_RET_IG,
                       US_CDT_DTS_CORP_HY,
                       US_CDT_DTS_CORP_IG,
                       US_CDT_DTS_ENGY_EQSV_IG,
                       US_CDT_DTS_ENGY_HY,
                       US_CDT_DTS_ENGY_IG,
                       US_CDT_DTS_ENGY_OILGS_IG,
                       US_CDT_DTS_FIN_BANK_HY,
                       US_CDT_DTS_FIN_BANK_IG,
                       US_CDT_DTS_FIN_CAPMK_IG,
                       US_CDT_DTS_FIN_DVRSF_IG,
                       US_CDT_DTS_FIN_HY,
                       US_CDT_DTS_FIN_IG,
                       US_CDT_DTS_FIN_INSR_IG,
                       US_CDT_DTS_FIN_REAL_IG,
                       US_CDT_DTS_HLTH_EQSV_IG,
                       US_CDT_DTS_HLTH_HY,
                       US_CDT_DTS_HLTH_IG,
                       US_CDT_DTS_HLTH_PHRM_IG,
                       US_CDT_DTS_IND_CAPGD_IG,
                       US_CDT_DTS_IND_HY,
                       US_CDT_DTS_IND_IG,
                       US_CDT_DTS_IND_SERV_IG,
                       US_CDT_DTS_MAT_CHEM_IG,
                       US_CDT_DTS_MAT_HY,
                       US_CDT_DTS_MAT_IG,
                       US_CDT_DTS_MAT_MET_IG,
                       US_CDT_DTS_SOVSUP,
                       US_CDT_DTS_TECH_HARD_IG,
                       US_CDT_DTS_TECH_HY,
                       US_CDT_DTS_TECH_IG,
                       US_CDT_DTS_TECH_SEMI_IG,
                       US_CDT_DTS_TECH_SOFT_IG,
                       US_CDT_DTS_TEL_DVRST_IG,
                       US_CDT_DTS_TEL_HY,
                       US_CDT_DTS_TEL_IG,
                       US_CDT_DTS_TEL_WIREL_IG,
                       US_CDT_DTS_TRANS_GND_IG,
                       US_CDT_DTS_TRANS_HY,
                       US_CDT_DTS_TRANS_IG,
                       US_CDT_DTS_UTL_ELTC_IG,
                       US_CDT_DTS_UTL_HY,
                       US_CDT_DTS_UTL_IG,
                       US_CDT_DTS_UTL_MULTI_IG,
                       USD_ABS_ALL,
                       USD_ABS_AUTO,
                       USD_ABS_CREDIT_CARD,
                       USD_ABS_HOME_EQUITY,
                       USD_ABS_MANUFACTURED,
                       USD_ABS_MISCELLANEOUS,
                       USD_ABS_STUDENT_LOAN,
                       USD_ABS_UTILITIES,
                       USD_BASIS_CDS_HY,
                       USD_BASIS_CDS_IG,
                       USD_BASIS_OTR_10Y,
                       USD_BASIS_OTR_1Y,
                       USD_BASIS_OTR_5Y,
                       USD_CMBS_ALL,
                       USD_GOV_10Y,
                       USD_GOV_1M,
                       USD_GOV_1Y,
                       USD_GOV_20Y,
                       USD_GOV_2Y,
                       USD_GOV_30Y,
                       USD_GOV_5Y,
                       USD_GOV_6M,
                       USD_IMPVOL_IMPLIED_VOL,
                       USD_INFL_10Y,
                       USD_INFL_1Y,
                       USD_INFL_20Y,
                       USD_INFL_2Y,
                       USD_INFL_30Y,
                       USD_INFL_5Y,
                       USD_LOAN_ALL,
                       USD_LOAN_COND,
                       USD_LOAN_CONS,
                       USD_LOAN_ENGY,
                       USD_LOAN_FIN,
                       USD_LOAN_HLTH,
                       USD_LOAN_IND,
                       USD_LOAN_MAT,
                       USD_LOAN_TEL,
                       USD_LOAN_TRANS,
                       USD_LOAN_UTL,
                       USD_MBS_ALL,
                       USD_MBS_CONVENTIONAL,
                       USD_MBS_CONVENTIONAL_15YR,
                       USD_MBS_CONVENTIONAL_30YR,
                       USD_MBS_CURRENT_COUPON,
                       USD_MBS_GNMA,
                       USD_MBS_GNMA_15YR,
                       USD_MBS_GNMA_30YR,
                       USD_MUNI_10Y,
                       USD_MUNI_1Y,
                       USD_MUNI_20Y,
                       USD_MUNI_2Y,
                       USD_MUNI_30Y,
                       USD_MUNI_5Y,
                       USD_MUNI_6M,
                       USD_MUNI_DTS_SPREAD,
                       USD_SWAP_SPREAD_10Y,
                       USD_SWAP_SPREAD_1M,
                       USD_SWAP_SPREAD_1Y,
                       USD_SWAP_SPREAD_20Y,
                       USD_SWAP_SPREAD_2Y,
                       USD_SWAP_SPREAD_30Y,
                       USD_SWAP_SPREAD_5Y,
                       USD_SWAP_SPREAD_6M
)

mlt_return <- as.matrix(df_wider_mlt) %*% A
mlt_return <- as_tibble(mlt_return)
colnames(mlt_return) <- c("US_CDT_DTS_AGNCY",
                          "US_CDT_DTS_COND_AUTOM_IG",
                          "US_CDT_DTS_COND_DURAP_IG",
                          "US_CDT_DTS_COND_HY",
                          "US_CDT_DTS_COND_IG",
                          "US_CDT_DTS_COND_MEDIA_IG",
                          "US_CDT_DTS_COND_RET_IG",
                          "US_CDT_DTS_COND_SERV_IG",
                          "US_CDT_DTS_CONS_FOOD_IG",
                          "US_CDT_DTS_CONS_HY",
                          "US_CDT_DTS_CONS_IG",
                          "US_CDT_DTS_CONS_PROD_IG",
                          "US_CDT_DTS_CONS_RET_IG",
                          "US_CDT_DTS_CORP_HY",
                          "US_CDT_DTS_CORP_IG",
                          "US_CDT_DTS_ENGY_EQSV_IG",
                          "US_CDT_DTS_ENGY_HY",
                          "US_CDT_DTS_ENGY_IG",
                          "US_CDT_DTS_ENGY_OILGS_IG",
                          "US_CDT_DTS_FIN_BANK_HY",
                          "US_CDT_DTS_FIN_BANK_IG",
                          "US_CDT_DTS_FIN_CAPMK_IG",
                          "US_CDT_DTS_FIN_DVRSF_IG",
                          "US_CDT_DTS_FIN_HY",
                          "US_CDT_DTS_FIN_IG",
                          "US_CDT_DTS_FIN_INSR_IG",
                          "US_CDT_DTS_FIN_REAL_IG",
                          "US_CDT_DTS_HLTH_EQSV_IG",
                          "US_CDT_DTS_HLTH_HY",
                          "US_CDT_DTS_HLTH_IG",
                          "US_CDT_DTS_HLTH_PHRM_IG",
                          "US_CDT_DTS_IND_CAPGD_IG",
                          "US_CDT_DTS_IND_HY",
                          "US_CDT_DTS_IND_IG",
                          "US_CDT_DTS_IND_SERV_IG",
                          "US_CDT_DTS_MAT_CHEM_IG",
                          "US_CDT_DTS_MAT_HY",
                          "US_CDT_DTS_MAT_IG",
                          "US_CDT_DTS_MAT_MET_IG",
                          "US_CDT_DTS_SOVSUP",
                          "US_CDT_DTS_TECH_HARD_IG",
                          "US_CDT_DTS_TECH_HY",
                          "US_CDT_DTS_TECH_IG",
                          "US_CDT_DTS_TECH_SEMI_IG",
                          "US_CDT_DTS_TECH_SOFT_IG",
                          "US_CDT_DTS_TEL_DVRST_IG",
                          "US_CDT_DTS_TEL_HY",
                          "US_CDT_DTS_TEL_IG",
                          "US_CDT_DTS_TEL_WIREL_IG",
                          "US_CDT_DTS_TRANS_GND_IG",
                          "US_CDT_DTS_TRANS_HY",
                          "US_CDT_DTS_TRANS_IG",
                          "US_CDT_DTS_UTL_ELTC_IG",
                          "US_CDT_DTS_UTL_HY",
                          "US_CDT_DTS_UTL_IG",
                          "US_CDT_DTS_UTL_MULTI_IG",
                          "USD_ABS_ALL",
                          "USD_ABS_AUTO",
                          "USD_ABS_CREDIT_CARD",
                          "USD_ABS_HOME_EQUITY",
                          "USD_ABS_MANUFACTURED",
                          "USD_ABS_MISCELLANEOUS",
                          "USD_ABS_STUDENT_LOAN",
                          "USD_ABS_UTILITIES",
                          "USD_BASIS_CDS_HY",
                          "USD_BASIS_CDS_IG",
                          "USD_BASIS_OTR_10Y",
                          "USD_BASIS_OTR_1Y",
                          "USD_BASIS_OTR_5Y",
                          "USD_CMBS_ALL",
                          "USD_GOV_10Y",
                          "USD_GOV_1M",
                          "USD_GOV_1Y",
                          "USD_GOV_20Y",
                          "USD_GOV_2Y",
                          "USD_GOV_30Y",
                          "USD_GOV_5Y",
                          "USD_GOV_6M",
                          "USD_IMPVOL_IMPLIED_VOL",
                          "USD_INFL_10Y",
                          "USD_INFL_1Y",
                          "USD_INFL_20Y",
                          "USD_INFL_2Y",
                          "USD_INFL_30Y",
                          "USD_INFL_5Y",
                          "USD_LOAN_ALL",
                          "USD_LOAN_COND",
                          "USD_LOAN_CONS",
                          "USD_LOAN_ENGY",
                          "USD_LOAN_FIN",
                          "USD_LOAN_HLTH",
                          "USD_LOAN_IND",
                          "USD_LOAN_MAT",
                          "USD_LOAN_TEL",
                          "USD_LOAN_TRANS",
                          "USD_LOAN_UTL",
                          "USD_MBS_ALL",
                          "USD_MBS_CONVENTIONAL",
                          "USD_MBS_CONVENTIONAL_15YR",
                          "USD_MBS_CONVENTIONAL_30YR",
                          "USD_MBS_CURRENT_COUPON",
                          "USD_MBS_GNMA",
                          "USD_MBS_GNMA_15YR",
                          "USD_MBS_GNMA_30YR",
                          "USD_MUNI_10Y",
                          "USD_MUNI_1Y",
                          "USD_MUNI_20Y",
                          "USD_MUNI_2Y",
                          "USD_MUNI_30Y",
                          "USD_MUNI_5Y",
                          "USD_MUNI_6M",
                          "USD_MUNI_DTS_SPREAD",
                          "USD_SWAP_SPREAD_10Y",
                          "USD_SWAP_SPREAD_1M",
                          "USD_SWAP_SPREAD_1Y",
                          "USD_SWAP_SPREAD_20Y",
                          "USD_SWAP_SPREAD_2Y",
                          "USD_SWAP_SPREAD_30Y",
                          "USD_SWAP_SPREAD_5Y",
                          "USD_SWAP_SPREAD_6M")

mlt_return <- bind_cols(mlt_return, select(df_wider, DataDate))

index_df <- read_csv("bloomberg_index.csv")
index_df <- mutate(index_df, US_High_Yield_return = log(US_High_Yield) - lag(log(US_High_Yield)))
index_df <- rename(index_df, DataDate = Date)
df_factor_return <- inner_join(mlt_return, select(.data = index_df, DataDate, US_High_Yield_return), by="DataDate")
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


#test_m <- as.matrix(df_wider_mlt)


#write_csv(as_tibble(cor(test_m %*% A)), "cor.csv")


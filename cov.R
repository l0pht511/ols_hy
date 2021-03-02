df_wider_us <- select(df_wider, USD_MUNI_6M, USD_GOV_1M, USD_GOV_6M, USD_MUNI_1Y, USD_GOV_1Y, USD_MUNI_2Y, USD_MUNI_5Y, USD_GOV_2Y, USD_GOV_5Y, USD_MUNI_10Y, USD_MUNI_20Y, 
                      USD_GOV_10Y, USD_GOV_20Y, USD_MUNI_30Y, USD_GOV_30Y, USD_GOV_50Y, USD_SWAP_SPREAD_1M, USD_LOAN_ALL, USD_MBS_CONVENTIONAL_15YR, US_CDT_DTS_AGNCY, USD_CMBS_ALL, 
                      USD_ABS_AUTO, USD_MUNI_DTS_SPREAD, USD_SWAP_SPREAD_6M, USD_MBS_CONVENTIONAL_30YR, USD_LOAN_COND, US_CDT_DTS_SOVSUP, USD_ABS_CREDIT_CARD, USD_ABS_HOME_EQUITY,
                      USD_SWAP_SPREAD_1Y, USD_MBS_GNMA_15YR, USD_LOAN_CONS, US_CDT_DTS_COND_IG, USD_ABS_MANUFACTURED, USD_SWAP_SPREAD_2Y, US_CDT_DTS_COND_HY, USD_LOAN_ENGY, USD_MBS_GNMA_30YR,
                      USD_LOAN_FIN, US_CDT_DTS_CONS_IG, USD_ABS_STUDENT_LOAN, USD_SWAP_SPREAD_5Y, USD_MBS_CURRENT_COUPON, USD_LOAN_HLTH, USD_MBS_GNMA, USD_ABS_UTILITIES, US_CDT_DTS_CONS_HY,
                      USD_SWAP_SPREAD_10Y, US_CDT_DTS_ENGY_IG, USD_ABS_MISCELLANEOUS, USD_LOAN_IND, USD_SWAP_SPREAD_20Y, USD_MBS_CONVENTIONAL, USD_LOAN_MAT, USD_ABS_ALL, USD_MBS_HYBRID,
                      USD_SWAP_SPREAD_30Y, US_CDT_DTS_ENGY_HY, US_CDT_DTS_FIN_IG, USD_MBS_ALL, USD_SWAP_SPREAD_50Y, USD_LOAN_TEL, US_CDT_DTS_FIN_HY, USD_LOAN_TRANS, USD_LOAN_UTL,
                      US_CDT_DTS_HLTH_IG, US_CDT_DTS_HLTH_HY, US_CDT_DTS_IND_IG, US_CDT_DTS_IND_HY, US_CDT_DTS_MAT_IG, US_CDT_DTS_MAT_HY, US_CDT_DTS_TECH_IG, US_CDT_DTS_TECH_HY,
                      US_CDT_DTS_TEL_IG, US_CDT_DTS_TEL_HY, US_CDT_DTS_TRANS_IG, US_CDT_DTS_TRANS_HY, US_CDT_DTS_UTL_IG, US_CDT_DTS_UTL_HY, US_CDT_DTS_COND_AUTOM_IG, US_CDT_DTS_COND_DURAP_IG,
                      US_CDT_DTS_COND_MEDIA_IG, US_CDT_DTS_COND_RET_IG, US_CDT_DTS_COND_SERV_IG, US_CDT_DTS_CONS_FOOD_IG, US_CDT_DTS_CONS_PROD_IG, US_CDT_DTS_CONS_RET_IG, US_CDT_DTS_ENGY_EQSV_IG,
                      US_CDT_DTS_ENGY_OILGS_IG, US_CDT_DTS_FIN_BANK_IG, US_CDT_DTS_FIN_BANK_HY, US_CDT_DTS_FIN_CAPMK_IG, US_CDT_DTS_FIN_DVRSF_IG, US_CDT_DTS_FIN_INSR_IG, US_CDT_DTS_FIN_REAL_IG,
                      US_CDT_DTS_HLTH_EQSV_IG, US_CDT_DTS_HLTH_PHRM_IG, US_CDT_DTS_IND_CAPGD_IG, US_CDT_DTS_IND_SERV_IG, US_CDT_DTS_MAT_CHEM_IG, US_CDT_DTS_MAT_MET_IG, US_CDT_DTS_TECH_HARD_IG,
                      US_CDT_DTS_TECH_SEMI_IG, US_CDT_DTS_TECH_SOFT_IG, US_CDT_DTS_TEL_DVRST_IG, US_CDT_DTS_TEL_WIREL_IG, US_CDT_DTS_TRANS_GND_IG, US_CDT_DTS_UTL_ELTC_IG, US_CDT_DTS_UTL_MULTI_IG,
                      US_CDT_DTS_CORP_IG, US_CDT_DTS_CORP_HY, USD_IMPVOL_IMPLIED_VOL, USD_BASIS_OTR_1Y, USD_BASIS_CDS_IG, USD_BASIS_OTR_5Y, USD_BASIS_CDS_HY, USD_BASIS_OTR_10Y, USD_INFL_1Y, 
                      USD_INFL_2Y, USD_INFL_5Y, USD_INFL_10Y, USD_INFL_20Y, USD_INFL_30Y, USD_INFL_50Y)

write.csv(scale_cov_us, file = "my_data.csv", row.names = FALSE)

cov_us = cov(df_wider_us)

torsion(scale_cov_us, model="minimum-torsion", method="exact", max_niter=10000L) 

sclae_df_wider_us <- scale(df_wider_us, center = TRUE, scale = TRUE)
scale_cov_us <- cov(sclae_df_wider_us)
scale_cov_us

install.packages("reshape2")
library(reshape2)
melted_covmat <- melt(scale_cov_us)
head(melted_covmat)

library(ggplot2)
ggplot(data = melted_covmat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

cov_msci <- read_delim(
  "FI400L_Covariance.20201225",
  delim="|",
  quote = "\"",
  comment = "[",
  col_names = c("Factor1", "Factor2", "VarCovar", "DataDate"),
  col_types = cols(Factor1 = col_character(),
                   Factor2 = col_character(),
                   VarCovar = col_number(),
                   DataDate = col_date(format = "%Y%m%d")),
  skip = 3
)

cov_msci_usa <- select(cov_msci, -DataDate)
cov_msci_usa <- select(cov_msci, )
cov_msci_t <- cov_msci %>%

cov_m <- cov_msci %>%
  tidyr::spread(key = Factor1, value = VarCovar)
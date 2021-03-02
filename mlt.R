# MLT
library(tidyverse)
library(stringr)

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
cov_msci_usa <- filter(cov_msci_usa, str_detect(Factor1, "^US") & str_detect(Factor2, "^US"))
cov_msci_usa <- filter(cov_msci_usa, Factor1!="USD_SWAP_SPREAD_50Y" & Factor2!="USD_SWAP_SPREAD_50Y")
cov_msci_usa <- filter(cov_msci_usa, Factor1!="USD_INFL_50Y" & Factor2!="USD_INFL_50Y")
cov_msci_usa <- filter(cov_msci_usa, Factor1!="USD_GOV_50Y" & Factor2!="USD_GOV_50Y")

cov_msci_t <- cov_msci_usa %>%
  filter(Factor1 != Factor2) %>%
  rename(Factor1=Factor2, Factor2=Factor1) %>%
  select(Factor1, Factor2, VarCovar)
cov_msci_usa <- rbind(cov_msci_usa, cov_msci_t)

sigma <- cov_msci_usa %>%
  tidyr::spread(key = Factor1, value = VarCovar)　%>%
  arrange(Factor2) %>%
  select(-Factor2) %>%
  as.matrix()

qr(sigma)$rank
# step1
d <- diag(diag(sigma), ncol(sigma), nrow(sigma))
d <- sqrt(d)
# step2
e <- eigen(sigma)
P <- e$vectors
lamda <- sqrt(diag(e$values))
svd <- svd(lamda %*% t(P) %*% d)
A <- P %*% solve(lamda) %*% svd$u %*% t(svd$v) %*% d

B <- t(A) %*% sigma %*% A

write_csv(as_tibble(cov2cor(B)), "MLT.csv")

library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(survival)
library(mgcv)
library(pammtools)

Set1 <- RColorBrewer::brewer.pal(9, "Set1")

# pbc data analysis
data("pbc", package = "survival")
head(pbc)[, c(1:5, 11, 12)]
head(pbcseq)[, c(1, 4:5, 7, 12, 13)]

pbc <- pbc %>% mutate(bili = log(bili), protime = log(protime))
pbcseq <- pbcseq %>% mutate(bili = log(bili), protime = log(protime))

pbc <- pbc %>% filter(id <= 312) %>%
  select(id:sex, bili, protime) %>%
  mutate(status = 1L * (status == 2))

pbc_list <- list(pbc, pbcseq)

pbc_ped <- as_ped(
  data = list(pbc, pbcseq),
  formula = Surv(time, status) ~ . + concurrent(bili, protime, tz_var = "day"),
  id = "id")

typeof(pbc_ped$ped_status)

pbc_pam <- gam(ped_status ~ s(tend) + bili + protime, data = pbc_ped,
  family = poisson(), offset = offset)

## Effect of bilirubin
# note that we use the reference argument to calculate
# the relative risk change (x - \bar{x})'\beta for comparison with predict.coxph
# (see also Details section in ?predict.coxph)
reference = sample_info(pbc_ped)
bili_df <- pbc_ped %>% ungroup() %>%
  make_newdata(bili = seq_range(bili, n = 100)) %>%
  add_term(pbc_pam, term = "bili", reference = reference)

length(reference)
length(seq_range(pbc_ped$bili, n=100))

## Effect of protime
protime_df <- pbc_ped %>% ungroup() %>%
  make_newdata(protime = seq_range(protime, n=100)) %>%
  add_term(pbc_pam, term = "protime", reference = reference)

# visualization
# remember that bili and protime are log transformed
p_term <- ggplot(data = NULL, aes(y = fit)) + geom_line(aes(col = "PAM")) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  scale_colour_manual(name = "Method", values = c("#E41A1C", "#000000"))
gridExtra::grid.arrange(
  p_term %+% bili_df + aes(x = exp(bili)),
  p_term %+% protime_df + aes(x = exp(protime)),
  nrow = 1L)


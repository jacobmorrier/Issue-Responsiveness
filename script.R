### Loading Libraries

library('dplyr')
library("tidyverse")
library("tidytext")
library("tm")
library("stm")
library("SnowballC")
library("quanteda")
library("quanteda.textmodels")
library("zoo")
library("udpipe")
library("showtext")
library("fastDummies")
library("AER")
library("showtext")
library("reshape2")
library('readxl')
library('extrafont')
library('stargazer')
library('rJST')
library('ggpubr')
library("clValid")
require('seededlda')
library("TTR")
library("orcutt")
library("prais")
library("pracma")
library("car")
library("sjmisc")
library("latex2exp")
library("ggrepel")
library("KraljicMatrix")
library('ggrepel')
library('haven')

### Setting Working Directory 
### (TO CHANGE ACCORDING TO THE LOCATION OF FILES IN YOUR COMPUTER)

setwd("~/Documents/Issue Responsiveness -- Reproduction Files")

### Figure 1

read_dta("2019 Canadian Election Study - Online Survey v1.0.dta") %>%
  filter(!is.na(pes19_weight_general_restricted) & pes19_votechoice2019 %in% 1:3) %>%
  mutate(pes19_votechoice2019 = c("LPC", "CPC", "NDP")[pes19_votechoice2019],
         pes19_cc1 = c("Yes", "No", "Don't know")[pes19_cc1],
         pes19_cc1 = factor(pes19_cc1, 
                            ordered = TRUE, 
                            levels = c("Yes", "No", "Don't know")),
         pes19_cc2 = c("Human activities", 
                       "Natural changes", 
                       "Other", 
                       "Don't know")[pes19_cc2],
         pes19_cc2 = ifelse(is.na(pes19_cc2), "Not Applicable", pes19_cc2),
         pes19_cc2 = factor(pes19_cc2, 
                            ordered = TRUE, 
                            levels = c("Human activities", 
                                       "Natural changes", 
                                       "Other", 
                                       "Don't know", 
                                       "Not Applicable"))) %>%
  group_by(pes19_votechoice2019) %>%
  count(pes19_cc1, pes19_cc2, wt = pes19_weight_general_restricted) %>%
  mutate(n = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = pes19_cc1, 
                       y = n, 
                       fill = pes19_cc2, 
                       label = round(n))) + 
  facet_wrap(vars(pes19_votechoice2019)) + 
  geom_col() + 
  geom_text(mapping = aes(color = pes19_cc2), 
            position = position_stack(vjust = .5), 
            size = 2.5, 
            show.legend = FALSE) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title.align = 0.5) +
  scale_color_manual(values = rev(c('black', 'black', 'black', 'black', 'white'))) +
  scale_fill_grey() +
  labs(x = "Do you think that climate change is happening?", 
       y = "Share of Voters (%)", 
       fill = "What do you think is the\nmain cause of climate change?") + 
  guides(fill = guide_legend(ncol = 3))

ggsave("Figure 1.pdf", width = 6.5, height = 5)

### Loading Google Trends Data

ts_GT <- read_excel("data.xlsx") %>%
  as.data.frame() %>%
  select(Week, Index_Canada, Index_UnitedStates) %>%
  rename(Index_CAN = Index_Canada,
         Index_USA = Index_UnitedStates) %>%
  mutate(Week = as.Date(Week)) %>%
  filter(Week >= as.Date("2006-04-02"))

base <- ts_GT %>%
  filter(Week == as.Date("2006-04-02")) %>%
  select(Index_CAN) %>%
  as.numeric()

ts_GT <- ts_GT %>%
  mutate_at(vars(Index_CAN, Index_USA), function(x){round(x) / base * 100})

### Figure 2

ts_GT %>%
  melt(id = 'Week') %>%
  mutate(variable = case_when(
    variable == 'Index_CAN' ~ 'Canada',
    variable == 'Index_USA' ~ 'United States'
  )) %>%
  ggplot(mapping = aes(x = Week, 
                       y = value, 
                       group = variable, 
                       color = variable, 
                       linetype = variable)) +
  geom_line(size = 0.5) +
  scale_color_grey() +
  scale_y_continuous(trans = 'log10') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = "Public Salience (log scale)", 
       color = "Country", 
       linetype = "Country")

ggsave("Figure 2.pdf", width = 6.5, height = 5)

ts_CAN <- ts(ts_GT %>% select(Index_CAN), frequency = 52, start = c(2006, 14))

### Loading Question Period Interventions

load("data.Rdata")

df_QP <- df_QP %>%
  mutate(Week = NA)

for (i in 1:dim(df_QP)[1]){
  
  d <- df_QP$Date[i]
  prev.days <- seq(d - 6, d, by = 'day')
  df_QP$Week[i] <- as.Date(prev.days[weekdays(prev.days) == 'Sunday'])
  
}

df_QP <- df_QP %>%
  mutate(Week = as.Date(Week),
         Year = as.factor(as.numeric(format(Date, "%Y"))),
         Line = as.numeric(rownames(df_QP)))

### Sampling a Training Set

set.seed(8)

df_QP <- df_QP %>%
  mutate(Training = rbinom(n = dim(df_QP)[1], size = 1, prob = 0.1)) %>%
  dummy_cols(select_columns = "Party")

### Preprocesing Text Data

custom_stop <- c("ndp", 
                 "parti", 
                 "prime", 
                 "bloc", 
                 "conserv", 
                 "secretari", 
                 "parliamentari", 
                 "madam", 
                 "hon", 
                 "liber")

df_tidy <- df_QP %>%
  select(Line, Text, Training) %>%
  mutate(Text = removePunctuation(Text),
         Text = removeNumbers(Text)) 

tidy <- df_tidy %>%
  filter(Training == 1) %>%
  unnest_tokens(Term, Text) %>%
  anti_join(stop_words, by = c("Term" = "word")) %>%
  mutate(Term = wordStem(Term)) %>%
  filter(!(Term %in% custom_stop)) %>%
  filter(nchar(Term) > 2) %>%
  count(Line, Term) %>%
  group_by(Line) %>%
  filter(sum(n) > 20) %>%
  ungroup() %>%
  cast_dtm(Line, Term, n)

too_frequent <- removeSparseTerms(tidy, 0.75) %>% readCorpus(type = c("slam"))

tidy <- removeSparseTerms(tidy, 0.99)

tidy <- dfm_remove(as.dfm(tidy), too_frequent$vocab)

df_QP <- df_QP %>%
  mutate(Party = as.factor(Party))

tidy_stm <- convert(tidy, 
                    to = "stm", 
                    docvars = df_QP[as.numeric(tidy@Dimnames$docs),] %>%
                                  select(Line, Party, Year))

### Generating Diagnostic Values to Determine the Optimal Number of Topics

set.seed(8)

Diagnostic <- searchK(documents = tidy_stm$documents, 
                      vocab = tidy_stm$vocab, 
                      K = 5:30, 
                      proportion = 0, 
                      prevalence = ~ Party, 
                      data = tidy_stm$meta)

### Figure A1

pdf("Figure A1.pdf", height = 6.5, width = 6.5)

plot(Diagnostic)

dev.off()

### Figure A2

temp <- as.data.frame(Diagnostic$results[c("K", "heldout", "semcoh")]) %>%
  mutate_all(as.numeric) %>%
  mutate(frontier = NA)

for (i in 1:dim(temp)[1]) {

  sum <- sum(ifelse((temp$heldout > temp$heldout[i]) & (temp$semcoh > temp$semcoh[i]), 1, 0))

  temp$frontier[i] <- (sum == 0)

}

temp <- temp %>%
  arrange(semcoh)

ggplot(data = temp, mapping = aes(y = heldout, x = semcoh, label = K)) +
  geom_path(data = temp[temp$frontier,], color = "gray") +
  geom_point() +
  geom_text_repel(data = temp[temp$frontier,], size = 3, nudge_x = 0.5, nudge_y = 0.005) +
  theme_bw() +
  labs(x = "Semantic Coherence", y = "Held-Out Likelihood")

ggsave("Figure A2.pdf", width = 5.5, height = 5.5)

### Estimation of the Model with 15 Topics

ntopics <- 15

lda <- stm(documents = tidy_stm$documents, 
           vocab = tidy_stm$vocab, 
           K = ntopics, 
           prevalence = ~ Party, 
           data = tidy_stm$meta, 
           seed = 8)

Topic_Labels <- c("COVID-19 / Health Care",
                  "National Defense / Veterans Affairs",
                  "Taxes",
                  "Democratic Institutions / Investigations / RCMP",
                  "Foreign Policy / Immigration / Refugees",
                  "Criminal Justice / Culture / Official Languages / Human Rights",
                  "Economic Growth / Industry / Job Creation",
                  "Auditor General / Public Procurement / Treasury Board",
                  "Climate Change / Environment",
                  "Employment / Labor",
                  "Education / Research / Youth",
                  "Intergovernmental Affairs",
                  "Families / Middle Class / Seniors",
                  "Natural Resources / Infrastructure",
                  "Ethics")

tidy_out <- df_tidy %>%
  filter(Training == 0) %>%
  unnest_tokens(Term, Text) %>%
  anti_join(stop_words, by = c("Term" = "word")) %>%
  mutate(Term = wordStem(Term)) %>%
  filter(!(Term %in% custom_stop) & nchar(Term) > 2) %>%
  count(Line, Term) %>%
  group_by(Line) %>%
  filter(sum(n) > 10) %>%
  ungroup() %>%
  cast_dtm(Line, Term, n) %>%
  as.dfm() %>%
  dfm_match(lda$vocab) %>%
  convert(to = 'stm')

df_QP <- df_QP %>%
  filter(Line %in% as.numeric(names(tidy_out$documents)))

stm_out <- fitNewDocuments(model = lda, 
                           documents = tidy_out$documents, 
                           newData = tidy_out$meta)

Theta <- as.data.frame(stm_out$theta)

names(Theta) <- as.character(1:ntopics)

df_QP <- cbind(df_QP, Theta)

df_QP$Topic <- apply(df_QP %>% select(as.character(1:ntopics)), 
                     1, 
                     function(x){which(x==max(x))[1]})

df_QP <- dummy_cols(df_QP, select_columns = c("Topic"))

ts_salience <- df_QP %>%
  select(c("Week", 
           "Party", 
           as.character(1:ntopics), 
           paste("Topic", 1:ntopics, sep = "_"))) %>%
  group_by(Week, Party) %>%
  summarise_all(mean) %>%
  mutate_at(vars(c(as.character(1:ntopics), 
                   paste("Topic", 1:ntopics, sep = "_"))), 
            function(x){x * 100})

ts_salience <- merge(ts_GT, ts_salience, all.x = TRUE)

ts_salience <- rbind(ts_salience[!is.na(ts_salience$Party),], 
                cbind(ts_salience[is.na(ts_salience$Party), c("Week", "Index_CAN", "Index_USA")], matrix(rep(c("LPC", rep(NA, times = 2*ntopics)), times = dim(ts_salience[is.na(ts_salience$Party),])[1]), ncol = 2*ntopics + 1, byrow = TRUE, dimnames = list(NULL, c("Party", as.character(1:ntopics), paste("Topic", 1:ntopics, sep = "_"))))),
                cbind(ts_salience[is.na(ts_salience$Party), c("Week", "Index_CAN", "Index_USA")], matrix(rep(c("CPC", rep(NA, times = 2*ntopics)), times = dim(ts_salience[is.na(ts_salience$Party),])[1]), ncol = 2*ntopics + 1, byrow = TRUE, dimnames = list(NULL, c("Party", as.character(1:ntopics), paste("Topic", 1:ntopics, sep = "_"))))),
                cbind(ts_salience[is.na(ts_salience$Party), c("Week", "Index_CAN", "Index_USA")], matrix(rep(c("NDP", rep(NA, times = 2*ntopics)), times = dim(ts_salience[is.na(ts_salience$Party),])[1]), ncol = 2*ntopics + 1, byrow = TRUE, dimnames = list(NULL, c("Party", as.character(1:ntopics), paste("Topic", 1:ntopics, sep = "_"))))))

ts_salience <- ts_salience %>% 
  arrange(Party, Week) %>%
  mutate_at(c(as.character(1:ntopics), paste("Topic", 1:ntopics, sep = "_")), 
            as.numeric)

rownames(ts_salience) <- NULL

ts_salience <- dummy_cols(ts_salience, select_columns = c("Party")) %>%
  mutate(After_2015 = as.integer(Week > as.Date("2015-10-19")))

### Figure 3

coeff <- 10

ggplot(ts_salience, aes(x = Week, y = `9`)) + 
  geom_smooth(mapping = aes(group = Party, color = Party, linetype = Party), 
              method = "loess", 
              span = 0.25, 
              se = FALSE) +
  geom_smooth(mapping = aes(x = Week, y = Index_CAN / coeff), 
              colour="black", 
              method = "loess", 
              span = 0.25, 
              size = 1.5, 
              se = FALSE) +
  geom_vline(xintercept = as.Date("2006-01-23"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2008-10-14"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2011-05-02"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2015-10-19"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2019-10-21"), linetype = "dotted") + 
  geom_vline(xintercept = as.Date("2021-09-20"), linetype = "dotted") + 
  scale_color_manual(values = c("blue", "red", "orange")) +
  theme_bw() + 
  xlab("Week") + 
  ylab("Prevalence in Question Period Interventions (%)") + 
  scale_y_continuous(name = "Prevalence in Question Period Interventions (%)", 
                     sec.axis = sec_axis(~.*coeff,  name = "Public Salience")) +
  guides(colour = guide_legend("Party"), 
         linetype = guide_legend("Party")) + 
  theme(legend.position = "bottom")

ggsave("Figure 3.pdf", width = 6.5, height = 5)

### Figure 4

ts_salience <- ts_salience %>%
  mutate_at(vars(as.character((1:ntopics)[-9])), 
            function(x){unlist(log(x) - log(100 - x - ts_salience['9']))}) %>%
  mutate_at(vars('9'), function(x){log(x) - log(100 - x)})

temp <- NULL

for (party in c("CPC", "LPC", "NDP")) {
  
  temp <- c(temp, lag(ts_salience %>% filter(Party == party) %>% select(`9`)))
  
}

ggplot(ts_salience %>% filter(!is.na(`9`)), 
       aes(x = log(Index_CAN), y = `9`, color = Party, shape = Party)) + 
  geom_point(alpha = 0.333, size = 1) + 
  geom_smooth(aes(linetype = Party), se = FALSE) + 
  stat_cor(method = "pearson", cor.coef.name = 'rho', show.legend = FALSE) + 
  theme_bw() + 
  scale_color_manual(values = c("blue", "red", "orange")) + 
  theme(legend.position = "bottom") + 
  labs(y = TeX(r"( $\log\left(Prevalence / 1 - Prevalence\right)$ )"), 
       x = TeX(r"( $\log($Public Salience$)$ )"))

ggsave("Figure 4.pdf", width = 6.5, height = 5)

### Causal Estimates of Issue Responsiveness

ts_salience <- ts_salience %>%
  mutate(Before_2015 = 1 - After_2015)

base_reg_const <- lm(`9` ~ log(Index_CAN) + Party_LPC + Party_NDP, data = ts_salience)

base_reg_const_break <- lm(`9` ~ log(Index_CAN):After_2015 + (Party_LPC + Party_NDP) * After_2015 + log(Index_CAN):Before_2015, data = ts_salience)

base_reg_const_break_before <- lm(`9` ~ log(Index_CAN) + Party_LPC + Party_NDP, data = ts_salience[ts_salience$After_2015 == 0,])

base_reg_const_break_after <- lm(`9` ~ log(Index_CAN) + Party_LPC + Party_NDP, data = ts_salience[ts_salience$After_2015 == 1,])

iv_reg_const <- ivreg(`9` ~ log(Index_CAN) + Party_LPC + Party_NDP | 1 + log(Index_USA) + Party_LPC + Party_NDP, data = ts_salience)

iv_reg_const_break <- ivreg(`9` ~ After_2015 + log(Index_CAN):After_2015 + Party_LPC:After_2015 + Party_NDP:After_2015 + log(Index_CAN):Before_2015 + Party_LPC:Before_2015 + Party_NDP:Before_2015 | 1 + log(Index_USA) * After_2015 + Party_LPC * After_2015 + Party_NDP * After_2015, data = ts_salience)

iv_reg_const_break_before <- ivreg(`9` ~ log(Index_CAN) + Party_LPC + Party_NDP | 1 + log(Index_USA) + Party_LPC + Party_NDP, data = ts_salience[ts_salience$After_2015 == 0,])

iv_reg_const_break_after <- ivreg(`9` ~ log(Index_CAN) + Party_LPC + Party_NDP | 1 + log(Index_USA) + Party_LPC + Party_NDP, data = ts_salience[ts_salience$After_2015 == 1,])

base_reg_const <- cochrane.orcutt(base_reg_const)

base_reg_const_break <- cochrane.orcutt(base_reg_const_break)

base_reg_const_break_before <- cochrane.orcutt(base_reg_const_break_before)

base_reg_const_break_after <- cochrane.orcutt(base_reg_const_break_after)

iv_reg_const <- cochrane.orcutt(iv_reg_const)

iv_reg_const_break <- cochrane.orcutt(iv_reg_const_break)

iv_reg_const_break_before <- cochrane.orcutt(iv_reg_const_break_before)

iv_reg_const_break_after <- cochrane.orcutt(iv_reg_const_break_after)

base_reg <- lm(`9` ~ Party_LPC + Party_NDP + log(Index_CAN):Party_CPC + log(Index_CAN):Party_LPC + log(Index_CAN):Party_NDP, data = ts_salience)

base_reg_CPC <- lm(`9` ~ log(Index_CAN), data = ts_salience[ts_salience$Party == 'CPC',])

base_reg_LPC <- lm(`9` ~ log(Index_CAN), data = ts_salience[ts_salience$Party == 'LPC',])

base_reg_NDP <- lm(`9` ~ log(Index_CAN), data = ts_salience[ts_salience$Party == 'NDP',])

base_reg <- cochrane.orcutt(base_reg)

base_reg_CPC <- cochrane.orcutt(base_reg_CPC)

base_reg_LPC <- cochrane.orcutt(base_reg_LPC)

base_reg_NDP <- cochrane.orcutt(base_reg_NDP)

iv_reg <- ivreg(`9` ~ Party_LPC + Party_NDP + log(Index_CAN):Party_CPC + log(Index_CAN):Party_LPC + log(Index_CAN):Party_NDP | 1 + log(Index_USA) * Party_LPC + log(Index_USA) * Party_NDP, data = ts_salience)

iv_reg_CPC <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[ts_salience$Party == 'CPC',])

iv_reg_LPC <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[ts_salience$Party == 'LPC',])

iv_reg_NDP <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[ts_salience$Party == 'NDP',])

iv_reg <- cochrane.orcutt(iv_reg)

iv_reg_CPC <- cochrane.orcutt(iv_reg_CPC)

iv_reg_LPC <- cochrane.orcutt(iv_reg_LPC)

iv_reg_NDP <- cochrane.orcutt(iv_reg_NDP)

base_reg_break <- lm(`9` ~ (Party_LPC + Party_NDP) * After_2015 + (log(Index_CAN):Party_CPC + log(Index_CAN):Party_LPC + log(Index_CAN):Party_NDP):After_2015 + (log(Index_CAN):Party_CPC + log(Index_CAN):Party_LPC + log(Index_CAN):Party_NDP):Before_2015, data = ts_salience)

base_reg_break_before_CPC <- lm(`9` ~ log(Index_CAN), data = ts_salience[(ts_salience$After_2015 == 0) & (ts_salience$Party == 'CPC'),])

base_reg_break_after_CPC <- lm(`9` ~ log(Index_CAN), data = ts_salience[(ts_salience$After_2015 == 1) & (ts_salience$Party == 'CPC'),])

base_reg_break_before_LPC <- lm(`9` ~ log(Index_CAN), data = ts_salience[(ts_salience$After_2015 == 0) & (ts_salience$Party == 'LPC'),])

base_reg_break_after_LPC <- lm(`9` ~ log(Index_CAN), data = ts_salience[(ts_salience$After_2015 == 1) & (ts_salience$Party == 'LPC'),])

base_reg_break_before_NDP <- lm(`9` ~ log(Index_CAN), data = ts_salience[(ts_salience$After_2015 == 0) & (ts_salience$Party == 'NDP'),])

base_reg_break_after_NDP <- lm(`9` ~ log(Index_CAN), data = ts_salience[(ts_salience$After_2015 == 1) & (ts_salience$Party == 'NDP'),])

base_reg_break <- cochrane.orcutt(base_reg_break)

base_reg_break_before_CPC <- cochrane.orcutt(base_reg_break_before_CPC)

base_reg_break_after_CPC <- cochrane.orcutt(base_reg_break_after_CPC)

base_reg_break_before_LPC <- cochrane.orcutt(base_reg_break_before_LPC)

base_reg_break_after_LPC <- cochrane.orcutt(base_reg_break_after_LPC)

base_reg_break_before_NDP <- cochrane.orcutt(base_reg_break_before_NDP)

base_reg_break_after_NDP <- cochrane.orcutt(base_reg_break_after_NDP)

iv_reg_break <- ivreg(`9` ~ (Party_LPC + Party_NDP) * After_2015 + (log(Index_CAN):Party_CPC + log(Index_CAN):Party_LPC + log(Index_CAN):Party_NDP):After_2015 + (log(Index_CAN):Party_CPC + log(Index_CAN):Party_LPC + log(Index_CAN):Party_NDP):Before_2015 | 1 + log(Index_USA) * Party_LPC * After_2015 + log(Index_USA) * Party_NDP * After_2015, data = ts_salience)

iv_reg_break_before_CPC <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[(ts_salience$After_2015 == 0) & (ts_salience$Party == 'CPC'),])

iv_reg_break_after_CPC <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[(ts_salience$After_2015 == 1) & (ts_salience$Party == 'CPC'),])

iv_reg_break_before_LPC <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[(ts_salience$After_2015 == 0) & (ts_salience$Party == 'LPC'),])

iv_reg_break_after_LPC <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[(ts_salience$After_2015 == 1) & (ts_salience$Party == 'LPC'),])

iv_reg_break_before_NDP <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[(ts_salience$After_2015 == 0) & (ts_salience$Party == 'NDP'),])

iv_reg_break_after_NDP <- ivreg(`9` ~ log(Index_CAN) | 1 + log(Index_USA), data = ts_salience[(ts_salience$After_2015 == 1) & (ts_salience$Party == 'NDP'),])

iv_reg_break <- cochrane.orcutt(iv_reg_break)

iv_reg_break_before_CPC <- cochrane.orcutt(iv_reg_break_before_CPC)

iv_reg_break_after_CPC <- cochrane.orcutt(iv_reg_break_after_CPC)

iv_reg_break_before_LPC <- cochrane.orcutt(iv_reg_break_before_LPC)

iv_reg_break_after_LPC <- cochrane.orcutt(iv_reg_break_after_LPC)

iv_reg_break_before_NDP <- cochrane.orcutt(iv_reg_break_before_NDP)

iv_reg_break_after_NDP <- cochrane.orcutt(iv_reg_break_after_NDP)

stargazer(base_reg_const, base_reg, base_reg_const_break, base_reg_break, iv_reg_const, iv_reg, iv_reg_const_break, iv_reg_break)

### Causal Estimation of the Government's Issue Responsiveness

ts.salience.unmelt <- ts_salience[c("Week", "Index_CAN", "Index_USA", "Party", "9", "After_2015", "Before_2015")]

ts.salience.unmelt <- dcast(ts.salience.unmelt, Week + Index_CAN + Index_USA + After_2015 + Before_2015 ~ Party, value.var = "9")

ts.salience.unmelt$After_2011 <- ifelse(ts.salience.unmelt$Week >= as.Date("2011-05-03"), 1, 0)

ts.salience.unmelt$Before_2011 <- 1 - ts.salience.unmelt$After_2011

reg.CPC.base <- lm(CPC ~ log(Index_CAN), data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC.base <- cochrane.orcutt(reg.CPC.base)

reg.CPC.base.IV <- ivreg(CPC ~ log(Index_CAN) | log(Index_USA), data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC.base.IV <- cochrane.orcutt(reg.CPC.base.IV)

reg.CPC.base.Party <- lm(CPC ~ log(Index_CAN) + LPC + NDP, data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC.base.Party <- cochrane.orcutt(reg.CPC.base.Party)

reg.CPC.base.Party.IV <- ivreg(CPC ~ log(Index_CAN) + LPC + NDP | log(Index_USA) + LPC + NDP , data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC.base.Party.IV <- cochrane.orcutt(reg.CPC.base.Party.IV)

reg.CPC <- lm(CPC ~ log(Index_CAN) + (LPC + NDP):Before_2011 + (LPC + NDP):After_2011 + After_2011, data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC <- cochrane.orcutt(reg.CPC)

reg.CPC.IV <- ivreg(CPC ~ log(Index_CAN) + (LPC + NDP):Before_2011 + (LPC + NDP):After_2011 + After_2011 | log(Index_USA) + (LPC + NDP):Before_2011 + (LPC + NDP):After_2011 + After_2011, data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC.IV <- cochrane.orcutt(reg.CPC.IV)

stargazer(reg.CPC.base, reg.CPC.base.Party, reg.CPC, reg.CPC.base.IV, reg.CPC.base.Party.IV, reg.CPC.IV)

reg.CPC.b <- lm(CPC ~ log(Index_CAN):(Before_2011 + After_2011) + (LPC + NDP):Before_2011 + (LPC + NDP):After_2011 + Before_2011, data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC.b <- cochrane.orcutt(reg.CPC.b)

reg.CPC.b.IV <- ivreg(CPC ~ log(Index_CAN):(Before_2011 + After_2011) + (LPC + NDP):Before_2011 + (LPC + NDP):After_2011 + Before_2011 | log(Index_USA):(Before_2011 + After_2011) + (LPC + NDP):Before_2011 + (LPC + NDP):After_2011 + Before_2011, data = ts.salience.unmelt[ts.salience.unmelt$Before_2015 == 1,])

reg.CPC.b.IV <- cochrane.orcutt(reg.CPC.b.IV)

stargazer(reg.CPC.b, reg.CPC.b.IV)

reg.LPC.base <- lm(LPC ~ log(Index_CAN), data = ts.salience.unmelt[ts.salience.unmelt$After_2015 == 1,])

reg.LPC.base <- cochrane.orcutt(reg.LPC.base)

reg.LPC.base.IV <- ivreg(LPC ~ log(Index_CAN) | log(Index_USA), data = ts.salience.unmelt[ts.salience.unmelt$After_2015 == 1,])

reg.LPC.base.IV <- cochrane.orcutt(reg.LPC.base.IV)

reg.LPC <- lm(LPC ~ log(Index_CAN) + CPC + NDP, data = ts.salience.unmelt[ts.salience.unmelt$After_2015 == 1,])

reg.LPC <- cochrane.orcutt(reg.LPC)

reg.LPC.IV <- ivreg(LPC ~ log(Index_CAN) + CPC + NDP | log(Index_USA) + CPC + NDP, data = ts.salience.unmelt[ts.salience.unmelt$After_2015 == 1,])

reg.LPC.IV <- cochrane.orcutt(reg.LPC.IV)

stargazer(reg.LPC.base, reg.LPC, reg.LPC.base.IV, reg.LPC.IV)

### Causal Estimation of Substitution Effects

Obfuscation <- c()

iv_reg_CPC <- list()

iv_reg_LPC <- list()

iv_reg_NDP <- list()

for (i in (1:ntopics)) {

  iv_reg_CPC[[i]] <- ivreg(paste("`", i, "` ~ log(Index_CAN) | 1 + log(Index_USA)", sep = ""), 
                           data = ts_salience %>% filter(Party == "CPC"))
  
  iv_reg_CPC[[i]] <- cochrane.orcutt(iv_reg_CPC[[i]])
  
  iv_reg_LPC[[i]] <- ivreg(paste("`", i, "` ~ log(Index_CAN) | 1 + log(Index_USA)", sep = ""), data = ts_salience[ts_salience$Party == "LPC",])
  
  iv_reg_LPC[[i]] <- cochrane.orcutt(iv_reg_LPC[[i]])
  
  iv_reg_NDP[[i]] <- ivreg(paste("`", i, "` ~ log(Index_CAN) | 1 + log(Index_USA)", sep = ""), data = ts_salience[ts_salience$Party == "NDP",])
  
  iv_reg_NDP[[i]] <- cochrane.orcutt(iv_reg_NDP[[i]])
  
  Obfuscation <- rbind(Obfuscation, 
                       cbind(rep(i, times = 3), 
                             c("CPC", "LPC", "NDP"), 
                             rbind(summary(iv_reg_CPC[[i]])$coefficients[2, 1:2],
                                   summary(iv_reg_LPC[[i]])$coefficients[2, 1:2],
                                   summary(iv_reg_NDP[[i]])$coefficients[2, 1:2])))
    
}

Obfuscation <- data.frame(Obfuscation)

names(Obfuscation) <- c("Topic", "Party", "Estimate", "Std.Error")

Obfuscation <- Obfuscation %>%
  mutate_at(vars(Topic), as.numeric) %>% 
  arrange(Topic, Party)

rownames(Obfuscation) <- NULL

Obfuscation <- Obfuscation %>%
  mutate_at(vars(Party, Topic), as.factor) %>%
  mutate_at(vars(Estimate, Std.Error), as.numeric)

Custom_Order <- c(9, rev(order(Topic_Labels))[rev(order(Topic_Labels)) != 9])

Obfuscation <- Obfuscation %>% 
  mutate(Topic = ordered(Topic, Custom_Order)) %>%
  arrange(Topic)

### Figure A3

ggplot(data = Obfuscation) +
  geom_pointrange(mapping = aes(y = Topic, x = Estimate, xmin = Estimate - 1.96 * Std.Error, xmax = Estimate + 1.96 * Std.Error, color = Party, shape = Party, linetype = Party, group = Party), orientation = "y", 
                  position = position_dodge(width = 0.5)) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  theme_bw() + 
  labs(y = "Topic") + 
  scale_color_manual(values = c("blue", "red", "orange")) + 
  theme(legend.position = "bottom") +
  labs(x = 'Estimate') +
  scale_y_discrete(labels = Topic_Labels[Custom_Order])

ggsave("Figure 5.pdf", width = 6.5, height = 8)

### Regional Heterogeneity in Issue Responsiveness

Provinces <- c('AB' = 'Alberta',
               'BC' = 'British Columbia',
               'MB' = 'Manitoba',
               'NB' = 'New Brunswick',
               'NL' = 'Newfoundland and Labrador',
               'NS' = 'Nova Scotia',
               'ON' = 'Ontario',
               'PE' = 'Prince Edward Island',
               'QC' = 'Québec',
               'SK' = 'Saskatchewan')

Provincial <- read_excel("data_Provinces.xlsx", sheet = "Final")

base <- Provincial %>%
  filter(Week == as.Date("2006-04-02")) %>%
  select(Canada) %>%
  as.numeric() 

Provincial <- Provincial %>%
  mutate(Week = as.Date(Week)) %>%
  mutate_at(vars(-Week), function(x){round(x / base *100, 0)})

### Figure A4

Provincial %>%
  melt(id = c("Week", "Canada")) %>%
  mutate(variable = factor(variable, 
                           levels = sort(names(Provinces))),
         variable = Provinces[variable]) %>%
  ggplot(mapping = aes(x = value, y = Canada)) + 
  facet_wrap(vars(variable), scales = "free_x") +
  geom_point() + 
  geom_smooth(color = 'gray') +
  stat_cor(aes(label = ..r.label..), 
           size = 3, 
           label.y.npc = "top", 
           label.x.npc = "left",
           cor.coef.name = 'rho') +
  labs(x = "Provincial Level of the Public Salience of Climate Change",
       y = "National Level of the Public Salience of Climate Change") +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 270))

ggsave("Figure A4.pdf", width = 8.5, height = 6.5)

ts_salience <- merge(ts_salience, Provincial, by = "Week") %>%
  mutate_at(vars(c('Canada', names(Provinces))), function(x){x + 10e-9}) %>%
  mutate_at(vars(c('Canada', names(Provinces))), log)

reg.LPC <- lm(`9` ~ Canada, data = ts_salience[ts_salience$Party == "LPC",])

reg.LPC <- cochrane.orcutt(reg.LPC)

reg.CPC <- lm(`9` ~ Canada, data = ts_salience[ts_salience$Party == "CPC",])

reg.CPC <- cochrane.orcutt(reg.CPC)

reg.NDP <- lm(`9` ~ Canada, data = ts_salience[ts_salience$Party == "NDP",])

reg.NDP <- cochrane.orcutt(reg.NDP)

reg.LPC.P <- lm(`9` ~ Canada + AB + BC + MB + NB + NL + NS + ON + PE + QC + SK, data = ts_salience[ts_salience$Party == "LPC",])

reg.LPC.P <- cochrane.orcutt(reg.LPC.P)

reg.CPC.P <- lm(`9` ~ Canada + AB + BC + MB + NB + NL + NS + ON + PE + QC + SK, data = ts_salience[ts_salience$Party == "CPC",])

reg.CPC.P <- cochrane.orcutt(reg.CPC.P)

reg.NDP.P <- lm(`9` ~ Canada + AB + BC + MB + NB + NL + NS + ON + PE + QC + SK, data = ts_salience[ts_salience$Party == "NDP",])

reg.NDP.P <- cochrane.orcutt(reg.NDP.P)

stargazer(reg.CPC, reg.CPC.P, reg.LPC, reg.LPC.P, reg.NDP, reg.NDP.P)

reg.LPC.b.B <- lm(`9` ~ Canada, data = ts_salience[ts_salience$Party == "LPC" & ts_salience$Before_2015 == 1,])

reg.LPC.b.B <- cochrane.orcutt(reg.LPC.b.B)

reg.LPC.b.A <- lm(`9` ~ Canada, data = ts_salience[ts_salience$Party == "LPC" & ts_salience$After_2015 == 1,])

reg.LPC.b.A <- cochrane.orcutt(reg.LPC.b.A)

reg.CPC.b.B <- lm(`9` ~ Canada, data = ts_salience[ts_salience$Party == "CPC" & ts_salience$Before_2015 == 1,])

reg.CPC.b.B <- cochrane.orcutt(reg.CPC.b.B)

reg.CPC.b.A <- lm(`9` ~ Canada, data = ts_salience[ts_salience$Party == "CPC" & ts_salience$After_2015 == 1,])

reg.CPC.b.A <- cochrane.orcutt(reg.CPC.b.A)

reg.LPC.b.B.P <- lm(`9` ~ Canada + AB + BC + MB + NB + NL + NS + ON + PE + QC + SK, data = ts_salience[ts_salience$Party == "LPC" & ts_salience$Before_2015 == 1,])

reg.LPC.b.B.P <- cochrane.orcutt(reg.LPC.b.B.P)

reg.LPC.b.A.P <- lm(`9` ~ Canada + AB + BC + MB + NB + NL + NS + ON + PE + QC + SK, data = ts_salience[ts_salience$Party == "LPC" & ts_salience$After_2015 == 1,])

reg.LPC.b.A.P <- cochrane.orcutt(reg.LPC.b.A.P)

reg.CPC.b.B.P <- lm(`9` ~ Canada + AB + BC + MB + NB + NL + NS + ON + PE + QC + SK, data = ts_salience[ts_salience$Party == "CPC" & ts_salience$Before_2015 == 1,])

reg.CPC.b.B.P <- cochrane.orcutt(reg.CPC.b.B.P)

reg.CPC.b.A.P <- lm(`9` ~ Canada + AB + BC + MB + NB + NL + NS + ON + PE + QC + SK, data = ts_salience[ts_salience$Party == "CPC" & ts_salience$After_2015 == 1,])

reg.CPC.b.A.P <- cochrane.orcutt(reg.CPC.b.A.P)

stargazer(reg.CPC.b.B, reg.CPC.b.B.P, reg.LPC.b.B, reg.LPC.b.B.P)

stargazer(reg.CPC.b.A, reg.CPC.b.A.P, reg.LPC.b.A, reg.LPC.b.A.P)

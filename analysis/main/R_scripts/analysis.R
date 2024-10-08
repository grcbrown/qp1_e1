library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(MuMIn)

setwd("/Users/gracebrown/qp1_exp1/qp1_e1")

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
theme_set(theme_bw())

# LOAD DATA
raw_data <- read.csv('./data/qp1_e1-merged.csv')
survey <- read.csv('./data/survey_post.csv')
data <- left_join(x=raw_data,y=survey,by="workerid")

# DATA SHAPING
## remove participants according to exclusion criteria 
## (1) Audio attention check
exclude_audio <- data %>% filter(trial_type == "audio-button-response") %>% group_by(workerid) %>% filter(response != 2) 
print(exclude_audio$workerid)
data <- data[!(data$workerid %in% c(728, 707, 697, 601, 832, 688, 631, 817, 853, 733, 646, 841, 828, 726, 734, 849, 772, 822, 594)),]
## (2) - responded to less than 85% of trials 
trial_num <- 108 #108 total trials (54 sib + 54 control)
exclude_trial <- data %>% filter(!is.na(rt)) %>% filter(trial_type == "audio-slider-response") %>% group_by(workerid) %>% count(workerid)
exclude2 <- exclude_trial %>% filter(n/trial_num < 0.85)
print(exclude2$workerid)
data <- data[!(data$workerid %in% c(866)),]
total_respondants <- n_distinct(data$workerid)

## separate numeric and string data
data$response_numeric <- ifelse(data$trial_type=="audio-slider-response" | data$trial_type=="html-slider-response", data$response, NA)
data$response_numeric <- as.double(data$response_numeric)

## unpack demographic data
### gender breakdown 
gender_summary <- data %>% select(workerid,gender) %>% group_by(gender) %>% reframe("count" = n_distinct(workerid))
### age breakdown### age breakdowngender_summary
age_summary <- data %>% filter(!is.na(age)==T) %>% summarize("min_age" = min(age), "mean_age" = mean(age), "max_age" = max(age))
### region breakdown
region_summary <- data %>% select(workerid,region) %>% group_by(region) %>% reframe("count" = n_distinct(workerid))
### education breakdown
edu_summary <- data %>% select(workerid,education) %>% group_by(education) %>% reframe("count" = n_distinct(workerid))
### political
political <- data %>% filter(trial_type == "survey-likert") %>% summarize("workerid" = workerid, "political" = response)
political$political <- str_replace(political$political, "\\{'Q0': ", "")
political$political <- str_replace(political$political, "\\}", "")
political$political %>% na_if("''")
data <- left_join(x=data,y=political,by="workerid")
data$political %>% as.integer()

## calculate SQR score and append it to main df 
data$coding[data$coding == ""] <- NA 
data$sqr_raw <- ifelse(data$trial_type == "html-slider-response" & is.na(data$coding) == FALSE,
                       data$response_numeric/100, NA)
data$gender_trans <- ifelse(data$trial_type == "html-slider-response" & is.na(data$coding) == FALSE & data$coding == "NEGATIVE",
                            (10000-data$sqr)/100, NA)
data$gender_link <- ifelse(data$trial_type == "html-slider-response" & is.na(data$coding) == FALSE & data$coding == "POSITIVE",
                           (data$sqr)/100, NA)
score_gender_link <- data %>% filter(!is.na(gender_link)) %>% group_by(workerid) %>% summarize("score_link" = mean(gender_link))
data <- merge(data, score_gender_link, by = "workerid", all.x = TRUE)
score_gender_trans <- data %>% filter(!is.na(gender_trans)) %>% group_by(workerid) %>% summarize("score_trans" = mean(gender_trans))
data <- merge(data, score_gender_trans, by = "workerid", all.x = TRUE)
srq_score <- data %>% group_by(workerid) %>% summarize("score" = mean(score_trans+score_link))

## relable spk
data$spk[data$spk == 246] = "non-masc"
data$spk[data$spk == 723] = "masc"
data$spk[data$spk == 340] = "neut"

### summarize numeric data 
exp_data <- data %>% 
  filter(!is.na(response)) %>% 
  filter(trial_type == "audio-slider-response") %>%
  group_by(spk) %>% 
  filter(spk == "non-masc" | spk == "neut" | spk == "masc")

exp_data <- filter(exp_data, is.na(response_numeric)==FALSE)

exp_data$sib_code <- factor(exp_data$sib_code, levels = c("control", "low", "mid", "high"))

#### by participant 
by_participant <- exp_data %>% group_by(workerid) %>% summarize("min" = min(response_numeric), "mean" = mean(response_numeric), "max" = max(response_numeric))
by_participant_spk <- exp_data %>% group_by(workerid, spk) %>% summarize("min" = min(response_numeric), "mean" = mean(response_numeric), "max" = max(response_numeric))

#### by spk
exp_sub_1 <- subset(exp_data, select = -c(trial_index,workerid)) 
exp_summary_1 <- summarize(exp_sub_1, "mean"=mean(response_numeric/100), "var" = var(response_numeric/100), "sd" = sd(response_numeric/100))
print(exp_summary_1)

#### by spk + sib 
exp_sub_2 <- subset(exp_data, select = -c(trial_index,triplet_id)) %>% group_by(sib_code, spk)
exp_summary_2 <- summarize(exp_sub_2, "mean"=mean(response_numeric/100), "var" = var(response_numeric/100), "sd" = sd(response_numeric/100))
print(exp_summary_2)

# VISUALIZATIONS 
## gen distribution of ratings 
hist_all <- ggplot(exp_data, aes(x=response_numeric/100)) + 
  geom_histogram(bins=30) +
  xlab("Masculinity Rating") + 
  scale_fill_manual(values = cbPalette) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(hist_all)
ggsave(file="./analysis/main/Graphs/hist_all.pdf",width=7,height=4)
ggsave(file="./analysis/main/Graphs/hist_all.png",width=,height=4)
## overall distribution of ratings by speaker
hist_by_spk <- ggplot(exp_data,aes(x=response_numeric/100))+
  geom_histogram(bins=30)+
  facet_grid(.~spk) +
  xlab("Masculinity Rating") +
  scale_fill_manual(values = cbPalette, name = "Speaker ID")
print(hist_by_spk)
ggsave(file="./analysis/main/Graphs/hist_spk.pdf",width=7,height=4)
ggsave(file="./analysis/main/Graphs/hist_spk.png",width=,height=4)
## overall distribution of ratings by sib_code
hist_by_sib <- ggplot(exp_data,aes(x=response_numeric/100))+
  geom_histogram(bins=30)+
  facet_grid(.~sib_code) +
  xlab("Masculinity Rating")
print(hist_by_sib)
ggsave(file="./analysis/main/Graphs/hist_sib.pdf",width=7,height=4)
ggsave(file="./analysis/main/Graphs/hist_sib.png",width=,height=4)
## speaker by sib_code
hist_by_spk_sib <- ggplot(exp_data,aes(x=response_numeric/100))+
  geom_histogram(bins=20)+
  facet_grid(sib_code~spk) +
  xlab("Masculinity Rating")
print(hist_by_spk_sib)
ggsave(file="./analysis/main/Graphs/hist_spk_sib.pdf",width=7,height=4)
ggsave(file="./analysis/main/Graphs/hist_spk_sib.png",width=,height=4)
## faceted by-triplet average ratings 
hist_by_trip <- ggplot(exp_data,aes(x=response_numeric/100))+
  geom_histogram(bins=30)+
  facet_grid(.~triplet_id) + 
  xlab("Masculinity Rating")
print(hist_by_trip) 

##barplots
spk_bar <- exp_data %>%
  group_by(spk) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric/100),
    sd=sd(response_numeric/100)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

bar_spk <- ggplot(spk_bar, aes(y = mean)) +
  geom_bar(aes(x = spk, fill = spk), stat="identity", alpha=0.7) +
  geom_errorbar(aes(x = spk, 
                    ymin=mean-ic, 
                    ymax=mean+ic), width=0.4, colour="black", alpha=0.9) + 
  ylab("Masculinity Rating") +
  xlab("Speaker ID") +
  scale_fill_manual(values = cbPalette)
print(bar_spk)
ggsave(file="./analysis/main/Graphs/bar_spk.pdf",width=4,height=4)
ggsave(file="./analysis/main/Graphs/bar_spk.png",width=4,height=4)
### by lexical_triplet
exp_data_1 <- filter(exp_data, triplet_id < 8) %>%
  group_by(triplet_id,spk) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric/100),
    sd=sd(response_numeric/100)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
exp_data_2 <- filter(exp_data, triplet_id > 7 & triplet_id < 16)  %>%
  group_by(triplet_id,spk) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric/100),
    sd=sd(response_numeric/100)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
exp_data_3 <- filter(exp_data, triplet_id > 15)  %>%
  group_by(triplet_id,spk) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric/100),
    sd=sd(response_numeric/100)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

exp_data_1$triplet_id <- as.factor(exp_data_1$triplet_id)
exp_data_2$triplet_id <- as.factor(exp_data_2$triplet_id)
exp_data_3$triplet_id <- as.factor(exp_data_3$triplet_id)

bar_1 <- ggplot(exp_data_1, aes(x = triplet_id, y = mean, fill = spk)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Sentence Frame") +
  scale_fill_manual(values = cbPalette, name = "Speaker")
print(bar_1)
ggsave(file="./analysis/main/Graphs/bar_1.pdf",width=8,height=5)
ggsave(file="./analysis/main/Graphs/bar_1.png",width=8,height=5)

bar_2 <- ggplot(exp_data_2, aes(x = triplet_id, y = mean, fill = spk)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Sentence Frame") +
  scale_fill_manual(values = cbPalette, name = "Speaker")
print(bar_2)
ggsave(file="./analysis/main/Graphs/bar_2.pdf",width=8,height=5)
ggsave(file="./analysis/main/Graphs/bar_2.png",width=8,height=5)

bar_3 <- ggplot(exp_data_3, aes(x = triplet_id, y = mean, fill = spk)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Sentence Frame") +
  scale_fill_manual(values = cbPalette, name = "Speaker")
print(bar_3)
ggsave(file="./analysis/main/Graphs/bar_3.pdf",width=8,height=5)
ggsave(file="./analysis/main/Graphs/bar_3.png",width=8,height=5)

## by sib
sib_bar <- exp_data %>%
  group_by(sib_code) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric/100),
    sd=sd(response_numeric/100)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

bar_sib <- ggplot(sib_bar, aes(y = mean)) +
  geom_bar(aes(x = sib_code, fill = sib_code), stat="identity", alpha=0.7) +
  geom_errorbar(aes(x = sib_code, 
                    ymin=mean-ic, 
                    ymax=mean+ic), width=0.4, colour="black", alpha=0.9) + 
  ylab("Masculinity Rating") +
  xlab("Sibilant Condition") +
  scale_fill_manual(values = cbPalette)
print(bar_sib)
ggsave(file="./analysis/main/Graphs/bar_sib.pdf",width=4,height=4)
ggsave(file="./analysis/main/Graphs/bar_sib.png",width=4,height=4)

## speaker * sib
exp_data_spk_sib <- exp_data %>%
  group_by(sib_code,spk) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric/100),
    sd=sd(response_numeric/100)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

bar_spk_sib <- ggplot(exp_data_spk_sib, aes(x = sib_code, y = mean, fill = spk)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Sibilant Condition") +
  scale_fill_manual(values = cbPalette, name = "Speaker")
print(bar_spk_sib)
ggsave(file="./analysis/main/Graphs/bar_spk_sib.pdf",width=8,height=5)
ggsave(file="./analysis/main/Graphs/bar_spk_sib.png",width=8,height=5)

bar_sib_spk <- ggplot(exp_data_spk_sib, aes(x = spk, y = mean, fill = sib_code)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Speaker") +
  scale_fill_manual(values = cbPalette, name = "Sibilant Condition")
print(bar_sib_spk)
ggsave(file="./analysis/main/Graphs/bar_sib_spk.pdf",width=8,height=5)
ggsave(file="./analysis/main/Graphs/bar_sib_spk.png",width=8,height=5)

box_sib_spk <- ggplot(exp_data,aes(x = spk, y = response_numeric/100))+
  geom_boxplot(aes(fill=sib_code))+ 
  ylab("Masculinity Rating") +
  scale_fill_manual("Sibilant Condition", values=cbPalette) 
print(box_sib_spk)
ggsave(file="./analysis/main/Graphs/box_sib_spk.pdf",width=6,height=4)
ggsave(file="./analysis/main/Graphs/box_sib_spk.png",width=6,height=4)

violin_sib_spk <- ggplot(exp_data, aes(x = spk, y = response_numeric/100))+
  geom_violin(aes(fill=sib_code))+
  ylab("Masculinity Rating")+
  scale_fill_manual("Sibilant Condition", values = cbPalette) 
print(violin_sib_spk)


### SQR 
gender_link <- ggplot(score_gender_link,aes(score_link))+geom_histogram(bins = 30)
print(gender_link)
gender_trans <- ggplot(score_gender_trans,aes(score_trans))+geom_histogram(bins = 30)
print(gender_trans)

# MODEL 
## linear mixed effects model
exp_data$triplet_id <- as.factor(exp_data$triplet_id)
exp_data$spk <- as.factor(exp_data$spk)
exp_data$spk <- relevel(exp_data$spk, ref = "neut")
exp_data$workerid <- as.factor(exp_data$workerid)
exp_data$score_link <- as.numeric(exp_data$score_link)
exp_data$score_trans <- as.numeric(exp_data$score_trans)
exp_data$sib_code <- relevel(exp_data$sib_code, ref = "control")
exp_data$political <- as.factor(exp_data$political)
exp_data$political <- relevel(exp_data$political, ref = "2")
exp_data$response_numeric <- exp_data$response_numeric/100

#maximally descriptive model
model_all <- lmer(response_numeric ~ sib_code*spk + age + gender + region + education + score_trans + political + (1|workerid), data = exp_data, REML = F)
summary(model_all) 

r.squaredGLMM(model_all)

model_2 <- lmer(response_numeric ~ sib_code*spk + age + gender + region + score_trans + political + (1+spk|workerid), data = exp_data)
summary(model_2) # failed to converge 

model_3 <- lmer(response_numeric ~ sib_code*spk + age + gender + score_trans + political + (1+spk|workerid) + (1+proliferate.condition), data = exp_data)
summary(model_3) # failed to converge 

model_4 <- lmer(response_numeric ~ sib_code*spk + age + gender + score_trans + (1+spk|workerid) + (1+proliferate.condition), data = exp_data)
summary(model_4) # failed to converge 

#planned analysis
model_planned <- lmer(response_numeric ~ sib_code*spk + score_trans + age + (1|workerid), data = exp_data)
summary(model_planned)

r.squaredGLMM(model_planned)


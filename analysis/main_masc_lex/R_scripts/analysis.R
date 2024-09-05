library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(MuMIn)

setwd("your_wd_here")

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
theme_set(theme_bw())

# LOAD DATA
lex <- read.csv('./data/masc_lex-merged.csv')

# DATA SHAPING
## remove participants according to exclusion criteria 
accuracy_by_participant <- lex %>% filter(!is.na(correct)) %>% group_by(participant_id,condition) %>% filter(accuracy == "correct") %>% count(accuracy)
exclude <- filter(accuracy_by_participant, n<6)
print(exclude$participant_id)
lex <- lex[!(lex$participant_id %in% c(9,13,14,25,32,40,44,52,55,58,62,67,70,72,78,95,97,110,111,113,126,130,133,145,150,151,158,159)),]

## separate numeric and string data
lex$response_numeric <- ifelse(lex$trial_type=="html-vas-response", lex$response, NA)
lex$response_numeric <- as.double(lex$response_numeric)

## unpack demographic data
lex$response_survey <- ifelse(lex$trial_type=="survey", lex$response, NA)
survey_results <- lex %>% filter(!is.na(response_survey) == TRUE) %>% group_by(participant_id)
#lex$response_survey <- gsub("'", '"', lex$response_survey)
#demo <- lex %>% filter(!is.na(response_survey))
#json_data <- fromJSON(demo$response_survey)
## error with json - invalid char in json text ("P0_Q0": None,) <- need to remove this 

lex$response_political <- ifelse(lex$trial_type == "survey-likert", lex$response, NA)

## calculate SQR score and append it to main df 
lex$coding[lex$coding == ""] <- NA 
lex$sqr <- ifelse(lex$trial_type == "html-vas-response" & is.na(lex$coding) == FALSE,
                       lex$response_numeric, NA)
lex$gender_trans <- ifelse(lex$trial_type == "html-vas-response" & is.na(lex$coding) == FALSE & lex$coding == "NEGATIVE",
                           1-lex$sqr, NA)
lex$gender_link <- ifelse(lex$trial_type == "html-vas-response" & is.na(lex$coding) == FALSE & lex$coding == "POSITIVE",
                          lex$sqr, NA)
score_gender_link <- lex %>% filter(!is.na(gender_link)) %>% group_by(participant_id) %>% summarize("score_link" = mean(gender_link))
lex <- merge(lex, score_gender_link, by = "participant_id", all.x = TRUE)
score_gender_trans <- lex %>% filter(!is.na(gender_trans)) %>% group_by(participant_id) %>% summarize("score_trans" = mean(gender_trans))
lex <- merge(lex, score_gender_trans, by = "participant_id", all.x = TRUE)

### summarize numeric data 
exp_data <- lex %>% 
  filter(!is.na(response)) %>% 
  filter(!is.na(triplet_id)) %>%
  group_by(triplet_id,expected,participant_id,trial_index,condition) %>% 
  summarize(response_numeric,score_link,score_trans)

exp_data <- filter(exp_data, is.na(response_numeric)==FALSE)
#### by expected + triplet_id
exp_sub_1 <- subset(exp_data, select = -c(trial_index,participant_id)) 
exp_summary_1 <- summarize(exp_sub_1, "mean"=mean(response_numeric), "var" = var(response_numeric))
print(exp_summary_1)

#### by expected + participant 
exp_sub_2 <- subset(exp_data, select = -c(trial_index,triplet_id)) 
exp_summary_2 <- summarize(exp_sub_2, "mean"=mean(response_numeric), "var" = var(response_numeric))
print(exp_summary_2)

# VISUALIZATIONS 
## overall distribution of ratings 
hist_by_cond <- ggplot(exp_data,aes(x=response_numeric, fill = expected))+
  geom_histogram(bins=30)+
  facet_grid(.~expected) +
  xlab("Masculinity Rating") +
  scale_fill_manual(values = cbPalette)
print(hist_by_cond)
ggsave(file="./analysis/main_masc_lex/Graphs/hist_by_condition.pdf",width=7,height=4)
ggsave(file="./analysis/main_masc_lex/Graphs/hist_by_condition.png",width=,height=4)
## faceted by-triplet average ratings 
hist_by_triplet <- ggplot(exp_data,aes(x=response_numeric))+
  geom_histogram(bins=30)+
  facet_grid(.~triplet_id)
print(hist_by_triplet) 

##barplots

box_all <- ggplot(exp_data,aes(y = response_numeric))+
  geom_boxplot(aes(fill=expected))+ 
  ylab("Masculinity Rating") +
  scale_fill_manual("Expected Masculinity", values=cbPalette) 
print(box_all)
ggsave(file="./analysis/main_masc_lex/Graphs/box_all.pdf",width=6,height=4)
ggsave(file="./analysis/main_masc_lex/Graphs/box_all.png",width=6,height=4)

exp_data_bar <- exp_data %>%
  group_by(expected) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric),
    sd=sd(response_numeric)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

bar_all <- ggplot(exp_data_bar, aes(y = mean)) +
  geom_bar(aes(x = expected, fill = expected), stat="identity", alpha=0.7) +
  geom_errorbar(aes(x = expected, 
                    ymin=mean-ic, 
                    ymax=mean+ic), width=0.4, colour="black", alpha=0.9) + 
  ylab("Masculinity Rating") +
  xlab("Lexical Variant Type") +
  scale_fill_manual(values = cbPalette)
print(bar_all)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_all.pdf",width=4,height=4)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_all.png",width=4,height=4)
### by lexical_triplet
exp_data_1 <- filter(exp_data, triplet_id < 9) %>%
  group_by(triplet_id,expected) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric),
    sd=sd(response_numeric)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
exp_data_2 <- filter(exp_data, triplet_id > 8 & triplet_id < 17)  %>%
  group_by(triplet_id,expected) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric),
    sd=sd(response_numeric)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
exp_data_3 <- filter(exp_data, triplet_id > 16)  %>%
  group_by(triplet_id,expected) %>%
  summarise( 
    n=n(),
    mean=mean(response_numeric),
    sd=sd(response_numeric)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

exp_data_1$triplet_id <- as.factor(exp_data_1$triplet_id)
exp_data_2$triplet_id <- as.factor(exp_data_2$triplet_id)
exp_data_3$triplet_id <- as.factor(exp_data_3$triplet_id)

bar_1 <- ggplot(exp_data_1, aes(x = triplet_id, y = mean, fill = expected)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Sentence Frame") +
  ylim(0, 1) + 
  scale_fill_manual(values = cbPalette, name = "Lexical Variant Type")
print(bar_1)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_1.pdf",width=8,height=5)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_1.png",width=8,height=5)

bar_2 <- ggplot(exp_data_2, aes(x = triplet_id, y = mean, fill = expected)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Sentence Frame") +
  ylim(0, 1) + 
  scale_fill_manual(values = cbPalette, name = "Lexical Variant Type")
print(bar_2)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_2.pdf",width=8,height=5)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_2.png",width=8,height=5)

bar_3 <- ggplot(exp_data_3, aes(x = triplet_id, y = mean, fill = expected)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(width = .9),
               size = 3) +
  geom_errorbar(aes(ymin=mean-ic, 
                    ymax=mean+ic), width=0.3, position = position_dodge(width = .9)) + 
  ylab("Masculinity Rating") +
  xlab("Sentence Frame") +
  ylim(0, 1) + 
  scale_fill_manual(values = cbPalette, name = "Lexical Variant Type")
print(bar_3)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_3.pdf",width=8,height=5)
ggsave(file="./analysis/main_masc_lex/Graphs/bar_3.png",width=8,height=5)

### SQR 
gender_link <- ggplot(score_gender_link,aes(score_link))+geom_histogram(bins = 30)
print(gender_link)
gender_trans <- ggplot(score_gender_trans,aes(score_trans))+geom_histogram(bins = 30)
print(gender_trans)

# MODEL 
## linear mixed effects model
exp_data$triplet_id <- as.factor(exp_data$triplet_id)
exp_data$expected <- as.factor(exp_data$expected)
exp_data$participant_id <- as.factor(exp_data$participant_id)
exp_data$trial_index <- as.factor(exp_data$trial_index)
exp_data$score_link <- as.numeric(exp_data$score_link)
exp_data$score_trans <- as.numeric(exp_data$score_trans)

exp_data$expected <- relevel(exp_data$expected, ref = "neutral")
model_all <- lmer(response_numeric ~ expected + (1+expected|participant_id) + (1+expected|trial_index), data = exp_data)
summary(model_all)

r.squaredGLMM(model_all)

## interaction between expected (masc condition) and triplet_id
exp_data$triplet_id <- relevel(exp_data$triplet_id, ref = "18")
model_trip <- lmer(response_numeric ~ expected*triplet_id + (1+expected|participant_id) + (1+expected|trial_index), data = exp_data)
summary(model_trip)

r.squaredGLMM(model_trip)

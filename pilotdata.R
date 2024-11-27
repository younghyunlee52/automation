
###############################
## Calling data
###############################

library(tidyverse)

read_Qualtrics <- function(file) {
  temp <- read.csv(file)
  qualtrics <- temp[3:nrow(temp), ] %>%
    type_convert() 
  return(qualtrics)
}

back <- read_Qualtrics("/Users/hyoon/Desktop/Yoon2/automation/experiment/automation/Subject Pool - Fall 2024 - Background Survey_November 4, 2024_16.16.csv") %>%
  select(7:ncol(.))
exp <- read_Qualtrics("/Users/hyoon/Desktop/Yoon2/automation/experiment/automation/Subject Pool - Fall 2024 - wave 1 - Lee_November 4, 2024_16.22.csv") %>%
  filter(Finished == "TRUE") %>% select(id, experiment1, contains("Q"))

###############################


###############################
## Recoding
###############################

data <- left_join(exp, back,
                  by = "id") %>% 
  filter(experiment1 %in% c("treat_anti", "treat_pro") & Q1 == "immigration" |
           experiment1 == "control" & Q1 == "art") %>%
  select(id, experiment1, contains("Q"),
         bgage, bggender, bgincome, bgcitizen, bgethnicity,
         vote_2022, vote_2024, contains("polpar"),
         libcon, partyid_D.R, mdfollowpol, govtrust, govwaste, govcorrupt, gov1,
         contains("affpol"), contains("ethno")) %>%
  select(-c(Q1, Q2, Q3, Q4)) %>%
  mutate(
    Q5 = rowMeans(across(starts_with("Q5")), na.rm = TRUE),
    Q6 = rowMeans(across(starts_with("Q6")), na.rm = TRUE),
    Q7 = rowMeans(across(starts_with("Q7")), na.rm = TRUE),
    Q8 = rowMeans(across(starts_with("Q8")), na.rm = TRUE),
    Q9 = rowMeans(across(starts_with("Q9")), na.rm = TRUE),
    Q10 = rowMeans(across(starts_with("Q10")), na.rm = TRUE),
    Q17 = rowMeans(across(starts_with("Q17")), na.rm = TRUE),
    Q18 = rowMeans(across(starts_with("Q18")), na.rm = TRUE)) %>%
  mutate(
    experiment_bi = case_when(experiment1 == "treat_pro" ~ 1,
                    experiment1 == "treat_anti" ~ 0,
                    TRUE ~ NA_real_)) %>%
  mutate(
    bgmale = case_when(bggender == "Male" ~ 1,
                       bggender == "Female" ~ 0,
                       TRUE ~ NA),
    bgincome_re = case_when(bgincome == "$0-$24,999" ~ 1,
                               bgincome == "$25,000-$49,999" ~ 2,
                               bgincome == "$50,000-$74,999" ~ 3,
                               bgincome == "$75,000-$99,999" ~ 4,
                               bgincome == "$100,000 - $124,999" ~ 5,
                               bgincome == "$125,000-$149,999" ~ 6,
                               bgincome == "$150,000-$174,999" ~ 7,
                               bgincome == "$175,000-$199,999" ~ 8,
                               bgincome == "Over $200,000" ~ 9,
                               TRUE ~ NA_integer_),
    bgcitizen_bi = case_when(bgcitizen == "Yes" ~ 1,
                             bgcitizen == "No" ~ 0,
                             TRUE ~ NA_integer_),
    bgethn = case_when(bgethnicity == "African American/Black" ~ "Black",
                       bgethnicity == "Asian" ~ "Asian",
                       bgethnicity == "Caucasian/White" ~ "White",
                       bgethnicity == "Native American/ Alaska Native" ~ "Native American",
                       bgethnicity %in% c("African American/Black,Caucasian/White", "African American/Black,Other (please specify)",
                       "Asian,Caucasian/White", "Asian,Other (please specify)", "Caucasian/White,Other (please specify)",
                       "Native American/ Alaska Native,Caucasian/White", "Native American/ Alaska Native,Other (please specify)") ~ "Mixed",
                       TRUE ~ NA), # no hispanic 
    Q15 = case_when(Q15 == "increased" ~ 1,
                    Q15 == "decreased" ~ 2,
                    Q15 == "kept the same" ~ 3),
    bgvote = case_when(vote_2022 == "No" ~ 0,
                       vote_2022 == "Yes" ~ 1),
    bgideo = case_when(libcon == "Very Conservative" ~ 1,
                       libcon == "Conservative" ~ 2,
                       libcon == "Somewhat right of center" ~ 3,
                       libcon == "Centrist, middle of the road" ~ 4,
                       libcon == "Somewhat left of center" ~ 5,
                       libcon == "Liberal" ~ 6,
                       libcon == "Very Liberal" ~ 7),
    bgrep = case_when(partyid_D.R == "Republican" ~ 1,
                      partyid_D.R == "Democrat" ~ 0,
                      TRUE ~ NA),
    bggov = case_when(govtrust == "Almost never" ~ 1,
                      govtrust == "Only some of the time" ~ 2,
                      govtrust == "Most of the time" ~ 3,
                      govtrust == "Just about always" ~ 4,
                      TRUE ~ NA)) 

data$experiment1 <- factor(data$experiment1)
data$experiment1 <- relevel(data$experiment1, ref = "control")

data$bgethn <- factor(data$bgethn)
data$bgethn <- relevel(data$bgethn, ref = "White")

# starts with
# Q5-Q10: automation
# Q11: outcomes
# Q12-Q19: immigration

# ends with
# 34: manufacturing
# 35: agriculture
# 36: construction
# 37: healthcare

###############################



###############################
## Covariate balance
###############################

library(kableExtra)

data_balance <- data %>% 
  mutate(n_rows = n()) %>% 
  group_by(experiment1) %>% 
  summarise(sample = n(),
            n_rows = mean(n_rows),
            sample_percent = sample/n_rows*100,
            mean_gender = mean(bgmale,  na.rm = TRUE), 
            mean_age = mean(bgage,  na.rm = TRUE),
            mean_income = mean(bgincome_re, na.rm = TRUE),
            mean_ideo = mean(bgideo, na.rm = TRUE),
            mean_vote = mean(bgvote, na.rm = T),
            mean_ftwhite = mean(ethnoscale_1, na.rm = T),
            mean_ftblack = mean(ethnoscale_2, na.rm = T),
            mean_ftlatino = mean(ethnoscale_3, na.rm = T),
            mean_ftasian = mean(ethnoscale_4, na.rm = T),
            mean_ftarabs = mean(ethnoscale_5, na.rm = T)) %>% 
  select(-n_rows) %>%
  mutate_if(is.numeric, round, 2)

kable(data_balance,  
      caption = 'Balance statistics', 
      format = "html") %>%
  kable_styling(full_width = T, font_size = 12) 

###############################


###############################
## Descriptive stats
###############################

# by question
calculate_summary <- function(question) {
  data %>%
    group_by(experiment1) %>%
    summarize(
      across(starts_with(question),
             list(mean = ~mean(. , na.rm = TRUE),
                  sd = ~sd(., na.rm = TRUE)),
             .names = "{col}_{fn}")) %>% as.data.frame()}

q5 <- calculate_summary("Q5")
q6 <- calculate_summary("Q6")
q7 <- calculate_summary("Q7")
q8 <- calculate_summary("Q8")
q9 <- calculate_summary("Q9")
q10 <- calculate_summary("Q10")

# by industry
calculate_summary_ind <- function(ind_code) {
  data %>%
    group_by(experiment1) %>%
    summarize(
      across(ends_with(ind_code),
             list(mean = ~mean(. , na.rm = TRUE),
                  sd = ~sd(., na.rm = TRUE)),
             .names = "{col}_{fn}")) %>% as.data.frame()}

manf <- calculate_summary_ind("34")
agri <- calculate_summary_ind("35")
const <- calculate_summary_ind("36")
health <- calculate_summary_ind("37")
serv <- calculate_summary_ind("38")


## checking for demographic tendencies

run_models <- function(data, dvs, iv) {
  model_summaries <- lapply(dvs, function(dv) {
    model <- lm(as.formula(paste(dv, "~", iv)), data = data)
    summary(model)
  })
  return(model_summaries)
}

# by gender
data %>% drop_na(bgmale) %>% group_by(bgmale) %>% 
  summarize(mean_q5 = mean(Q5, na.rm=T),
            mean_q6 = mean(Q6, na.rm=T),
            mean_q7 = mean(Q7, na.rm=T),
            mean_q8 = mean(Q8, na.rm=T),
            mean_q9 = mean(Q9, na.rm=T),
            mean_q10 = mean(Q10, na.rm=T))

run_models(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "bgmale")

# by income
data %>% drop_na(bgincome_re) %>% group_by(bgincome_re) %>% 
  summarize(mean_q5 = mean(Q5, na.rm=T),
            mean_q6 = mean(Q6, na.rm=T),
            mean_q7 = mean(Q7, na.rm=T),
            mean_q8 = mean(Q8, na.rm=T),
            mean_q9 = mean(Q9, na.rm=T),
            mean_q10 = mean(Q10, na.rm=T))

run_models(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "bgincome_re")

# by ideology
data %>% drop_na(bgideo) %>% group_by(bgideo) %>% 
  summarize(mean_q5 = mean(Q5, na.rm=T),
            mean_q6 = mean(Q6, na.rm=T),
            mean_q7 = mean(Q7, na.rm=T),
            mean_q8 = mean(Q8, na.rm=T),
            mean_q9 = mean(Q9, na.rm=T),
            mean_q10 = mean(Q10, na.rm=T))

run_models(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "bgideo")

# by citizenship
data %>% drop_na(bgcitizen_bi) %>% group_by(bgcitizen_bi) %>% 
  summarize(mean_q5 = mean(Q5, na.rm=T),
            mean_q6 = mean(Q6, na.rm=T),
            mean_q7 = mean(Q7, na.rm=T),
            mean_q8 = mean(Q8, na.rm=T),
            mean_q9 = mean(Q9, na.rm=T),
            mean_q10 = mean(Q10, na.rm=T))

run_models(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "bgcitizen_bi")

# by ethnicity
data %>% drop_na(bgethn) %>% group_by(bgethn) %>% 
  summarize(mean_q5 = mean(Q5, na.rm=T),
            mean_q6 = mean(Q6, na.rm=T),
            mean_q7 = mean(Q7, na.rm=T),
            mean_q8 = mean(Q8, na.rm=T),
            mean_q9 = mean(Q9, na.rm=T),
            mean_q10 = mean(Q10, na.rm=T))

run_models(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "bgethn")

# by voting record
data %>% drop_na(bgvote) %>% group_by(bgvote) %>% 
  summarize(mean_q5 = mean(Q5, na.rm=T),
            mean_q6 = mean(Q6, na.rm=T),
            mean_q7 = mean(Q7, na.rm=T),
            mean_q8 = mean(Q8, na.rm=T),
            mean_q9 = mean(Q9, na.rm=T),
            mean_q10 = mean(Q10, na.rm=T))

run_models(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "bgvote")

# by gov trust
data %>% drop_na(bggov) %>% group_by(bggov) %>% 
  summarize(mean_q5 = mean(Q5, na.rm=T),
            mean_q6 = mean(Q6, na.rm=T),
            mean_q7 = mean(Q7, na.rm=T),
            mean_q8 = mean(Q8, na.rm=T),
            mean_q9 = mean(Q9, na.rm=T),
            mean_q10 = mean(Q10, na.rm=T))

run_models(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), "bggov")

###############################


###############################
## Main analysis
###############################

library(broom)

create_coef_plot <- function(data, vars, title = "Coefficient Plot") {
  coef_df <- data.frame()
  
    for (var in vars) {
    model <- lm(as.formula(paste(var, "~ experiment1")), data = data)
    coef_df <- bind_rows(coef_df, tidy(model) %>% mutate(variable = var))
  }
  
  coef_df <- coef_df %>%
    filter(term != "(Intercept)") %>%
    mutate(term = case_when(
      term == "experiment1treat_anti" ~ "Anti",
      term == "experiment1treat_pro" ~ "Pro",
      TRUE ~ term))
  
  ggplot(coef_df, aes(x = term, y = estimate, color = variable)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  position = position_dodge(width = 0.5), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    labs(title = title,
      x = "", y = "", color = "") +
    theme_minimal()
}

# pooled
create_coef_plot(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), title = "Pooled by Question")

# by question
create_coef_plot(data, c("Q5_34", "Q5_35", "Q5_36", "Q5_37", "Q5_38"), title = "Q5")
create_coef_plot(data, c("Q6_34", "Q6_35", "Q6_36", "Q6_37", "Q6_38"), title = "Q6")
create_coef_plot(data, c("Q7_34", "Q7_35", "Q7_36", "Q7_37", "Q7_38"), title = "Q7")
create_coef_plot(data, c("Q8_34", "Q8_35", "Q8_36", "Q8_37", "Q8_38"), title = "Q8")
create_coef_plot(data, c("Q9_34", "Q9_35", "Q9_36", "Q9_37", "Q9_38"), title = "Q9")
create_coef_plot(data, c("Q10_34", "Q10_35", "Q10_36", "Q10_37", "Q10_38"), title = "Q10")

# by industry
create_coef_plot(data, c("Q5_34", "Q6_34", "Q7_34", "Q8_34", "Q9_34", "Q10_34"), title = "Manufacturing")
create_coef_plot(data, c("Q5_35", "Q6_35", "Q7_35", "Q8_35", "Q9_35", "Q10_35"), title = "Agriculture")
create_coef_plot(data, c("Q5_36", "Q6_36", "Q7_36", "Q8_36", "Q9_36", "Q10_36"), title = "Construction")
create_coef_plot(data, c("Q5_37", "Q6_37", "Q7_37", "Q8_37", "Q9_37", "Q10_37"), title = "Healthcare")
create_coef_plot(data, c("Q5_38", "Q6_38", "Q7_38", "Q8_38", "Q9_38", "Q10_38"), title = "Service")

# using Q19_9
ggplot(data, aes(x = Q19_9, color = experiment1)) + 
  geom_density(alpha = 0.4, size = 1) 

create_coef_plot2 <- function(data, vars, title = "Coefficient Plot") {
  coef_df <- data.frame() 
  
  for (var in vars) {
    model <- lm(as.formula(paste(var, "~ Q19_9")), data = data)
    coef_df <- bind_rows(coef_df, tidy(model) %>% mutate(variable = var))
  }
  
  coef_df <- coef_df %>%
    filter(term != "(Intercept)")
  
  ggplot(coef_df, aes(x = term, y = estimate, color = variable)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  position = position_dodge(width = 0.5), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    labs(title = title,
         x = "", y = "", color = "") +
    theme_minimal()
}

# iv=q19_9
create_coef_plot2(data, c("Q5", "Q6", "Q7", "Q8", "Q9", "Q10"), title = "pooled by question (q19)")

create_coef_plot2(data, c("Q5_34", "Q5_35", "Q5_36", "Q5_37", "Q5_38"), title = "Q5")
create_coef_plot2(data, c("Q6_34", "Q6_35", "Q6_36", "Q6_37", "Q6_38"), title = "Q6")
create_coef_plot2(data, c("Q7_34", "Q7_35", "Q7_36", "Q7_37", "Q7_38"), title = "Q7")
create_coef_plot2(data, c("Q8_34", "Q8_35", "Q8_36", "Q8_37", "Q8_38"), title = "Q8")
create_coef_plot2(data, c("Q9_34", "Q9_35", "Q9_36", "Q9_37", "Q9_38"), title = "Q9")
create_coef_plot2(data, c("Q10_34", "Q10_35", "Q10_36", "Q10_37", "Q10_38"), title = "Q10")

create_coef_plot2(data, c("Q5_34", "Q6_34", "Q7_34", "Q8_34", "Q9_34", "Q10_34"), title = "Manufacturing")
create_coef_plot2(data, c("Q5_35", "Q6_35", "Q7_35", "Q8_35", "Q9_35", "Q10_35"), title = "Agriculture")
create_coef_plot2(data, c("Q5_36", "Q6_36", "Q7_36", "Q8_36", "Q9_36", "Q10_36"), title = "Construction")
create_coef_plot2(data, c("Q5_37", "Q6_37", "Q7_37", "Q8_37", "Q9_37", "Q10_37"), title = "Healthcare")
create_coef_plot2(data, c("Q5_38", "Q6_38", "Q7_38", "Q8_38", "Q9_38", "Q10_38"), title = "Service")

###############################



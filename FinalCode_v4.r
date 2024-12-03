library(haven)
library(dplyr)
library(lavaan)
library(ggplot2)
library(stargazer)
library(survey)
library(lavaan.survey)

# 1. data clean
# import data
CGSS2021 = read_dta("CGSS2021.dta")

# select variables and rename
# consider split job insecurity into objective and subjective well-being: 
CGSS = CGSS2021 %>% 
  select(id, A53aa, A59b, A59g, A17, A36, L17, L11_c, L16_a, A45, A58a, A16, A2, A3_1, A18, A10, A7a, A62,
         provinces, community_i, weight, weight_raking) %>% 
  rename(
    workhours = A53aa,       # Weekly work hours
    workcontract = A59b,     # Employment contract type
    workself = A59g,         # Job autonomy
    depressed = A17,         # Depression feelings
    happy = A36,             # Happiness level  
    satisfaction = L17,      # job satisfaction
    workstress = L11_c,       # have work-related stress
    wlb = L16_a,              # Impact of work on family or personal life
    union = A45,             # Whether a member of a labor union.
    parttime = A58a,         # have multiple part-time job 
    
    health = A16,            # Health impact on work
    sex = A2,                # Gender
    year = A3_1,             # Year of birth
    urban = A18,             # Urban or rural residence
    party = A10,             # Party membership ？
    education = A7a,         # Education level 
    income = A62             # total family Income in last year
  )

# check sampling weight
CGSS %>% 
  ggplot(aes(x = weight)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Sampling Weight",
       x = "Sampling Weight",
       y = "Frequency")

ggsave("weight_full.png", width = 6, height = 4)

# recode variables
CGSS = CGSS %>% 
  mutate(
    # job insecurity
    # workhours: working hours per week
    workhours = ifelse(workhours == 998 | workhours == 999 | workhours == 168, NA, workhours), # remove 168
    # workcontract: type of work contract
    workcontract = case_when(
      workcontract == 98 | workhours == 99 ~ NA,
      workcontract == 2 ~ 1, # 1. infinite term (most stable)
      workcontract == 3 ~ 2, # 2. fixed term (stable)
      workcontract == 1 ~ 3, # 3. no contract (least stable)
    ),
    # workself: job atonomy 
    # 1 = completely controlled by self, 4 =  completely controlled by others
    workself = ifelse(workself == 98 | workself == 99, NA, workself),
    # satisfaction
    # 1 = very satisfied , 2 = satisfied, 3 = normal, 4 = not satisfied, 5 = not satisfied
    satisfaction = ifelse(satisfaction == 98 | satisfaction == 99, NA, satisfaction),
    # workstress
    # 1 = rarely, 2 = sometimes, 3 = often, 4 = always
    workstress = case_when(
      workstress == 98 | workstress == 99 ~ NA,
      workstress == 1 ~ 4,
      workstress == 2 ~ 3,
      workstress == 3 ~ 2,
      workstress == 4 ~ 1
    ),
    # wlb：Impact of work on family or personal life
    # 1 = always, 2 = often, 3 = sometimes, 4 = rarely, 5 = never
    wlb = ifelse(wlb == 98 | wlb == 99, NA, wlb),
    #union
    # 1 = currently yes, 2 = previously yes, 3 = never
    union = ifelse(union == 98 | union == 99, NA, union),
    #parttime
    # 1 = yes, 2= no
    parttime = ifelse(parttime == 98 | parttime == 99, NA, parttime),
    
    # mental health
    # depressed: feeling depressed
    # 1 = all the time, 2 = often, 3 = sometimes, 4 = rarely, 5 = never
    depressed = ifelse(depressed == 98 | depressed == 99, NA, depressed),
    # happy: feeling happy
    # 1 = very not happy, 2 = not happy, 3 = normal, 4 = happy, 5 = very happy
    happy = ifelse(happy == 98 | happy == 99, NA, happy),
    # health: did health affect your work or daily life
    # 1 = always, 2 = often, 3 = sometimes, 4 = rarely, 5 = never
    health = ifelse(health == 98, NA, health),
    
    # demographic variables
    # age
    age = 2021 - year,
    # sex: 0 = female； 1 = male
    sex = ifelse(sex == 2, 0, 1),
    # urban: 0 = rural； 1 = urban
    urban = case_when(
      urban == 1 | urban == 3 ~ 0,
      urban == 2 | urban == 4 ~ 1,
      TRUE ~ NA
    ),
    # party: 0 = not CCP member； 1 = CCP member
    party = case_when(
      party == 4 ~ 1,
      party == 98 | party == 99 ~ NA,
      TRUE ~ 0
    ),
    # education 
    # 1 = no, 2= home, 3= primary, 4=middle, 5,6=high, 78= technical, 9,12=college, 13= master+
    ####"should we combine some of the category?"###
    # 1= no, 2= college and under, 3=beyond college
    education = case_when(
      education == 1 ~ 1,
      education %in% c(2, 3, 4, 5, 6, 7, 8, 9, 12) ~ 2,
      education == 13 ~ 3,
      education == 98 | education == 99 ~ NA,
      TRUE ~ 0
    )
    # income
    ## consder splitting into categories##
  )

# select variables of interest and remove incomplete cases
CGSS = CGSS %>% 
  select(id, 
         workhours, workcontract, workself, satisfaction, workstress,
         depressed, happy, health, 
         party,
         provinces, community_i, weight, weight_raking) %>% 
  na.omit()

# remove the observation that has workhours == 168
CGSS = CGSS[CGSS$workhours != 168, ]

nrow(CGSS)

# we have 993 cases

# recode sex and party
CGSS = CGSS %>%
  mutate(# sex = recode(sex, "0" = "Female", "1" = "Male"),
         party = recode(party, "0" = "Non-CCP", "1" = "CCP"))

# 2. descriptive statistics
# check distribution of each variable
# workhours
CGSS %>% 
    ggplot(aes(x = workhours)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(title = "Distribution of Working Hours per Week",
         x = "Working Hours per Week",
         y = "Frequency")

# workcontract
CGSS %>% 
    ggplot(aes(x = workcontract)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Work Contract",
         x = "Type of Work Contract",
         y = "Frequency")

# workself
CGSS %>% 
    ggplot(aes(x = workself)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Job Autonomy",
         x = "Job Autonomy",
         y = "Frequency")

# depressed，take log or cubic root
CGSS %>% 
    ggplot(aes(x = depressed)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Feeling Depressed",
         x = "Feeling Depressed",
         y = "Frequency")

# log, still not
CGSS %>% 
  ggplot(aes(x = log(depressed))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Feeling Depressed",
       x = "Feeling Depressed",
       y = "Frequency")

#(depressed)^(1/3): still not 
CGSS %>% 
  ggplot(aes(x = (depressed)^(1/3))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Feeling Depressed",
       x = "Feeling Depressed",
       y = "Frequency")

# happy
CGSS %>% 
    ggplot(aes(x = happy)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Feeling Happy",
         x = "Feeling Happy",
         y = "Frequency")

# health
CGSS %>% 
    ggplot(aes(x = health)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Health Affecting Work or Daily Life",
         x = "Health Affecting Work or Daily Life",
         y = "Frequency")

# party
CGSS %>%
    ggplot(aes(x = party)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of CCP Membership",
            x = "CCP Membership",
            y = "Frequency")

###### new:
# satisfaction
CGSS %>%
  ggplot(aes(x =satisfaction)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Job Satisfaction",
       x = "Job Satisfaction",
       y = "Frequency")
# workstress
CGSS %>%
  ggplot(aes(x =workstress)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of workstress",
       x = "workstress",
       y = "Frequency")

# sampling weight
CGSS %>% 
    ggplot(aes(x = weight)) +
    geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
    labs(title = "Distribution of Sampling Weight",
         x = "Sampling Weight",
         y = "Frequency")
ggsave("weight_subset.png", width = 6, height = 4)

# descriptive statistics
CGSS %>% select(-c(id, weight, weight_raking)) %>% 
    as.data.frame() %>%
    stargazer(., type = "text", 
              title = "Descriptive Statistics of Variables",
              digits = 2)

# 3. SEM
# overall model
model1 = '
    # measurement model

    insecurity =~ workhours + workcontract + workself + satisfaction + workstress 
    mental =~ depressed + happy + health

    # structural model
    
    mental ~ insecurity
    '

# equal regression coefficient
model2 = '
    # measurement model
    insecurity =~ workhours + workcontract + workself + satisfaction + workstress
    mental =~ depressed + happy + health

    # structural model (with constrained regression coefficient)
    mental ~ c(b1, b1)*insecurity
'

fit1 = sem(model1, 
           data = CGSS, 
           estimator = "WLSMV", 
           ordered = c("workcontract", "workself", "satisfaction", "workstress",
                       "depressed", "happy", "health"))
summary(fit1)

# account for survey design
## treat each individual as independent and ignore cluster
design = svydesign(ids = ~1, weights = ~weight, data = CGSS)

## treat each community as a cluster (SSU-level)
design2 = svydesign(ids = ~community_i, weights = ~weight, data = CGSS)

# only WLS is supported, account for survey weights
fit1_survey = lavaan.survey(fit1, design, estimator = "WLS")
summary(fit1_survey)

# account for survey weights and cluster
fit1_survey2 = lavaan.survey(fit1, design2, estimator = "WLS")
summary(fit1_survey2)

# account for survey weights using lavaan
fit1_survey3 = sem(model1,
  data = CGSS,
  estimator = "WLSMV",
  ordered = c(
    "workcontract", "workself", "satisfaction", "workstress",
    "depressed", "happy", "health"
  ),
  sampling.weights = "weight"
)
summary(fit1_survey3)

# the result from unweighted and weighted data are similar

# check modifiication indices
modindices(fit1, minimum = 10, sort = TRUE)

# the suggestion from modification indices does not make sense
# but we still try one to see the result
model4 = '
    # measurement model

    insecurity =~ workhours + workcontract + workself + satisfaction + workstress 
    mental =~ depressed + happy + health

    # structural model
    
    mental ~ insecurity

    # covariance
    depressed ~~ health
    '

fit2 = sem(model4, 
           data = CGSS, 
           estimator = "WLSMV", 
           ordered = c("workcontract", "workself", "satisfaction", "workstress",
                       "depressed", "happy", "health"))
summary(fit2)
# this result is similar to the previous one

# party
fit3_party = sem(model1, 
                 data = CGSS, 
                 estimator = "WLSMV", 
                 group = "party",
                 ordered = c("workcontract", "workself", "satisfaction", "workstress",
                             "depressed", "happy", "health"))
summary(fit3_party)

fit3_party_coef = sem(model2, 
                 data = CGSS, 
                 estimator = "WLSMV", 
                 group = "party",
                 ordered = c("workcontract", "workself", "satisfaction", "workstress",
                             "depressed", "happy", "health"))
summary(fit3_party_coef)

lavTestLRT(fit3_party, fit3_party_coef)

## if we use survey weights
fit3_party_survey = sem(model1, 
                        data = CGSS, 
                        estimator = "WLSMV", 
                        group = "party",
                        ordered = c("workcontract", "workself", "satisfaction", "workstress",
                                    "depressed", "happy", "health"),
                        sampling.weights = "weight")

summary(fit3_party_survey)

fit3_party_coef_survey = sem(model2, 
                             data = CGSS, 
                             estimator = "WLSMV", 
                             group = "party",
                             ordered = c("workcontract", "workself", "satisfaction", "workstress",
                                         "depressed", "happy", "health"),
                             sampling.weights = "weight")
summary(fit3_party_coef_survey)

lavTestLRT(fit3_party_survey, fit3_party_coef_survey)
# the result is similar to the unweighted data

# save the results
rm(CGSS2021)
save(list = ls(), file = "FinalResults.RData")

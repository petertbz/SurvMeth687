library(haven)
library(dplyr)
library(lavaan)
library(ggplot2)
library(stargazer)

# 1. data clean
# import data
CGSS2021 = read_dta("CGSS2021.dta")

# select variables and rename
# consider split job insecurity into objective and subjective wellbeing: 
CGSS = CGSS2021 %>% 
  select(id, A53aa, A59b, A59g, A17, A36, L17, L11_c, L16_a, A45, A58a, A16, A2, A3_1, A18, A10, A7a, A62,
         weight, weight_raking) %>% 
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

# recode variables
CGSS = CGSS %>% 
  mutate(
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
    # depressed: feeling depressed
    # 1 = all the time, 2 = often, 3 = sometimes, 4 = rarely, 5 = never
    depressed = ifelse(depressed == 98 | depressed == 99, NA, depressed),
    # happy: feeling happy
    # 1 = very not happy, 2 = not happy, 3 = normal, 4 = happy, 5 = very happy
    happy = ifelse(happy == 98 | happy == 99, NA, happy),
    # satisfaction
    # 1 = very satisfied , 2 = satisfied, 3 = normal, 4 = not satisfied, 5 = not satisfied
    satisfaction = ifelse(satisfaction == 98 | satisfaction == 99, NA, satisfaction),
    # workstress
    # 1 = always , 2 = often, 3 = sometimes, 4 = rarely
    workstress = ifelse(workstress == 98 | workstress == 99, NA, workstress),
    #wlb
    # 1 = always, 2 = often, 3 = sometimes, 4 = rarely, 5 = never
    wlb = ifelse(wlb == 98 | wlb == 99, NA, wlb),
    #union
    # 1 = currently yes, 2 = previously yes, 3 = never
    union = ifelse(union == 98 | union == 99, NA, union),
    #parttime
    # 1 = yes, 2= no
    parttime = ifelse(parttime == 98 | parttime == 99, NA, parttime),
    
    # health: did health affect your work or daily life
    # 1 = always, 2 = often, 3 = sometimes, 4 = rarely, 5 = never
    health = ifelse(health == 98, NA, health),
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

# remove incomplete cases
CGSS = CGSS %>% 
  na.omit() %>% 
  select(id, workhours, workcontract, workself, 
         depressed, happy,satisfaction,workstress,wlb,union,parttime,
         health, age, sex, urban, party,
         income, education,
         weight, weight_raking)
nrow(CGSS)

# we have 1,049 cases
# now 957

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

# age
CGSS %>% 
    ggplot(aes(x = age)) +
    geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
    labs(title = "Distribution of Age",
         x = "Age",
         y = "Frequency")

# sex
CGSS %>%
    ggplot(aes(x = sex)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Sex",
            x = "Sex",
            y = "Frequency")

# urban
CGSS %>%
    ggplot(aes(x = urban)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Urban or Rural",
            x = "Urban or Rural",
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

# wlb
CGSS %>%
  ggplot(aes(x =wlb)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of work impacted life",
       x = "wlb",
       y = "Frequency")

#union
CGSS %>%
  ggplot(aes(x =union)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of union membership",
       x = "union membership",
       y = "Frequency")

#parttime
CGSS %>%
  ggplot(aes(x =parttime)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of having parttime job",
       x = "parttime job",
       y = "Frequency")

#income
CGSS %>%
  ggplot(aes(x =income)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of total family income in 2020",
       x = "total family income",
       y = "Frequency")

# education
CGSS %>%
  ggplot(aes(x =education)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of highlest education",
       x = "highest education",
       y = "Frequency")

# descriptive statistics
CGSS %>% select(-id) %>% 
    as.data.frame() %>%
    stargazer(., type = "text", 
              title = "Descriptive Statistics of Variables",
              digits = 2)

# 3. SEM
# overall model
model1 = '
    # measurement model

    insecurity =~ workhours + workcontract + workself + satisfaction + parttime + union 
    mental =~ depressed + happy + health + workstress + wlb

    # structural model
    
    mental ~ insecurity
    '
fit1 = sem(model1, data = CGSS)
summary(fit1)

# modification indices for model 1
modificationIndices(fit1, sort = TRUE, minimum.value = 3)

fit1_urban = sem(model1, data = CGSS, group = "urban")
summary(fit1_urban)

# however, other MGA cannot run
fit1_sex = sem(model1, data = CGSS, group = "sex")
summary(fit1_sex)

#####################
# recode working hours
CGSS = CGSS %>% 
    mutate(
        workhours2 = case_when(
            workhours <= 40 ~ 1,
            workhours > 40 & workhours <= 50 ~ 2,
            workhours > 50 ~ 3
        )
    )

ggplot(CGSS, aes(x = workhours2)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = "Distribution of Working Hours per Week",
         x = "Working Hours per Week",
         y = "Frequency")

model2 = '
    # measurement model

    insecurity =~ workhours2 + workcontract + workself
    mental =~ depressed + happy + health

    # structural model
    
    mental ~ insecurity
    '
fit2 = sem(model2, data = CGSS)
summary(fit2)

# modification indices for model 2
modificationIndices(fit2, sort = TRUE, minimum.value = 3)

fit2_urban = sem(model2, data = CGSS, group = "urban")
summary(fit2_urban)

fit2_sex = sem(model2, data = CGSS, group = "sex")


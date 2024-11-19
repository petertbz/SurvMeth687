library(haven)
library(dplyr)
library(lavaan)
library(ggplot2)
library(stargazer)

# 1. data clean
# import data
CGSS2021 = read_dta("CGSS2021.dta")

# select variables and rename
CGSS = CGSS2021 %>% 
    select(id, A53aa, A59b, A59g, A17, A36, A16, A2, A3_1, A18, A10,
          weight, weight_raking) %>% 
    rename(
        workhours = A53aa,
        workcontract = A59b,
        workself = A59g,
        depressed = A17,
        happy = A36,
        health = A16,
        sex = A2,
        year = A3_1,
        urban = A18,
        party = A10
        )

# recode variables
CGSS = CGSS %>% 
    mutate(
        # workhours: working hours per week
        workhours = ifelse(workhours == 998 | workhours == 999, NA, workhours),
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
        )
    )

# remove incomplete cases
CGSS = CGSS %>% 
    na.omit() %>% 
    select(id, workhours, workcontract, workself, 
           depressed, happy, health, 
           age, sex, urban, party,
           weight, weight_raking)
nrow(CGSS)

# we have 1,049 cases

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

# depressed
CGSS %>% 
    ggplot(aes(x = depressed)) +
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

    insecurity =~ workhours + workcontract + workself
    mental =~ depressed + happy + health

    # structural model
    
    insecurity ~ mental
    '
fit1 = sem(model1, data = CGSS)
summary(fit1)

# modification indices for model 1
modificationIndices(fit1, sort = TRUE, minimum.value = 3)

fit1_urban = sem(model1, data = CGSS, group = "urban")
summary(fit1_urban)

# however, other MGA cannot run

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
    
    insecurity ~ mental
    '
fit2 = sem(model2, data = CGSS)
summary(fit2)

# modification indices for model 2
modificationIndices(fit2, sort = TRUE, minimum.value = 3)

library(dplyr)
library(lattice)
library(caret)

## Introduction


crash_data_old <- read.csv("http://www.tmr.qld.gov.au/~/media/aboutus/corpinfo/Open%20data/crash/locations.csv", 
                       stringsAsFactors = F) 



crash_data <- crash_data_old %>% 
  filter(Crash_Year %in% 2018,
         !Crash_Nature %in% c("Struck by external load", "Struck by internal load","Collision - miscellaneous"),
         !Crash_Traffic_Control %in% c("Railway - lights and boom gate", "Railway - lights only", 
                                       "Railway crossing sign", "Supervised school crossing",
                                       "School crossing - flags"),
         Crash_Speed_Limit != ""
         ) %>% 
  mutate(Crash_Emergency = as.factor(if_else(Crash_Severity %in% c("Fatal","Hospitalisation"), 'Yes', 'No')),
         Crash_Week = if_else(Crash_Day_Of_Week %in% c("Saturday", "Sunday"), 'Weekend', 'Weekday')) %>% 
  as_tibble() %>% 
  select(-starts_with("Count"), -contains("DCA"), -Loc_Post_Code, -contains("Street"), -contains("Road_Name"))

# write.csv(crash_data, "crash_data_18.csv")

unique(crash_data$Crash_Severity)
str(crash_data)
dim(crash_data)

# Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
#   Crash_Traffic_Control + Crash_Speed_Limit + Crash_Atmospheric_Condition + 
#   Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align

with(crash_data, table(Crash_Speed_Limit, Crash_Nature))

histogram(~ Crash_Emergency | Crash_Speed_Limit, data = crash_data,
          scales = list(x = list(rot = 45)))

histogram(~ Crash_Emergency | Crash_Road_Horiz_Align, data = crash_data,
          scales = list(x = list(rot = 45)))

histogram(~ Crash_Emergency | Crash_Nature, data = crash_data,
          scales = list(x = list(rot = 45)))

histogram(~ as.factor(Crash_Hour) | Crash_Serious, data = crash_data,
          scales = list(x = list(rot = 45)))

histogram(~ as.factor(Crash_Hour) | Crash_Emergency, data = crash_data,
          scales = list(x = list(rot = 45)))



paste0(colnames(crash_data), collapse = " + ")

glm1 <- glm(Crash_Serious ~ Crash_Month + Crash_Day_Of_Week + as.factor(Crash_Hour) + Crash_Nature + Crash_Type + 
              Loc_ABS_Remoteness + Crash_Roadway_Feature + Crash_Traffic_Control + 
              Crash_Speed_Limit + Crash_Road_Surface_Condition + Crash_Atmospheric_Condition + 
              Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align + 
              as.factor(Crash_DCA_Code) + Crash_DCA_Group_Description, data = crash_data_18,
            family = binomial())

formula(MASS::stepAIC(glm1))

glm2 <- glm(Crash_Serious ~ Crash_Week + Crash_Nature + Crash_Speed_Limit + Loc_ABS_Remoteness + 
              Crash_Traffic_Control + Crash_Atmospheric_Condition + 
              Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align,
            data = crash_data_18,
            family = binomial())

glm3 <- glm(Crash_Serious ~ (Crash_Week + Crash_Nature + Crash_Speed_Limit + Loc_ABS_Remoteness + 
              Crash_Traffic_Control + Crash_Atmospheric_Condition + 
              Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align),
            data = crash_data_18,
            family = binomial())

# the binomial family the links logit, probit, cauchit, (corresponding to 
# logistic, normal and Cauchy CDFs respectively) log and cloglog (complementary 
# log-log)

glm3.step <- MASS::stepAIC(glm3)

glm3.1 <- glm(Crash_Serious ~ Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
                Crash_Roadway_Feature + Crash_Traffic_Control + Crash_Speed_Limit + 
                Crash_Road_Surface_Condition + Crash_Lighting_Condition + 
                Crash_Road_Vert_Align + as.factor(Crash_DCA_Code),
            data = crash_data_18,
            family = binomial(link = "logit"))

glm3.2 <- glm(Crash_Serious ~ Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
                Crash_Roadway_Feature + Crash_Traffic_Control + Crash_Speed_Limit + 
                Crash_Road_Surface_Condition + Crash_Lighting_Condition + 
                Crash_Road_Vert_Align + as.factor(Crash_DCA_Code),
            data = crash_data_18,
            family = binomial(link = "probit"))

glm3.3 <- glm(Crash_Serious ~ Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
                Crash_Roadway_Feature + Crash_Traffic_Control + Crash_Speed_Limit + 
                Crash_Road_Surface_Condition + Crash_Lighting_Condition + 
                Crash_Road_Vert_Align + as.factor(Crash_DCA_Code),
            data = crash_data_18,
            family = binomial(link = "cauchit"))

glm3.4 <- glm(Crash_Serious ~ Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
                Crash_Roadway_Feature + Crash_Traffic_Control + Crash_Speed_Limit + 
                Crash_Road_Surface_Condition + Crash_Lighting_Condition + 
                Crash_Road_Vert_Align + as.factor(Crash_DCA_Code),
            data = crash_data_18,
            family = binomial(link = "logit"))

glm3.5 <- glm(Crash_Serious ~ Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
                Crash_Roadway_Feature + Crash_Traffic_Control + Crash_Speed_Limit + 
                Crash_Road_Surface_Condition + Crash_Lighting_Condition + 
                Crash_Road_Vert_Align + as.factor(Crash_DCA_Code),
            data = crash_data_18,
            family = binomial(link = "cloglog"))

AIC(glm3.1,glm3.2,glm3.3,glm3.4,glm3.5)

par(mfrow=c(2,2))
plot(glm3.2)

formula(MASS::stepAIC(glm2))

# Crash_Serious ~ Crash_Nature + Crash_Speed_Limit + Crash_Road_Horiz_Align

exp(cbind(OR = coef(glm2), confint(glm2)))

par(mfrow=c(2,2))
plot(glm2)

newdata1 <- 
  with(crash_data_18, tidyr::crossing(
    Crash_Week = unique(Crash_Week), 
    Crash_Nature = unique(Crash_Nature), 
    Loc_ABS_Remoteness = unique(Loc_ABS_Remoteness),
    Crash_Traffic_Control = unique(Crash_Traffic_Control),
    Crash_Speed_Limit = unique(Crash_Speed_Limit),
    Crash_Atmospheric_Condition = unique(Crash_Atmospheric_Condition),
    Crash_Lighting_Condition = unique(Crash_Lighting_Condition),
    Crash_Road_Horiz_Align = unique(Crash_Road_Horiz_Align),
    Crash_Road_Vert_Align = unique(Crash_Road_Vert_Align))
  )

newdata1$deathP <- predict(glm2, newdata = newdata1, type = "response")
dim(newdata1)

bwplot(deathP ~ Crash_Nature | Crash_Speed_Limit, data = newdata1,
       scales = list(x = list(rot = 45)))

newdata1 %>% arrange(deathP)

crash_data_18$cooks_d <- cooks.distance(glm3.2)
crash_data_18_filt %>% arrange(-cooks_d) %>% 
  select(Crash_Severity, Crash_Year, Crash_Nature, Crash_Speed_Limit, Crash_Type, cooks_d)

crash_data_18_filt <- crash_data_18 %>% filter(cooks_d < 0.0003)
plot(crash_data_18_filt$cooks_d)



glm3.2 <- glm(Crash_Serious ~ Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
                Crash_Roadway_Feature + Crash_Traffic_Control + Crash_Speed_Limit + 
                Crash_Road_Surface_Condition + Crash_Lighting_Condition + 
                Crash_Road_Vert_Align + as.factor(Crash_DCA_Code),
              data = crash_data_18_filt,
              family = binomial(link = "probit"))
par(mfrow=c(2,2))
plot(glm3.2)

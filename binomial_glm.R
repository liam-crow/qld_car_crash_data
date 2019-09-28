library(dplyr)
library(lattice)

crash_data <- read.csv("http://www.tmr.qld.gov.au/~/media/aboutus/corpinfo/Open%20data/crash/locations.csv", stringsAsFactors = F)

crash_data_18 <- crash_data %>% 
  filter(Crash_Year == 2018,
         !Crash_Nature %in% c("Struck by external load", "Struck by internal load"),
         !Crash_Traffic_Control %in% c("Railway - lights and boom gate", "Railway - lights only", "Railway crossing sign", "Supervised school crossing"),
         ) %>% 
  mutate(Crash_Death = as.factor(if_else(Crash_Severity %in% c("Fatal"), 'Yes', 'No')),
         Crash_Week = as.factor(if_else(Crash_Day_Of_Week %in% c("Saturday", "Sunday"), 'Weekend', 'Weekday')))

str(crash_data_18)

# Crash_Day_Of_Week + Crash_Nature + Loc_ABS_Remoteness + 
#   Crash_Traffic_Control + Crash_Speed_Limit + Crash_Atmospheric_Condition + 
#   Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align

with(crash_data_18, table(Crash_Speed_Limit, Crash_Nature))

histogram(~ Crash_Death | Crash_Speed_Limit, data = crash_data_18,
          scales = list(x = list(rot = 45)))

histogram(~ Crash_Death | Crash_Road_Horiz_Align, data = crash_data_18,
          scales = list(x = list(rot = 45)))

paste0(colnames(crash_data_18), collapse = " + ")

glm1 <- glm(Crash_Death ~ Crash_Month + Crash_Day_Of_Week + as.factor(Crash_Hour) + Crash_Nature + Crash_Type + 
              Loc_ABS_Remoteness + Crash_Roadway_Feature + Crash_Traffic_Control + 
              Crash_Speed_Limit + Crash_Road_Surface_Condition + Crash_Atmospheric_Condition + 
              Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align + 
              as.factor(Crash_DCA_Code) + Crash_DCA_Group_Description, data = crash_data_18,
            family = binomial())

formula(MASS::stepAIC(glm1))

glm1 <- glm(Crash_Death ~ Crash_Week + Crash_Nature + Crash_Speed_Limit + Loc_ABS_Remoteness + 
              Crash_Traffic_Control + Crash_Atmospheric_Condition + 
              Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align,
            data = crash_data_18,
            family = binomial())

glm2 <- glm(Crash_Death ~ Crash_Week + Crash_Nature * Crash_Speed_Limit + Loc_ABS_Remoteness + 
              Crash_Traffic_Control + Crash_Atmospheric_Condition + 
              Crash_Lighting_Condition + Crash_Road_Horiz_Align + Crash_Road_Vert_Align,
            data = crash_data_18,
            family = binomial())

AIC(glm1, glm2)

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


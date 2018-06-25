## This Commmand was used to clean the data and remove the 1 and 99 that was in the original data set ##
Video_Cleaned <- Video_Data[which(Video_Data$like == 2 | Video_Data$like == 3 | Video_Data$like == 4 | Video_Data$like == 5),]

##This was the commands used to change the like string to binary terms. The way we did was to classify the 2 & 3 data points by a binary entry of 1 as Liked to play and 4 & 5 as otherwise ## 
Video_Cleaned$like[Video_Cleaned$like == 2 | Video_Cleaned$like == 3] = 1
Video_Cleaned$like[Video_Cleaned$like == 4 | Video_Cleaned$like == 5] = 0

## Using the cleaning function we removed data with regards to the 99's in the work string ##
Video_Removed <- Video_Cleaned[-c(32, 44, 45),]

## Factoring data for the logit model ##
Video_Removed_Copy <- Video_Removed
Video_Removed_Copy$where <- factor(Video_Removed_Copy$where)
Video_Removed_Copy$freq <- factor(Video_Removed_Copy$freq)
Video_Removed_Copy$busy <- facotr(Video_Removed_Copy$busy)
Video_Removed_Copy$educ <- factor(Video_Removed_Copy$educ)
Video_Removed_Copy$sex <- factor(Video_Removed_Copy$sex)
Video_Removed_Copy$own <- factor(Video_Removed_Copy$own)
Video_Removed_Copy$home <- factor(Video_Removed_Copy$home)
Video_Removed_Copy$math <- factor(Video_Removed_Copy$math)
Video_Removed_Copy$cdrom <- factor(Video_Removed_Copy$cdrom)
Video_Removed_Copy$email <- factor(Video_Removed_Copy$email)
Video_Removed_Copy$grade <- factor(Video_Removed_Copy$grade)

## Logit models that failed ##
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home + math + work + own + cdrom + email + grade, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glmmylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home + math + work + own + cdrom + email, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home + math + work + own + cdrom + email, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home + math + work + own + cdrom, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home + math + work + own, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home + math + work, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home + math, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age + home, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex + age, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ + sex, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy + educ, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq + busy, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where + freq, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time + where, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ time, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)

## Logit Models that may work ##
mylogit <- glm(like ~ where + freq + busy + educ +sex + age + math + work + cdrom + email + grade, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ where + freq + busy + educ + sex + age + math + work + grade, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ where + freq + busy + educ + sex + age + math + work, data = Video_Removed_Copy,family = "binomial")
summary(mylogit)
mylogit <- glm(like ~ where + freq + educ + busy + age + time + cdrom + sex + work, data = Video_Removed_Copy,family = "log")
summary(mylogit)

## This was the final one ##
mylogit <- glm(like~ email + cdrom + home + work + educ + own + sex + math + grade, data = Video_Removed_Copy, family = "binomial")
summary(mylogit)
summary(mylogit)
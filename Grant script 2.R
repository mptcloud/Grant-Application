x <- c('AppliedPredictiveModeling', 'caret', 'forcats', 'glmnet', 'kernlab', 'klaR', 'MASS', 'pamr', 
       'pls', 'plyr', 'lubridate', 'pROC', 'rms', 'sparseLDA', 'stringr', 'subselect', 'tidyverse')
lapply(x, require, character.only = TRUE)

raw_data2 <- read_csv("unimelb_training.csv")

raw_data2$Sponsor.Code[is.na(raw_data2$Sponsor.Code)] <- 'UnkCode'
raw_data2$Sponsor.Code <- factor(raw_data2$Sponsor.Code)

raw_data2$Grant.Category.Code[is.na(raw_data2$Grant.Category.Code)] <- 'UnkCatCode'
raw_data2$Grant.Category.Code <- factor(raw_data2$Grant.Category.Code)

raw_data2$Contract.Value.Band...see.note.A[is.na(
                raw_data2$Contract.Value.Band...see.note.A)] <- 'UnkBand'
raw_data2$Contract.Value.Band...see.note.A <- factor(raw_data2$Contract.Value.Band...see.note.A)

raw_data2$Role.1[is.na(raw_data2$Role.1)] <- 'Unk'

# Get the unique values of the birth years and department codes. 
# These will be used later to make factor variables.

bYears2 <- raw_data2 %>%
        select(contains('Year.of.Birth')) %>%
        distinct()
bYears3 <- (unique(do.call("c", bYears2)))
bYears3 <- bYears3[!is.na(bYears3)]
bYears3 <- c(bYears3, 'UnkYear')

dpmt2 <- raw_data2 %>%
        select(contains('Dept.No')) %>%
        distinct()
dpmt3 <- (unique(do.call("c", dpmt2)))
dpmt3 <- dpmt3[!is.na(dpmt3)]
dpmt3 <- c(dpmt3, 'UnkDept')

# 'Tidyfying' the data. 
tmp2 <- vector(mode = "list", length = 15)
for(i in 1:15) {
        tmpData2 <- raw_data2 %>%
                select(Grant.Application.ID, matches(str_c("\\.", i, "$")))
        names(tmpData2) <- str_replace(names(tmpData2), str_c("\\.", i, "$"), "")
        if(i == 1) nms <- names(tmpData2)
        if(all(names(tmpData2) != "RFCD.Code")) tmpData2$RFCD.Code <- NA
        if(all(names(tmpData2) != "RFCD.Percentage")) tmpData2$RFCD.Percentage <- NA
        if(all(names(tmpData2) != "SEO.Code")) tmpData2$SEO.Code <- NA
        if(all(names(tmpData2) != "SEO.Percentage")) tmpData2$SEO.Percentage <- NA
        
        tmp2[[i]] <- tmpData2[,nms]
        rm(tmpData2)
}

tidied <- do.call("rbind", tmp2)

# Easier to get relevant information for a particular grant
# Example: See all investigators for grant ID 1
# data1 <- tidied %>%
#        + filter(Grant.Application.ID == 1)
# View(data1)

# Removing rows with no roles. ~17000 rows left.
tidied <- tidied %>%
        filter (Role != '')

tidied$Number.of.Successful.Grant <- as.integer(tidied$Number.of.Successful.Grant)
tidied$Number.of.Unsuccessful.Grant <- as.integer(tidied$Number.of.Unsuccessful.Grant)

tidied$Country.of.Birth[is.na(tidied$Country.of.Birth)] <- 'UnkCountry'

tidied$Home.Language[is.na(tidied$Home.Language)] <- 'UnkLang'

tidied$Year.of.Birth[is.na(tidied$Year.of.Birth)] <- 'UnkYear'

tidied$Dept.No.[is.na(tidied$Dept.No.)] <- 'UnkDept'

tidied$With.PHD[is.na(tidied$With.PHD)] <- 'UnkDeg'

tidied$Faculty.No.[is.na(tidied$Faculty.No.)] <- 'UnkNo.'

tidied$A. <- as.integer(tidied$A.)
tidied$A <- as.integer(tidied$A)
tidied$B <- as.integer(tidied$B)
tidied$C <- as.integer(tidied$C)

# 'Factoring' a few of the columns
tidied$Role <- factor(tidied$Role)
tidied$Year.of.Birth <- factor(tidied$Year.of.Birth, levels = bYears3)
tidied$Country.of.Birth <- factor(tidied$Country.of.Birth)
tidied$Home.Language <- factor(tidied$Home.Language)
tidied$Dept.No. <- factor(tidied$Dept.No.)
tidied$Faculty.No. <- factor(tidied$Faculty.No.)

tidied$RFCD.Percentage[is.na(tidied$RFCD.Code)] <- NA
tidied$RFCD.Code[is.na(tidied$RFCD.Code)] <- 'UnkRFCD'
tidied$RFCD.Code <- factor(tidied$RFCD.Code)
tidied <- tidied %>%
        mutate(RFCD.Code = fct_recode(RFCD.Code, '0' = '999999'))
tidied$RFCD.Code[tidied$RFCD.Code == '0'] <- 'UnkRFCD'

tidied$SEO.Percentage[is.na(tidied$SEO.Code)] <- NA
tidied$SEO.Code[is.na(tidied$SEO.Code)] <- 'UnkSEO'
tidied$SEO.Code <- factor(tidied$SEO.Code)
tidied <- tidied %>%
        mutate(SEO.Code = fct_recode(SEO.Code, '0' = '999999'))
tidied$SEO.Code[tidied$SEO.Code == '0'] <- 'UnkSEO'


tidied$No..of.Years.in.Uni.at.Time.of.Grant[is.na(tidied$No..of.Years.in.Uni.at.Time.of.Grant)] <- 
                                                                                'DurUnk'
tidied$No..of.Years.in.Uni.at.Time.of.Grant <- 
                factor(tidied$No..of.Years.in.Uni.at.Time.of.Grant)

# Abbreviate tenure length of applicants
tidied <- tidied %>%
        mutate(No..of.Years.in.Uni.at.Time.of.Grant = fct_recode(No..of.Years.in.Uni.at.Time.of.Grant, 
                                        "Dur0to5" = ">=0 to 5" ,
                                        "Dur5to10" = ">5 to 10",
                                        "Dur10to15" = ">10 to 15",
                                        "DurGT15" = "more than 15",
                                        "DurLT0" = "Less than 0"))

# Abbreviate role titles
tidied <- tidied %>%
        mutate(Role = fct_recode(Role, 
                                 "ECI" = "EXT_CHIEF_INVESTIGATOR" ,
                                 "SCI" = "STUD_CHIEF_INVESTIGATOR",
                                 "CI" = "CHIEF_INVESTIGATOR",
                                 "DR" = "DELEGATED_RESEARCHER",
                                 "EA" = "EXTERNAL_ADVISOR",
                                 "HV" = "HONVISIT", 
                                 "PS" = "PRINCIPAL_SUPERVISOR", 
                                 "SR" = "STUDRES",
                                 "UNK" =  "Unk"))

# Identifying zero/near zero variance predictors
# x <- nearZeroVar(tidied, saveMetrics = TRUE)
# x[x[,"zeroVar"] + x[,"nzv"] > 0, ] 

# Number of applicants per grant
num_people <- tidied %>%
        group_by(Grant.Application.ID) %>%
        summarise(noApplicants = n())

# Number of applicants by role per grant
role_count <- tidied %>%
        group_by(Grant.Application.ID, Role) %>%
        summarise(count = n())

role_count_2 <- spread(role_count, key = Role, value = count)

role_count_2 <- role_count_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# Number of applicants in each age group per grant
age_freq <-  tidied %>%
        group_by(Grant.Application.ID, Role, Year.of.Birth) %>%
        summarise(count = n())

age_freq_2 <- age_freq %>%
        mutate(type = str_c(Role, ".", Year.of.Birth)) %>%
        ungroup() %>%
        select(-(Role:Year.of.Birth))

age_freq_2 <- spread(age_freq_2, key = type, value = count) 

age_freq_2 <- age_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))
 
# x <- nearZeroVar(age_freq, saveMetrics = TRUE)
# str(x)       

# Frequency of applicants from different countries per grant
country_freq <-  tidied %>%
        group_by(Grant.Application.ID, Role, Country.of.Birth) %>%
        summarise(count = n())

country_freq_2 <- country_freq %>%
        mutate(type = str_c(Role, ".", Country.of.Birth)) %>%
        ungroup() %>%
        select(-(Role:Country.of.Birth))

country_freq_2 <- spread(country_freq_2, key = type, value = count) 

country_freq_2 <- country_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))
                   
# x <- nearZeroVar(country_freq, saveMetrics = TRUE)
# str(x)

# Frequency of applicants for native languages per grant
lang_freq <-  tidied %>%
        group_by(Grant.Application.ID, Role, Home.Language) %>%
        summarise(count = n())

lang_freq_2 <- lang_freq %>%
        mutate(type = str_c(Role, ".", Home.Language)) %>%
        ungroup() %>%
        select(-(Role:Home.Language))

lang_freq_2 <- spread(lang_freq_2, key = type, value = count) 

lang_freq_2 <- lang_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))


# x <- nearZeroVar(lang_freq, saveMetrics = TRUE)
# str(x)

# Number of applicants with Ph.D.'s per role per grant
phd_freq <-  tidied %>%
        group_by(Grant.Application.ID, Role, With.PHD) %>%
        summarise(count = n())

phd_freq_2 <- phd_freq %>%
        mutate(type = str_c(Role, ".", With.PHD)) %>%
        ungroup() %>%
        select(-(Role:With.PHD))

phd_freq_2 <- spread(phd_freq_2, key = type, value = count) 

phd_freq_2 <- phd_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# Positive value here!
# x <- nearZeroVar(phd_freq, saveMetrics = TRUE)
# str(x)

# Number of successful/unsuccessful grants per role per grant
success_freq <-  tidied %>%
        group_by(Grant.Application.ID, Role) %>%
        summarise(
                success = sum(Number.of.Successful.Grant, na.rm = TRUE),
                failure = sum(Number.of.Unsuccessful.Grant, na.rm = TRUE)
        )

success_freq_2 <- success_freq %>%
        gather('success', 'failure', key = 'Type', value = 'Counts') %>%
        unite(temp1, Role, Type, sep = '.') %>%
        spread(key = temp1, value = Counts)

success_freq_2 <- success_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# x <- nearZeroVar(success_freq, saveMetrics = TRUE)
# str(x)

# Frequency of applicants from specific departments per role per grant
dept_freq <-  tidied %>%
        group_by(Grant.Application.ID, Role, Dept.No.) %>%
        summarise(count = n())

dept_freq_2 <- dept_freq %>%
        mutate(type = str_c(Role, ".", Dept.No.)) %>%
        ungroup() %>%
        select(-(Role:Dept.No.))

dept_freq_2 <- spread(dept_freq_2, key = type, value = count) 

dept_freq_2 <- dept_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# x <- nearZeroVar(dept_freq, saveMetrics = TRUE)
# str(x)

# Dummy variables for tenure length of applicants
tenure_freq <-  tidied %>%
        group_by(Grant.Application.ID) %>%
        count(No..of.Years.in.Uni.at.Time.of.Grant)

tenure_freq_2 <- spread(tenure_freq, key = No..of.Years.in.Uni.at.Time.of.Grant, value = n)

tenure_freq_2 <- tenure_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# Frequency of applicants organised by faculty number per role per grant
faculty_freq <-  tidied %>%
        group_by(Grant.Application.ID, Role, Faculty.No.) %>%
        summarise(count = n())

faculty_freq_2 <- faculty_freq %>%
        mutate(type = str_c(Role, ".", Faculty.No.)) %>%
        ungroup() %>%
        select(-(Role:Faculty.No.))

faculty_freq_2 <- spread(faculty_freq_2, key = type, value = count) 

faculty_freq_2 <- faculty_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# x <- nearZeroVar(faculty_freq, saveMetrics = TRUE)
# str(x)

# Number of publications organised by journal type per grant
pub_freq <-  tidied %>%
        group_by(Grant.Application.ID) %>%
        summarise(
                Astar = sum(A., na.rm = TRUE),
                A = sum(A, na.rm = TRUE),
                B = sum(B, na.rm = TRUE),
                C = sum(C, na.rm = TRUE),
                PubTotal = sum(c(A., A, B, C), na.rm = TRUE)
        )

# Number of publications organised by journal type per role per grant
pub_freq_byrole <-  tidied %>%
        group_by(Grant.Application.ID, Role) %>%
        summarise(
                Astar = sum(A., na.rm = TRUE),
                A = sum(A, na.rm = TRUE),
                B = sum(B, na.rm = TRUE),
                C = sum(C, na.rm = TRUE)
        )

pub_freq_byrole_2 <- pub_freq_byrole %>%
        gather('Astar', 'A', 'B', 'C', key = 'Type', value = 'Counts') %>%
        unite(temp1, Role, Type, sep = '.') %>%
        spread(key = temp1, value = Counts)

pub_freq_byrole_2 <- pub_freq_byrole_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# x <- nearZeroVar(pub_freq_byrole, saveMetrics = TRUE)
# str(x)

# RFCD Code count
rfcd_freq <-  tidied %>%
        group_by(Grant.Application.ID, RFCD.Code) %>%
        summarise(count = n())

rfcd_freq_2 <- spread(rfcd_freq, key = RFCD.Code, value = count)

rfcd_freq_2 <- rfcd_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# SEO Code count
seo_freq <-  tidied %>%
        group_by(Grant.Application.ID, SEO.Code) %>%
        summarise(count = n())

seo_freq_2 <- spread(seo_freq, key = SEO.Code, value = count)

seo_freq_2 <- seo_freq_2 %>% 
        mutate_all(funs(ifelse(is.na(.), 0, .)))

# Dummy variables for grant-relevant data
grantData <- raw_data2 %>%
        select(Sponsor.Code, Contract.Value.Band...see.note.A, Grant.Category.Code)

startDate <- dmy(raw_data2$Start.date)

grantData$Month <- factor(as.character(month(startDate, label = TRUE)))
grantData$Weekday <- factor(as.character(wday(startDate, label = TRUE)))
grantData$Day <- yday(startDate) # Check
grantYear <- year(startDate) # Why?

dummies <- dummyVars(~., data = grantData)
grantData <- as.data.frame(predict(dummies, grantData))
# names(grantData) <- gsub(" ", "", names(grantData))

# grantData$Grant.Application.ID <- raw_data2$Grant.Application.ID
grantData$Class <- factor(ifelse(raw_data2$Grant.Status, "successful", "unsuccessful"))
grantData$Grant.Application.ID <- raw_data2$Grant.Application.ID

grantData$is2008 <- (year(startDate) == 2008)

# Merging all the datasets created so far
final <- merge(role_count_2, age_freq_2)
final <- merge(final, country_freq_2)
final <- merge(final, lang_freq_2)
final <- merge(final, phd_freq_2)
final <- merge(final, success_freq_2)
final <- merge(final, dept_freq_2)
final <- merge(final, faculty_freq_2, by = 'Grant.Application.ID')
final <- merge(final, tenure_freq_2)
final <- merge(final, pub_freq_byrole_2)
final <- merge(final, pub_freq)
final <- merge(final, num_people)
final <- merge(final, rfcd_freq_2)
final <- merge(final, seo_freq_2)

final <- merge(final, grantData, by = 'Grant.Application.ID')

# Dropping the ID
final <- final %>%
        select(-Grant.Application.ID)
# print(str(final))

# Creating training and test sets
training <- final %>%
        filter(is2008 != TRUE)

pre2008 <- 1:nrow(training)

year2008 <- final %>%
        filter(is2008)

# Plots of success/failure vs prior successes/failures

success_freq_3 <- merge(success_freq_2, grantData[ , c('Class', 'Grant.Application.ID')], 
                        by.x = 'Grant.Application.ID', by.y = 'Grant.Application.ID')

ggplot(data = success_freq_3) +
        coord_cartesian(xlim = c(0, 10)) +
        geom_bar(mapping = aes(x = failure, y = (..count..)/sum(..count..)*100, fill = Class)) +
        labs(x = 'Number of prior failures', y = 'Percent of total')

ggplot(data = success_freq_3) +
        coord_cartesian(xlim = c(0, 10)) +
        geom_bar(mapping = aes(x = success, y = (..count..)/sum(..count..)*100, fill = Class)) +
        labs(x = 'Number of prior successes', y = 'Percent of total')

# Plot for relative proportion of successes vs. Day of the year

ggplot(data = DayClass) +
geom_bar(mapping = aes(x = Day, y = (..count..)/sum(..count..)*100, fill = Class))

# Randomly select some 2008 data for training and add it back into the existing training data
set.seed(42)
inTrain <- createDataPartition(year2008$Class, p = 3/4)[[1]]
training2 <- year2008[inTrain,]
testing   <- year2008[-inTrain,]
training <- rbind(training, training2)

training <- training %>%
        select(-is2008) 

testing <- testing %>%
        select(-is2008)

# A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
        keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
        x[,keepers,drop = FALSE]
}

# Removing zero variance predictors. Could also use nearZeroVar here.
training <- noZV(training)
testing <- testing[, names(training)]

# 'All' the predictors as character vector. Length 1939
fulltraining <- names(training)[names(training) != "Class"]

# Remove highly correlated predictors. Reduced predictors' length: 1843
corrPred <- cor(training[,fulltraining])
highCorr <- findCorrelation(corrPred, .99)
fulltraining2 <- fulltraining[-highCorr]

library(reshape2)
meltedCorr <- melt(corrPred)
head(meltedCorr)

# Confusion matrix for predictor correlation
ggplot(data = meltedCorr, mapping = aes(x = Var1, y = Var2, fill = value)) +
        geom_raster() +
        theme(axis.text        = element_blank(),
              axis.ticks       = element_blank(),
              axis.title       = element_blank(),
              panel.background = element_blank())
        
#Aggressive near zero variance removal
isNZV <- nearZeroVar(training[,fulltraining2], saveMetrics = TRUE, freqCut = floor(nrow(training)/5))
fullSet <-  rownames(subset(isNZV, !nzv))
str(fullSet)

reducedSet <- rownames(subset(isNZV, !nzv & freqRatio < floor(nrow(training)/50)))

# Removing collinear predictors. Removing one more.
fullSet2 <- fullSet[(fullSet != "PubTotal") &
                                 (fullSet != "num_People") &
                                 (fullSet != "Mar") &
                                 (fullSet != "Sun")
                         ]

reducedSet <- reducedSet[(reducedSet != "PubTotal") &
                            (reducedSet != "num_People") &
                            (reducedSet != "Mar") &
                            (reducedSet != "Sun")
                    ]

# Logistic regression on pre-2008 rows only on Day of Year
LogregModel1 <- glm(Class ~ Day, data = training[pre2008, ], family = binomial)  
LogregModel1
successProb <- 1 - predict(LogregModel1, newdata = data.frame(Day = 0:365), type = 'response')
#Need to overlay over scatter between days and success
plot(successProb, ylab = 'Probability of Success')

set.seed(42)
ctrl <- trainControl(method = 'LGOCV', summaryFunction = twoClassSummary, classProbs = TRUE,
                     index = list(TrainSet = pre2008), savePredictions = TRUE)
                     
LogRegFull <- train(training[ ,fullSet], y = training$Class, method = 'glm',
                    metric = 'ROC', trControl = ctrl)
LogRegFull

LogRegReduced <- train(training[ ,reducedSet], y =training$Class, method = 'glm',
                    metric = 'ROC', trControl = ctrl)
LogRegReduced

# ReducedROC <- roc(response = LogRegReduced$pred$obs,
#                   predictor = LogRegReduced$pred$successful, 
#                   levels = rev(levels(LogRegReduced$pred$obs)))
# 
# plot(ReducedROC, legacy.axes = TRUE)
# auc(ReducedROC)


FullROC <- roc(response = LogRegFull$pred$obs,
                  predictor = LogRegFull$pred$successful, 
                  levels = rev(levels(LogRegFull$pred$obs)))

plot(FullROC, legacy.axes = TRUE)
auc(FullROC)

# Test (Not very useful in this case)
resamp <- resamples(list(LogisticR = LogRegReduced, LogisticF = LogRegFull))
summary(resamp)
modelDifferences <- diff(resamp)
summary(modelDifferences)
xyplot(resamp)

# PLSDA
plsdaModelRed <- plsda(x = training[pre2008,reducedSet], y = training[pre2008, "Class"],
                       scale = TRUE, probMethod = "Bayes", ncomp = 4, validation = "LOO")
plsPred <- predict(plsdaModelRed, newdata = training[-pre2008, reducedSet])
plsProbs <- predict(plsdaModelRed, newdata = training[-pre2008, reducedSet]
                    , type = 'prob')

plot(RMSEP(plsdaModelRed), legendpos = "topright")

plsFitRed <- train(x = training[, reducedSet], y = training$Class, 
                   method = "pls", tuneGrid = expand.grid(.ncomp = 1:10), 
                   preProc = c("center","scale"), metric = "ROC", trControl = ctrl)
plsImpGrant <- varImp(plsFitRed, scale = FALSE)
plsImpGrant
plot(plsImpGrant, top = 20, scales = list(y = list(cex = .95)))

# SVM

sigmaRangeReduced <- sigest(as.matrix(training[,reducedSet]))
svmRGridReduced <- expand.grid(.sigma = sigmaRangeReduced[1], .C = 2^(seq(-4, 4)))
svmRedModel1 <- train(training[,reducedSet], training$Class, method = "svmRadial", metric = "ROC", 
                   preProc = c("center", "scale"), tuneGrid = svmRGridReduced, fit = FALSE, 
                   trControl = ctrl)
svmRedModel1

# Changing the training set
set.seed(42)
inTrain2 <- createDataPartition(final$Class, p = 3/4)[[1]]
trainPredictors <- final[inTrain2, ]
trainClasses <- final$Class[inTrain2]
ctrl2 <- trainControl(method = 'LGOCV', summaryFunction = twoClassSummary, classProbs = TRUE,
                     index = list(TrainSet = pre2008), savePredictions = TRUE)
sigmaRangeReduced2 <- sigest(as.matrix(training[pre2008,reducedSet]))
svmRGridReduced2 <- expand.grid(.sigma = sigmaRangeReduced[1], .C = 2^(seq(-4, 4)))
svmRedModel2 <- train(training[pre2008, reducedSet], training[pre2008, "Class"], method = "svmRadial", metric = "ROC", 
                      preProc = c("center", "scale"), tuneGrid = svmRGridReduced, fit = FALSE, 
                      trControl = ctrl2)
svmRedModel2

svmPCM1 <- confusionMatrix(svmRedModel1, norm = "none")
svmPCM2 <- confusionMatrix(svmRedModel2, norm = "none")

svmPRoc <- roc(response = svmRedModel1$pred$obs,
               predictor = svmRedModel1$pred$successful,
               levels = rev(levels(svmRedModel1$pred$obs)))

svmPRoc2 <- roc(response = svmRedModel2$pred$obs,
               predictor = svmRedModel2$pred$successful,
               levels = rev(levels(svmRedModel2$pred$obs)))

plot(svmPRoc, type = "s", add = TRUE, legacy.axes = TRUE)

# Random Forest
mtryValues <- c(5, 10, 20, 32, 50, 100, 250, 500, 1000)
set.seed(42)
rfFit <- train(x = training[,reducedSet], y = training$Class, method = "rf", ntree = 500,
               tuneGrid = data.frame(mtry = mtryValues), importance = TRUE, metric = "ROC",
               trControl = ctrl)
varImpPlot(rfFit)

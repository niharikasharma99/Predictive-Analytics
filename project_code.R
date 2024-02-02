##1
### Read required packages
library("data.table")
library("ggplot2")
library("psych")
library("stringr")
library("vctrs")
library("tibble")
library("caret")
library("stats")
library("dplyr")

library("rlang")



##reading data
hospital_data=fread('hospital_readmissions.csv')



#statistical analysis

describeBy(hospital_data, group=NULL)



##
hospital_data$age_t <- gsub("[", "(", hospital_data$age, fixed=TRUE)
unique(hospital_data$age_t)
hospital_data$age_t_1<-as.integer(factor(hospital_data$age_t),levels=c("(70-80)","(50-60)","(60-70)","(40-50)","(80-90)","(90-100)"))

##checking unique values for categorical columns
unique(hospital_data$glucose_test)
unique(hospital_data$A1Ctest)
hospital_data$glucose_test_t <- as.integer(factor(hospital_data$glucose_test, levels = c("no", "normal", "high")))
hospital_data$A1Ctest_t <- as.integer(factor(hospital_data$A1Ctest, levels = c("no", "normal", "high")))

# making dummies 
hospital_data$change_t <- as.integer(factor(hospital_data$change) )
hospital_data$readmitted_t <- 0
hospital_data$readmitted_t[hospital_data$readmitted=='yes']=1
hospital_data$diabetes_med_t <- as.integer(factor(hospital_data$diabetes_med))

unique(hospital_data$medical_specialty)
hospital_data$medical_specialty_t <- as.integer(factor(hospital_data$medical_specialty,levels = c("Missing" ,"Other",
                                                                                    "InternalMedicine",
                                                                                    "Family/GeneralPractice",
                                                                                    "Cardiology","Surgery",
                                                                                    "Emergency/Trauma")))
unique(hospital_data$diag_1)
hospital_data$diag_1_t <- as.integer(factor(hospital_data$diag_1, levels = c("Circulatory","Other","Injury","Digestive","Respiratory","Diabetes","Musculoskeletal", "Missing")))


unique(hospital_data$diag_2)
hospital_data$diag_2_t <- as.integer(factor(hospital_data$diag_2, levels = c("Circulatory","Other","Injury","Digestive","Respiratory","Diabetes","Musculoskeletal", "Missing")))

unique(hospital_data$diag_3)
hospital_data$diag_3_t <- as.integer(factor(hospital_data$diag_3, levels = c("Circulatory","Other","Injury","Digestive","Respiratory","Diabetes","Musculoskeletal", "Missing")))



##creating a subset dataframe with all the predictor variables
data_sub <- hospital_data[,!c("age","age_t","medical_specialty","diag_1","diag_2","diag_3","glucose_test","A1Ctest","change","diabetes_med","readmitted")]


##Dividing data into train and split data
set.seed(162807)
trainIndex <- createDataPartition(data_sub$readmitted_t, p = 0.7, list = FALSE)
trainData <- data_sub[trainIndex, ]
testData <- data_sub[-trainIndex, ]

##Approach 1 Feature selection using step function
##Applying logistic regression
glm_model <- glm(readmitted_t~.,family=binomial(link="logit"),data=trainData)
summary(glm_model)
step(glm_model)

##Glm model with only significant features
modify_model <- glm(readmitted_t ~ time_in_hospital + n_procedures + n_outpatient + 
                      n_inpatient + n_emergency + age_t_1 + diabetes_med_t + medical_specialty_t + 
                      diag_2_t + diag_3_t,family=binomial(link="logit"),data=trainData)
summary(modify_model)


##Predicting the values of readmission on Test data
predicted_readmission <- predict(modify_model,newdata=testData,type="response")


##probability of success and failure cutoff
mbrCutoff<-0.5
##Creating Classification and Confusion Matrix 

TstCls <- predicted_readmission*0
TstCls[predicted_readmission >= mbrCutoff] <- 1
(llogitTst <- table(TstCls,testData$readmitted_t, dnn=c("Predicted","Actual")) )

##Accuracy for glm
accuracy <- sum(diag(llogitTst))/sum(llogitTst)

##Decision Tree code
require("partykit")
dtreemodel<-ctree(readmitted_t~time_in_hospital + n_procedures + n_outpatient + 
                    n_inpatient + n_emergency + age_t_1 + diabetes_med_t + medical_specialty_t + 
                    diag_2_t + diag_3_t,data=trainData)
plot(dtreemodel)

##predicting y values on test data based on decision tree
dTreepredTst <- predict(dtreemodel,newdata=testData)

##creating classification and confusion matrix
dTreecls <- dTreepredTst*0
dTreecls[dTreepredTst >= mbrCutoff] <- 1
(dTreeTst <- table(dTreecls,testData$readmitted_t,dnn=c("Predicted","Actual")))

##Accuracy for decision tree
(accuracy_dtree <- sum(diag(dTreeTst))/sum(dTreeTst))







library(gridExtra)

library(ggplot2)
options(repr.plot.width=20, repr.plot.height=25)
ggtitle("Analysis Of Variable readmitted",
        subtitle = "Variable n_procedures, medical_specialty, diag_1, diag_2, and diag_3") +
  theme(plot.title = element_text(face = "bold", size = 20))
p1 <- ggplot(hospital_data, aes(x = n_procedures, fill = readmitted)) +
  geom_bar(position = "dodge") +
  labs(title = "Variable n_procedures") +
  theme(plot.title = element_text(face = "bold"))

#Create the second subplot
p2 <- ggplot(hospital_data, aes(x = medical_specialty, fill = readmitted)) +
  geom_bar(position = "dodge") +
  labs(title = "Variable medical_specialty") +
  theme(plot.title = element_text(face = "bold"))

#Create the third subplot
p3 <- ggplot(hospital_data, aes(x = diag_1, fill = readmitted)) +
  geom_bar(position = "dodge") +
  labs(title = "Variable diag_1")

#Create the fourth subplot
p4 <- ggplot(hospital_data, aes(x = diag_2, fill = readmitted)) +
  geom_bar(position = "dodge") +
  labs(title = "Variable diag_2")

#Create the fifth subplot
p5 <- ggplot(hospital_data, aes(x = diag_3, fill = readmitted)) +
  geom_bar(position = "dodge") +
  labs(title = "Variable diag_3")

#Arrange the subplots in a grid
grid.arrange(p1, p2, p3, p4, p5, ncol = 1)


# continous variable scatter plot 

library(ggplot2)

ggplot(hospital_data, aes(x = time_in_hospital, y = readmitted)) +
  geom_point() +
  labs(title = "Analysis Of Variable readmitted", 
       subtitle = "", 
       x = "Time in Hospital", 
       y = "Readmitted") +
  theme(plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  facet_wrap(~readmitted, nrow = 3, ncol = 2)

ggplot(hospital_data, aes(x = n_lab_procedures, y = readmitted)) +
  geom_point() +
  labs(title = "", 
       subtitle = "", 
       x = "Number of Lab Procedures", 
       y = "") +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank()) +
  facet_wrap(~readmitted, nrow = 3, ncol = 2)

ggplot(hospital_data, aes(x = n_procedures, y = readmitted)) +
  geom_point() +
  labs(title = "", 
       subtitle = "", 
       x = "Number of Procedures", 
       y = "") +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank()) +
  facet_wrap(~readmitted, nrow = 3, ncol = 2)

ggplot(hospital_data, aes(x = n_medications, y = readmitted)) +
  geom_point() +
  labs(title = "", 
       subtitle = "", 
       x = "Number of Medications", 
       y = "") +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank()) +
  facet_wrap(~readmitted, nrow = 3, ncol = 2)

ggplot(hospital_data, aes(x = n_outpatient, y = readmitted)) +
  geom_point() +
  labs(title = "", 
       subtitle = "", 
       x = "Number of Outpatient Visits", 
       y = "") +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank()) +
  facet_wrap(~readmitted, nrow = 3, ncol = 2)

ggplot(hospital_data, aes(x = n_emergency, y = readmitted)) +
  geom_point() +
  labs(title = "", 
       subtitle = "", 
       x = "Number of Emergency Visits", 
       y = "") +
  theme(plot.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank()) +
  facet_wrap(~readmitted, nrow = 3, ncol = 2)


#confusion Matrix 
library(rpart)
library(caret)

decision_tree <- rpart(y_train ~ ., data = X_train, method = "class", parms = list(split = "information"), control = rpart.control(minsplit = 5, maxdepth = 6, cp = 0))
previsoes <- predict(decision_tree, X_test, type = "class")

cm <- confusionMatrix(factor(previsoes), factor(y_test))
cm$overall['Accuracy']


#ouliers scatterplot 
library(ggplot2)
install.packages(rlang)

library(rlang)

ggplot(hospital_data, aes(x = time_in_hospital)) + geom_point()+labs(title = "Scatter plot of time_in_hospital", 
       x = "Time in Hospital", 
       y = "Frequency")
last_error()

# continuous variable in BoxBolt 

library(ggplot2)
library(RColorBrewer)

# Set color palette
color_palette <- brewer.pal(n = 8, name = "Set2")

# Set figure size and title
options(repr.plot.width = 25, repr.plot.height = 20)
ggtitle_main <- theme(plot.title = element_text(face="bold", size=20))
p <- ggtitle("Analysis Of Variable readmitted") + ggtitle_main

# Create subplots with added color
p1 <- ggplot(hospital_data, aes(x=readmitted, y=time_in_hospital)) + 
  geom_boxplot(fill=color_palette[1], color=color_palette[1]) + 
  ggtitle("time_in_hospital") + ggtitle_main
p2 <- ggplot(hospital_data, aes(x=readmitted, y=n_lab_procedures)) + 
  geom_boxplot(fill=color_palette[3], color=color_palette[1]) + 
  ggtitle("n_lab_procedures") + ggtitle_main
p3 <- ggplot(hospital_data, aes(x=readmitted, y=n_procedures)) + 
  geom_boxplot(fill=color_palette[5], color=color_palette[4]) + 
  ggtitle("n_procedures") + ggtitle_main
p4 <- ggplot(hospital_data, aes(x=readmitted, y=n_medications)) + 
  geom_boxplot(fill=color_palette[7], color=color_palette[8]) + 
  ggtitle("n_medications") + ggtitle_main
p5 <- ggplot(hospital_data, aes(x=readmitted, y=n_outpatient)) + 
  geom_boxplot(fill=color_palette[1], color=color_palette[2]) + 
  ggtitle("n_outpatient") + ggtitle_main
p6 <- ggplot(hospital_data, aes(x=readmitted, y=n_emergency)) + 
  geom_boxplot(fill=color_palette[3], color=color_palette[4]) + 
  ggtitle("n_emergency") + ggtitle_main

# Arrange subplots
p_final <- gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2, 
                                   top=p, widths=c(2,2))
print(p_final)


# Bivariate Analysis #
library(ggplot2)

# Set the plot dimensions
options(repr.plot.width = 20, repr.plot.height = 15)

# Create a function to plot the count plots for each variable
plot_count <- function(x_var, y_var, title){
  ggplot(hospital_data, aes(x = {{x_var}}, fill = readmitted)) +
    geom_bar(position = 'dodge') +
    ggtitle(title) +
    labs(x = deparse(substitute(x_var)), y = 'Count') +
    scale_fill_manual(values = c('maroon', 'pink')) +
    theme_classic() +
    theme(legend.position = 'top')
}

# Create a grid of subplots using the plot_count function
grid.arrange(
  plot_count(age, readmitted, 'Variable age'),
  plot_count(glucose_test, readmitted, 'Variable glucose_test'),
  plot_count(A1Ctest, readmitted, 'Variable A1Ctest'),
  plot_count(change, readmitted, 'Variable change'),
  plot_count(diabetes_med, readmitted, 'Variable diabetes_med'),
  nrow = 3, ncol = 2
)



















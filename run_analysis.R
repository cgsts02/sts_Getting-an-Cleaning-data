install.packages("tidyr")
install.packages("dplyr")

library(tidyr)
library(dplyr)

#We load the packages "tidyr" and "dplyr", which are the necessary ones to carry out the cleaning work

Features <- read.csv("features.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)

#Structure of data.frame features.txt: 561 obs. of two variables (V1: 1,2,3,4...) and (V2: chr "tBodyAcc-mean()-X", tBodyAcc-mean()-Y"),...

activity_labels <- read.csv("activity_labels.txt", sep = "", header = FALSE)

#Structure of data.frame activity_labels.txt:6 obs. of two variables (V1: 1,2,3,4...) and (V2:"LAYING", "SITTING", "WALKING_UPSTAIRS",...)

#Reading, train sets

testset <- read.csv("X_test.txt", sep = "", header = FALSE)

#Structure of data.frame X_test.txt: 2947 entries of 561 variables (v1: 0.257, 0.28,..),...(V561: -0.0579, -0.0838)

trainset <- read.csv("X_train.txt", sep = "", header = FALSE)



#Structure of data.frame X_train.txt: 7352 entries of 561 variables (V1: 0.257, 0.286,...),...(V561: -0.058, -0.054,..)

merged_training_test <- rbind(testset,trainset)

#Structure of data.frame rbind(testset,trainset): combination of two variables. 10299 entries of 561 variables (V1: 0.257, 0.286,...),....,(V561: -0.0579, -0.0838)

#             Reading, train Movement Sets

testmoves <- read.csv("y_test.txt", sep = "", header = FALSE)

#Structure of data.frame y_test.txt: one variables. 2947 entries of 1 variable (V1: 5,5,5,....)

trainmoves <- read.csv("y_train.txt", sep = "", header = FALSE)

#Structure of data.frame y_train.txt: one variables. 7352 entries of 1 variable (V1: 5,5,5,....)

merged_movement <- rbind(testmoves,trainmoves)

#Structure of data.frame rbind(testmoves,trainmoves): combination of two variables. 10299 entries of 561 variables (V1: 5,5,5,...)

testperson <- read.csv("subject_test.txt", sep = "", header = FALSE)

#Structure of data.frame subject_test.txt: one variables. 2947 entries of 1 variable (V1: 2,2,2,....)

trainperson <- read.csv("subject_train.txt", sep = "", header = FALSE)

#Structure of data.frame subject_train.txt: one variables. 7352 entries of 1 variable (V1: 1,1,1,....)

all_person <- rbind(testperson,trainperson)

#Structure of data.frame : one variables. 10299 entries of 1 variable (V1: 2,2,2,....)

labelled_movements <- merge(merged_movement,activity_labels)

#Structure of data.frame : one variables. 10299 entries of 2 variable (V1: 1,1,1,....) and (V2: WALKING,...)

locations <- grep(pattern = "-mean|-std", x =Features[,c(2)])

req_features <- grep(pattern = "-mean|-std", x =Features[,c(2)],value = TRUE)

#Structure of data.frame req_features: one variables. 70 entries of 1 variable (V1: tBodyAcc-mean()-X, tBodyAcc-mean()-Y,..)

obj_one <- merged_training_test[locations]

#Structure of data.frame obj_one: one variable. 10299 entries of 79 variables

obj_two <- t(req_features)

names(obj_one) <- obj_two

obj_three <- bind_cols(all_person,obj_one)

#Structure of data.frame obj_three: one variable. 10299 entries of 79 variables, (V1: 2,2,2,2....), tBodyAcc-mean()-X: 0.257,0.286, tBodyAcc-mean()-Y: -0.023,-0.0132 

obj_four <- bind_cols(labelled_movements[2], obj_three)

names(obj_four)[1]="Activity"
names(obj_four)[2]="Person_Number"

#Extracting the means of all the variables for all person and activities

summer <- summarise_all(group_by(obj_four,Activity,Person_Number),funs(mean))
order_summer <- summer[order(summer$Person_Number),]

#Output

write.table(summer[order(summer$Person_Number),],file="clean_and_tidied.txt",row.names = FALSE)


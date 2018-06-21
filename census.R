adult <- read.table("adult.data.txt", stringsAsFactors = F)
sapply(adult, function(x) sum(is.na(x)))
is.na(adult)
na.omit(adult)
colnames(adult) <-c("Age","Class", "Fnlwgt", "Work", "Educ", "Marital", "Job", "Relationship", 
                    "Race", "sex", "Gain", "Loss", "Hours", "Country", "50")
str(adult)
adult <-adult[complete.cases(adult),]
adult$Age<-as.numeric(gsub(",", "", adult$Age))
adult$Age <-as.numeric(adult$Age)

adult$Class <-as.factor(gsub(",", "", adult$Class))

adult$Fnlwgt <-as.numeric(gsub(",", "", adult$Fnlwgt))

adult$Work <-as.factor(gsub(",", "", adult$Work))

adult$Educ <-as.factor(gsub(",", "", adult$Educ))

adult$Marital <-as.factor(gsub(",", "", adult$Marital))

adult$Job <-as.factor(gsub(",", "", adult$Job))

adult$Relationship <-as.factor(gsub(",", "", adult$Relationship))

adult$Race <- as.factor(gsub(",", "", adult$Race))

adult$sex <- as.factor(gsub(",", "", adult$sex))

adult$Gain <- as.numeric(gsub(",", "", adult$Gain))

adult$Loss <- as.numeric(gsub(",", "", adult$Loss))

adult$Hours <-as.numeric(gsub(",", "", adult$Hours))

adult$Country <-as.character(gsub(",", "", adult$Country))
str(adult)

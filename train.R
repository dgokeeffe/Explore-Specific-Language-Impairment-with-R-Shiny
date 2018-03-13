library(caret)
library(tidyverse)
library(doMC)

# Register number of cores ---------------------------------------
registerDoMC(cores = 6)

# Read in the data -----------------------------------------------
all3 <- read_csv("/home/kazooy/SLI/data/all_data_R_high_cor_removed.csv", col_names = T, trim_ws = T)

# Shuffle data ---------------------------------------------------
all3 <- all3[sample(nrow(all3)),]

# Split on corpora -----------------------------------------------
conti4 <- all3 %>% filter(corpus == "Conti4")
enni <- all3 %>% filter(corpus == "ENNI")
gillam <- all3 %>% filter(corpus == "Gillam")

# Remove the 

# Train the models -----------------------------------------------
set.seed(98)
fitControl <- trainControl(## Repeating k-fold 10 CV
    method = "cv",
    number = 10,
    classProbs = TRUE,
    allowParallel = TRUE,
    summaryFunction = twoClassSummary,
    search = "random",
    # Try subsampling for class imbalance
    sampling = "smote")

# Print out the proportions for each dataset
print('Conti4')
prop.table(table(conti4$group))
print('ENNI')
prop.table(table(enni$group))
print('Gillam')
prop.table(table(gillam$group))
print('All')
prop.table(table(all3$group))

# Random Forest --------------------------------------------------
train_rf <- function(df, factorize_sex = TRUE){
  # Select numerics
  if(factorize_sex == TRUE){
    df <- mutate(df, sex = as.numeric(factor(sex)))
  } else {
    df <- select(df, -sex)
  } 
  df <- select(df, -filename, -corpus)
  rf.fit <- train(as.factor(group) ~ .,data = df, 
                  method = "rf", 
                  trControl = fitControl,
                  preProcess = c("nzv", "center", "scale"),
                  tuneLength = 8,
                  metric = "ROC")
  return(rf.fit)
}

# Train ----------------------------------------------------------- 
conti4.rf <- train_rf(conti4, FALSE)
enni.rf <- train_rf(enni)
gillam.rf <- train_rf(gillam)
all3.rf <- train_rf(all3, FALSE)

# 

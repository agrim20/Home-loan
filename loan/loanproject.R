dataset = read.csv('train.csv')
# Taking care of missing data
dataset$LoanAmount = ifelse(is.na(dataset$LoanAmount),
                     ave(dataset$LoanAmount, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$LoanAmount)
dataset$Loan_Amount_Term = ifelse(is.na(dataset$Loan_Amount_Term),
                        ave(dataset$Loan_Amount_Term, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Loan_Amount_Term)
dataset$Credit_History = ifelse(is.na(dataset$Credit_History),
                                  ave(dataset$Credit_History, FUN = function(x) mean(x, na.rm = TRUE)),
                                  dataset$Credit_History)
dataset$Gender = ifelse(is.na(dataset$Gender),
                                ave(dataset$Gender, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$Gender)
dataset$Married = ifelse(is.na(dataset$Married),
                                ave(dataset$Married, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$Married)
dataset$Self_Employed = ifelse(is.na(dataset$Self_Employed),
                                ave(dataset$Self_Employed, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$Self_Employed)
dataset$Property_Area = factor(dataset$Property_Area,
                         levels = c('Urban', 'Semiurban', 'Rural'),
                         labels = c(1, 2, 3))
dataset$Loan_Status = factor(dataset$Loan_Status,
                         levels = c('Y', 'N'),
                         labels = c(1,0))
dataset$Gender = factor(dataset$Gender,
                         levels = c('Male', 'Female'),
                         labels = c(1, 2))
dataset$Married = factor(dataset$Married,
                         levels = c('Yes', 'No'),
                         labels = c(1, 2))
dataset$Education = factor(dataset$Education,
                         levels = c('Graduate', 'Not Graduate'),
                         labels = c(1, 0))
dataset$Self_Employed = factor(dataset$Self_Employed,
                         levels = c('No', 'Yes'),
                         labels = c(0,1))
dataset = dataset[2:13]
library(rpart)
classifier = rpart(formula = Loan_Status ~ .,
                   data = dataset)

y_pred = predict(classifier, newdata = dataset[-12], type = 'class')
cm = table(dataset[, 12], y_pred)
datasettest= read.csv('test.csv')
datasettest$LoanAmount = ifelse(is.na(datasettest$LoanAmount),
                            ave(datasettest$LoanAmount, FUN = function(x) mean(x, na.rm = TRUE)),
                            datasettest$LoanAmount)
datasettest$Loan_Amount_Term = ifelse(is.na(datasettest$Loan_Amount_Term),
                                  ave(datasettest$Loan_Amount_Term, FUN = function(x) mean(x, na.rm = TRUE)),
                                  datasettest$Loan_Amount_Term)
datasettest$Credit_History = ifelse(is.na(datasettest$Credit_History),
                                ave(datasettest$Credit_History, FUN = function(x) mean(x, na.rm = TRUE)),
                                datasettest$Credit_History)
datasettest$Gender = ifelse(is.na(datasettest$Gender),
                        ave(datasettest$Gender, FUN = function(x) mean(x, na.rm = TRUE)),
                        datasettest$Gender)
datasettest$Married = ifelse(is.na(datasettest$Married),
                         ave(datasettest$Married, FUN = function(x) mean(x, na.rm = TRUE)),
                         datasettest$Married)
datasettest$Self_Employed = ifelse(is.na(datasettest$Self_Employed),
                               ave(datasettest$Self_Employed, FUN = function(x) mean(x, na.rm = TRUE)),
                               datasettest$Self_Employed)
datasettest$Property_Area = factor(datasettest$Property_Area,
                               levels = c('Urban', 'Semiurban', 'Rural'),
                               labels = c(1, 2, 3))
datasettest$Gender = factor(datasettest$Gender,
                        levels = c('Male', 'Female'),
                        labels = c(1, 2))
datasettest$Married = factor(datasettest$Married,
                         levels = c('Yes', 'No'),
                         labels = c(1, 2))
datasettest$Education = factor(datasettest$Education,
                           levels = c('Graduate', 'Not Graduate'),
                           labels = c(1, 0))
datasettest$Self_Employed = factor(datasettest$Self_Employed,
                               levels = c('No', 'Yes'),
                               labels = c(0,1))
y_pred2 = predict(classifier, newdata = datasettest, type = 'class')
write.csv(y_pred2,file='loan.csv')

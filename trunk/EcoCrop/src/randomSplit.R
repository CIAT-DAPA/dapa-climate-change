#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Description: separate an input dataset into training and test datasets. Will simply add a column with "Test" for test points and
#"Train" for training datapoints. This is done only using those 

randomSplit <- function(dataset, per=20, seed=1234) {
	if (per < 5 | per > 50) {
		stop("Please change your test percentage, it should be between 5 and 50%")
	}
	
	if (nrow(dataset) < 10) {
		stop("The input dataset has an insufficient number of rows, should have at least 10")
	}
	
	ntest <- round(nrow(dataset)*per/100, 0)
	ntrain <- nrow(dataset) - ntest
	
  set.seed(seed)
	test <- sample(1:nrow(dataset), ntest)
	train <- c(1:nrow(dataset))[-test]
	
	dataset$TEST_TRAIN <- NA
	dataset$TEST_TRAIN[test] <- "TEST"
	dataset$TEST_TRAIN[train] <- "TRAIN"
	
	return(dataset)
}

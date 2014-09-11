library("e1071")
library("MASS")

#Create a function to read libsvm file (Reference: http://stackoverflow.com/questions/12112558/read-write-data-in-libsvm-format)
require(Matrix)
read.libsvm = function( filename ) {
  content = readLines( filename )
  num_lines = length( content )
  tomakemat = cbind(1:num_lines, -1, substr(content,1,1))
 
  # loop over lines
  makemat = rbind(tomakemat,
                  do.call(rbind,
                          lapply(1:num_lines, function(i){
                            # split by spaces, remove lines
                            line = as.vector( strsplit( content[i], ' ' )[[1]])
                            cbind(i, t(simplify2array(strsplit(line[-1],
                                                               ':'))))  
                          })))
  class(makemat) = "numeric"
 
  #browser()
  yx = sparseMatrix(i = makemat[,1],
                    j = makemat[,2]+2,
                    x = makemat[,3])
  return( yx )
}

# read data using created function and store into sparce matrix format
data <- read.libsvm("/home/likewise-open/EZDI-DOMAIN/ssoni/Header_footer/Word_shape/_R_data/train(2)") 
data <- as.data.frame.matrix(data)
data
data1 <- read.matrix.csr("/home/likewise-open/EZDI-DOMAIN/ssoni/Header_footer/Word_shape/_R_data/test2.libsvm", fac=TRUE, ncol=NULL);
data1
names(data)[1] <- "label"
data$label <- as.factor(data$label)
model <- svm(data$label~.,data=data,scale = FALSE, kernel="radial", gamma=0.001, cost=215,)
print(model)
test<- read.libsvm("/home/likewise-open/EZDI-DOMAIN/ssoni/Header_footer/Word_shape/_R_data/test(1)")
test <- as.data.frame.matrix(test)

names(test)[1] <- "label"
test$label <- as.factor(test$label)
predict_label <- predict(model,newdata=test[,-1],type="response")
predict_label
c_table <- ftable(test$label,predict_label)
c_table
accuracy <- sum(diag(c_table)) * 100/nrow(test)
print(accuracy)

library(relimp, pos=4)


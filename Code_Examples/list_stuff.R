library(tidyverse)


A<- 1:10
B<- letters
C<-c(TRUE,TRUE,FALSE)

x<-list(A,B,C)
x
 x[[1]]

 x[[3]]

 x[[1]][3]
 
 for (i in 1:3){
   print(x[[i]][1])}
 
#PURR
map(x,1)
map_chr(x,1)

y<- list(a=iris,
         b=mtcars)
y

map(y,class)
#mapped function class to everything in that list
map(x,class)
#function that takes first and second columns and multiplys them to make new column
#named products
#double is first data frame, single bracket is columns and rows
make_products<-
  function(x){
    if(!is.numeric(x[,1])){
      stop("hey idiot, the first column isnt numeric")
   x[,1] <-as.numeric(x[,1])
   }
newcol <- x[,1] * x[,2]
x["products"] <- newcol
return(x)
  }
y$a$Sepal.Length <-as.numeric(y$a$Sepal.Length)

#dataframes are lists of the same length
map(y,make_products)
# map successor to lapply
#can take any data frame to make a products column of the first 2
#can change x to specify which dataframe in the list we want it to act on 

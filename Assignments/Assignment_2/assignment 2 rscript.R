#had to find files and make sure the data was working
list.files(path="Data")
csv_files <- list.files(path="Data",pattern = ".csv" ,recursive = TRUE ,full.names = TRUE)
length(csv_files) #checking how many there are
df <- read.csv("Data/wingspan_vs_mass.csv")
head(df, n = 5)
#using the head function on a specific csv file
#finding files that start with b 
list.files(path="Data",pattern = "^b", recursive = TRUE)
bfiles <- list.files(path="Data",pattern = "^b", recursive = TRUE, full.names = TRUE)
for(i in bfiles){
  print(readLines(i, n=1))
}
x <- 1:10
for(i in x){
  (print(i*2))}
#testing the for loop
for(i in csv_files){
  print(readLines(i, n=1))
}


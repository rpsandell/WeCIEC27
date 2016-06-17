
# Predicting -mant- and -vant-


setwd("/Users/rpsandell/Desktop/Research Projects/WeCIEC27") # Navigate to directory
mant.vant.table <- read.table("RVMantVant.txt", header=T, sep="\t") # Read in tab-separated values data
library(party) # load party package for conditional inference regression trees
colnames(mant.vant.table) <- c("IsVant", "Form", "Base Syllables", "Base Accent", "FS [-consonantal]", "FS [+high]", "FS [+long]", "FS [+round]", "FS [+coronal]", "FS [+anterior]", "FS [+back]", "FS [+continuant]", "FS [+nasal]", "FS [+sonorant]", "2S [+cor,-ant,-son,-cont]", "2S[+high,-cons]")
# Set the column names to more readable values

vant.tree <- ctree(IsVant ~ ., data= mant.vant.table[, -c(2)]) # Train the tree, ignoring the 2nd column that has the names of the forms
plot(vant.tree) # display the image of the CIR tree

treepredicts <- predict(vant.tree) # get predictions

# Set predicted probabilities of -vant- less than 0.5 to 0
for(i in 1:length(treepredicts)){
  if(treepredicts[i] < 0.5){
    treepredicts[i] = 0
  }
}

# Set predicted probabilities of -vant- greater than 0.5 to 1
for(i in 1:length(treepredicts)){
  if(treepredicts[i] > 0.5){
    treepredicts[i] = 1
  }
}

num.Correct.Tree = 0
length(treepredicts)
length(mant.vant.table$IsVant)

# Get number of correct predictions
for(i in 1:283){
  if(treepredicts[i] == mant.vant.table$IsVant[i]){
    num.Correct.Tree = num.Correct.Tree + 1
  }
}

# Accuracy 
num.Correct.Tree/283 
mant.vant.table <- cbind(mant.vant.table, treepredicts)


# Recall on -mant- forms
tree.mant.recall <- length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,17] == 0))) / (length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,17] == 0))) + length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,17] == 1))))
# Precision on -mant- forms
tree.mant.precision <- length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,17] == 0))) / (length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,17] == 0))) + length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,17] == 0))))

# Recall on -vant- forms
tree.vant.recall <- length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,17] == 1))) / (length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,17] == 1))) + length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,17] == 0))))
# Precision on -vant- forms
tree.vant.precision <- length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,17] == 1))) / (length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,17] == 1))) + length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,17] == 1))))

tree.precision.recall.frame <- data.frame(Precision = c(tree.mant.precision, tree.vant.precision), Recall= c(tree.mant.recall, tree.vant.recall), row.names = c("-mant-", "-vant-"))
print(tree.precision.recall.frame)

#Logistic Regression Model
library(arm) # load arm package for bayesglm function
mant.vant.table <- read.table("RVMantVant.txt", header=T, sep="\t") # read in a clean version of the data
vant.glm <- step(bayesglm(IsVant ~ ., data= mant.vant.table[-c(2)])) # Use step() function called on bayesglm() to do step-wise regression on AIC
summary(vant.glm) # display summary results of the model

glmpredicts <- predict(vant.glm) # get predictions

# Set predicted probabilities of -vant- less than 0.5 to 0
for(i in 1:length(glmpredicts)){
  if(glmpredicts[i] < 0.5){
    glmpredicts[i] = 0
  }
}

# Set predicted probabilities of -vant- greater than 0.5 to 1
for(i in 1:length(glmpredicts)){
  if(glmpredicts[i] > 0.5){
    glmpredicts[i] = 1
  }
}


num.Correct.GLM <- 0

# Get number of correct predictions
for(i in 1:283){
  if(glmpredicts[i] == mant.vant.table[, 1][i]){
    num.Correct.GLM = num.Correct.Tree + 1
  }
}
# Accuracy
num.Correct.GLM/283
mant.vant.table <- cbind(mant.vant.table, glmpredicts)

# Recall on -mant- forms
glm.mant.recall <- length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,18] == 0))) / (length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,18] == 0))) + length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,18] == 1))))
# Precision on -mant- forms
glm.mant.precision <- length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,18] == 0))) / (length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,18] == 0))) + length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,18] == 0))))

# Recall on -vant- forms
glm.vant.recall <- length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,18] == 1))) / (length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,18] == 1))) + length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,18] == 0))))
# Precision on -vant- forms
glm.vant.precision <- length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,18] == 1))) / (length(which((mant.vant.table[,1] == 1) & (mant.vant.table[,18] == 1))) + length(which((mant.vant.table[,1] == 0) & (mant.vant.table[,18] == 1))))

glm.precision.recall.frame <- data.frame(Precision = c(glm.mant.precision, glm.vant.precision), Recall= c(glm.mant.recall, glm.vant.recall), row.names = c("-mant-", "-vant-"))
print(glm.precision.recall.frame)


# Testing Arnold's Emendations of śaktīvant-
# 11-syllable śaktīvantaḥ
saktivantah.11 <- c(0,0,0,0,1,0,0,0)
HLHh.11 <- c(55,44,0,2,174,24,1,707)
HHHh.11 <- c(69,3,0,1,5,1,0,4)
sakti.HLHh.11 <- matrix(c(saktivantah.11, HLHh.11), nrow=2, byrow=T)
sakti.HHHh.11 <- matrix(c(saktivantah.11, HHHh.11), nrow=2, byrow=T)

fisher.test(sakti.HLHh.11)
fisher.test(sakti.HHHh.11)

# 12-syllable śaktīvaḥ
saktivah.12 <- c(1, 0,0,0,0,0,0,0,0,0)
HLh.12 <- c(23,90,40,1, 171, 73, 1, 315, 0, 854)
HHh.12 <- c(118,36,157,0,16,7,7,3,0,0)

sakti.HLh.12 <- matrix(c(saktivah.12, HLh.12), nrow=2, byrow=T)
sakti.HHh.12 <- matrix(c(saktivah.12, HHh.12), nrow=2, byrow=T)

fisher.test(sakti.HLh.12)
fisher.test(sakti.HHh.12)

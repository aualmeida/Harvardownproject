#Installing if necessary and loading the required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(mda)) install.packages("mda", repos = "http://cran.us.r-project.org")
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")

#Create an object to hold the URL address
URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00229/Skin_NonSkin.txt"

# Read file into object and insert column names
sknnon <- read_tsv(URL, col_names = c("B","G","R","Y"))

# View structure of the dataset
str(sknnon)

#View summary of the dataset
summary(sknnon)

# View first 5 rows of the dataset

head(sknnon,5)

# Convert column Y into factor for classification
sknnon$Y <- as_factor(sknnon$Y)

# check proportions
prop.table(table(sknnon$Y))

# Create histograms with ggplot for each parameter then use grid.arrange to present plots side by side
p1 <- sknnon %>% ggplot(aes(B))+
  geom_histogram(bins = 30, color = "white", fill = "blue")

p2 <- sknnon %>% ggplot(aes(G))+
  geom_histogram(bins = 30, color = "white", fill = "green")

p3 <- sknnon %>% ggplot(aes(R))+
  geom_histogram(bins = 30, color = "white", fill = "red")

grid.arrange(p1,p2,p3,ncol = 3)

# Create histograms with ggplot for each parameter then use grid.arrange to present plots side by side

d1 <- sknnon %>% ggplot(aes(B))+
  geom_density(fill = "blue", alpha = 0.5)

d2 <- sknnon %>% ggplot(aes(G))+
  geom_density(fill = "green", alpha = 0.5)

d3 <- sknnon %>% ggplot(aes(R))+
  geom_density(fill = "red", alpha = 0.5)

grid.arrange(d1,d2,d3,ncol = 3)

# Create scatter plots of the variables using Y values to color the points

s1 <- sknnon %>% ggplot(aes(B,G, color = Y))+
  geom_point()
s2 <- sknnon %>% ggplot(aes(G,R, color = Y))+
  geom_point()
s3 <- sknnon %>% ggplot(aes(R,B, color = Y))+
  geom_point()
grid.arrange(s1,s2,s3,ncol=3)

# Set seed 
set.seed(1,sample.kind = "Rounding")

# Create data partition
skindex <- createDataPartition(sknnon$Y,1,p=0.8,list = FALSE)

# Use index to create train and test objects
sktrain <- sknnon[skindex,]
sktest <- sknnon[-skindex,]

# Run a baseline classification model using the train function
set.seed(1,sample.kind = "Rounding")
modglm <- train(Y~B+G+R,
                data = sktrain,
                method = "glm")
modglm

# Use the trained model to generate predictions on the test subset of the sknnon dataset
yhatglm <- predict(modglm,sktest)


# Calculate Accuracy using the confusion matrix
confusionMatrix(yhatglm,sktest$Y)$overall["Accuracy"]

#using a linear discriminant analysis method to check predicted accuracy
set.seed(1,sample.kind = "Rounding")
modlda <- train(Y~B+G+R,method = "lda",data = sktrain)
modlda
yhatlda <- predict(modlda,sktest)
confusionMatrix(yhatlda,sktest$Y)$overall["Accuracy"]

#Using a penalised discriminant analysis method to check predicted accuracy
set.seed(1,sample.kind = "Rounding")
modpda <- train(Y~B+G+R,method = "pda",data = sktrain)
modpda
yhatpda <- predict(modpda,sktest)
confusionMatrix(yhatpda,sktest$Y)$overall["Accuracy"]

# Apply Quadratic discriminant analysis and gage predicted accuracy
set.seed(1,sample.kind = "Rounding")
modqda <- train(Y~B+G+R,method = "qda",data = sktrain)
modqda
yhatqda <- predict(modqda,sktest)
confusionMatrix(yhatqda,sktest$Y)$overall["Accuracy"]





















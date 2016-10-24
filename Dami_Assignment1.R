
# *********************************************
# DAMI Preprocessing Exercise R file
# Complete the codes to complete the assignment
# *********************************************

# 1. Import data for analysis to R environment
# Downloaded "Adult" dataset from UCI Machine Learning Repository
# URL http://archive.ics.uci.edu/ml/datasets/Adult
# Import dataset in adult_db
# Missing values are represented as "?" in data file, make sure that R read them as missing values (NAs)
# HINT: use read.table() function, use ?read.table for more help
# ------------------------------------------------------------------------------------------------------ #
adult_db <- read.table("adult.data",sep=",",strip.white=TRUE,na.strings = c("?"),stringsAsFactors=FALSE)


# 2. Assign attribute names (column names) to the data we just imported
# Attribute names are in separate file "adult.names", scroll down to the bottom of this file
# Attribute names such as ("age", "workclass", "fnlwgt",...)
# Last column of the dataset adult.db with values ">50K" and "<=50K" does not have name, 
# this is the class attribute, so we just name it as "class"
# ----------------------------------------------------------------------------------------- #
names(adult_db) = c("age",
                    "workclass",
                    "fnlwgt",
                    "education",
                    "education_num",
                    "marital_status",
                    "occupation",
                    "relationship",
                    "race",
                    "sex",
                    "capital_gain",
                    "capital_loss",
                    "hours_per_week",
                    "native_country",
                    "class")



# Inspect data set in tabular form
# -----------------------------
fix(adult_db)
# Change class labels to 1 (adults who earn more than 50K) and 0 (adults who earn less than or equal to 50K)
# ----------------------------------------------------------------------------------------------
adult_db$class[adult_db$class==">50K"] <- 1
adult_db$class[adult_db$class=="<=50K"] <- 0


# 3. Check for missing values
# Write code to check how many missing values each attribute has
# Hint: use "apply()" function along columns of "adult.db", for each column (attribute) find how many NAs are there
# is.na(x) function can be used to see if x has NA, ?is.na for help
# --------------------------------------------------------------------------------------------------------------- #
apply(adult_db,2,function(x) sum(is.na(x)))



# Delete records (rows) with any missing value
# --------------------------------------- #
adult_db_nomiss <- na.omit(adult_db)



# 4. We will take only small chunk of the data for our experimental purpose.
# So, randomly select 1000 records from among 30 thousand records in the dataset.
# ------------------------------------------------------------------------------- #
set.seed(1013)
idx = sample(1:nrow(adult_db_nomiss),1000)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL

fix(adult_db_lim)



# 5. Examine attributes of the dataset
# Plot histogram for numeric attribute "age", with 50 breaks, show main title and attribute name on the plot.
# Plot barchart for categorical attribute "race", show legend, attribute name and main title for the plot.

# HINT: use hist() function for plotting histogram, ?hist to see how to use it.
# HINT: use barplot() function for plotting barchars, ?barplot for more help.
# --------------------------------------------------------------------------------------------------------

# ******* YOUR CODE FOR HISTOGRAM PLOT GOES HERE ******** #
hist(adult_db_lim$age[adult_db_lim$class==0],breaks = 50,xlab="Age",main="Age of adults",col="red")
hist(adult_db_lim$age[adult_db_lim$class==1],breaks = 50,xlab="Age",col="blue",add=T)
legend(x=70,y=1250,legend=c(">50k","<=50k"),col=c("blue","red"),pch=20)

# ******* YOUR CODE FOR BAR CHART GOES HERE ******* #
barplot(table(adult_db_lim$race), xlab="Race", main="Race of adults",col=c(1,2,3,4,5))
legend(x=1,y=600,legend=c("Amer-Indian-Eskimo","Asian-Pac-Islander","Black","Other","White"),col=c(1,2,3,4,5),pch=20)

# 6. Plot a boxplot for attribute "Age" and show possible outlier for this attribute
# HINT: ?boxplot for more help
# ---------------------------------------------------------------------------------------------
# ****** YOUR CODE GOES HERE ***** #
boxplot(adult_db_lim$age,col="red",main="Age of adults")

# show possible outlier values
boxplot.stats(adult_db_lim$age)$out


# Create new data set from our latest dataset with only numeric attributes
# ------------------------------------------------------------------------
adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_val <- as.numeric(adult_db_lim[,c("class")])





# 7. Standardize numeric attributes in "adult_db_numeric" dataset.
# mean = 0 and sd = 1 for all numeric attributes
# -----------------------------------------------------------------------------------------------
adult_db_num_std <- scale(adult_db_numeric)
  
  
  
# we can check the mean and standard deviation of the standardized data
# ------------------------------------------------------------------
apply(adult_db_num_std, 2, mean)
apply(adult_db_num_std, 2, sd)





# 8. Run Principal Component Analysis (PCA) on the numeric dataset from above "adult_db_num_std"
# plot the first 2 principal components
# HINT: for class specific colours, in plot(...) command use parameter col = (class_val + 2)
# HINT: ?prcomp to know about the parameters
# ------------------------------------------------------------------------------------------

# ******** YOUR CODE FOR GETTING PRINCIPAL COMPONENTS GOES HERE ******** #
pr.out <- prcomp(adult_db_num_std, scale = TRUE, center = TRUE)
principal_comp <- pr.out$x

  
# ******** PLOT FOR FIRST TWO PRINCIPAL COMPONENTS GOES HERE ****** #  
plot(principal_comp[,1:2], col=(class_val + 2), pch=20, , main="First two Principal Components")
legend(x=-7,y=5,pch=20,legend = c("<=50k",">50k"),col=c("red","green"))
      
      
 
  
  
  
# 9. Plot percentage of the variance explained by principal components
# ----------------------------------------------------------------------------
# write a code to show proportion of variance explained by each Principal Component
# Standard deviation are stored as "sdev"

# *** YOUR CODE TO FIND PROPORTION OF VARIANCE EXPLAINED *** #
pr.var <- pr.out$sdev^2 # hint: square the standard deviation,look for "sdev" in pr.out object
pve <- pr.var/sum(pr.var) # hint: to calculate proportion of variance, divide each variance by sum of variance

  
# *** PLOT VARIANCE EXPLAINED BY PRINCIPAL COMPONENTS AND CUMULATIVE PROPORTION OF VARIANCE *** #
# par(...) for a plot with 2 figures.
par(mfrow=c(1,2), oma=c(0,0,2,0))
plot(pve , xlab="Principal Components", ylab="Variance", ylim=c(0,1) ,type='b', col="red")

  
# use cumsum(pve) for cumulative sum of variance
plot(cumsum(pve ), xlab=" Principal Components", ylab ="Cumulative Variance", ylim=c(0,1), type='b', col="red")
mtext("Proportion of variance explained by Principal Components", outer=TRUE, cex=1.2)


#Based on your results, how many principal components would you use to capture at least 50% of the 
#total variance in the dataset? How many would you use to capture at least 90% of the variance? 
#Please include your answer as a comment into the .R file you submit.

# You need 3 principal components to capture at least 50% of the total variance.
# You need 6 principal components to capture at least 90% of the total variance.

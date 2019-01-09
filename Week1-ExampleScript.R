# Example R Script for Intro To R & GITHUB
# Elise Larsen, 2019-01-09

#### OUTLINE
# 1. Example script for Intro to R, week 1 activity
# 2. Instructions for how to use this file in Intro to Github

# 1. Intro to R: Example script

#Looking at an existing dataset in R
summary(iris)

#iris data has 3 species. I select 2 to compare. There are many ways to do this. 
setosa.sl<-iris$Sepal.Length[iris$Species=="setosa"]
virginica.sl<-iris$Sepal.Length[iris$Species=="virginica"]

#analysis: t-test?

#test assumptions: Normality, Equal Variance

#Normality
hist(iris$Sepal.Length[iris$Species!=”versicolor”])
## Perform the test
shapiro.test(setosa.sl)
shapiro.test(virginica.sl)

## Plot using a qqplot
qqnorm(setosa.sl);qqline(setosa.sl, col = 2)
qqnorm(virginica.sl);qqline(virginica.sl, col = 2)

#Equal variance
var(setosa.sl)
var(virginica.sl)
### DOES NOT PASS ASSUMPTION. Cannot use t.test()

##MANN WHITNEY U TEST
# independent 2-group Mann-Whitney U Test
wilcox.test(setosa.sl,virginica.sl)

## CONCLUSION: REJECT the idea that sepal lengths are the same between setosa and virginica species
## Sepal Lengths differ between species

##Plot sepal lengths by species
boxplot(Sepal.Length~Species, data=iris[iris$Species!="versicolor",], notch=TRUE,
        col=("gold"),
        main="Sepal Length by Species", xlab="Species")




#####################  END HERE ###################




## 2. Intro to Github: Instructions 
##    ALL LINES BELOW COMMENTED OUT BUT YOU WILL RUN SOME OF THEM
##
## AFTER creating Github account, keep Github credentials available. 
## If you haven't already: 

## A. Configure Github:
## install if needed (do this exactly once):
## RUN:
# install.packages("usethis")

##configure name & email using your own name and email and then RUN:
#library(usethis)
#use_git_config(user.name = "YOUR NAME", user.email = "YOUREMAIL@georgetown.edu")

## B. Create a new RStudio project, select Version Control, Git, and use this repository link:
## https://github.com/leslieries/AdvancedModelsCourse
## On the top left panel, you'll now see a "Git" tab. 
## Within the git tab, the branch of the git repository you are on is int he top right corner. 
## The default is "master"
## Use the icon to the left to create a new branch  
## Edit this file in that branch.
## Commit the changes to your branch (include a commit message).

### FOR THE FOLLOWING, YOU WILL NEED TO BE ADDED AS A COLLABORATOR ON GITHUB.
# Switch to master branch.
# Merge branches (using shell in Tools menu).
# Push changes to origin.



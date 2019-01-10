# Example R Script for Intro To R & GITHUB
# Elise Larsen, 2019-01-09

#### OUTLINE
# 1. Example script for Intro to R, week 1 activity
# 2. Intro to Journal Review of Statistical Methods
# 3. Instructions for how to use this file in Intro to Github

# 1. Intro to R: Example script

#Looking at an existing dataset in R
summary(iris)

#iris data has 3 species. I select 2 to compare. There are many ways to do this. 
setosa.sl<-iris$Sepal.Length[iris$Species=="setosa"]
virginica.sl<-subset(iris$Sepal.Length, iris$Species=="virginica")

#analysis: t-test?

#test assumptions: Normality, Equal Variance

#Normality
hist(iris$Sepal.Length[iris$Species!='versicolor'])
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

##MANN WHITNEY U TEST / WILCOXON RANK  SUM TEST
# Wilcoxon rank sum test
wilcox.test(setosa.sl,virginica.sl)

## CONCLUSION: REJECT the idea that sepal lengths are the same between setosa and virginica species
## Sepal Lengths differ between species

##Plot sepal lengths by species
boxplot(Sepal.Length~Species, data=iris[iris$Species!="versicolor",], notch=TRUE,
        col=("gold"),
        main="Sepal Length by Species", xlab="Species")




#####################  END INTRO TO R HERE ###################




## 2. Intro to Journal Review of Statistical Methods

##LOAD DATA
stat.review<-read.csv("StatMethodReview.csv", header=T)

##VIEW DATA
head(stat.review) #shows column names and first 5 rows
summary(stat.review) #gives summary statistics by column

#dplyr is a very useful package for manipulating data
#install.packages("dplyr") ## install if needed
library(dplyr)
library(ggplot2)

##LOOKING AT TRENDS OVER TIME


##Type of study by year

paper.x.year<-stat.review %>%
  group_by(Year, Type.of.study) %>%
  summarize(n())
names(paper.x.year)[3]<-"n.papers"

## Visualizations
## Grouped
#ggplot(paper.x.year, aes(fill=Type.of.study, y=n.papers, x=Year)) + 
#  geom_bar(position="dodge", stat="identity")

# Stacked
ggplot(paper.x.year, aes(fill=Type.of.study, y=n.papers, x=Year)) + 
  geom_bar( stat="identity") +
  ylab("Number of Papers")


# Stacked Percent
ggplot(paper.x.year, aes(fill=Type.of.study, y=n.papers, x=Year)) + 
  geom_bar( stat="identity", position="fill") + 
  ylab("Proportion of Papers")



#### SUBSET Papers to *Studies*

#Look at the Proportion of studies (observational, field experiment, lab experiment, data compilation study only) that:
# Use statistics

##FILTER TO "STUDIES"
studytypes<-c("Observational study","Field experiment","Lab experiment", "Data compilation study")

#Filter Data
study.review <- stat.review %>%
  filter(Type.of.study %in% studytypes) %>%
  select(Year, Type.of.study, No.statistical.test, Correlation, GOF, Regression, Non.linear, Other.model)
study.review$Type.of.study<-factor(study.review$Type.of.study)


#### HOW OFTEN WERE STATISTICAL TESTS PRESENTED IN STUDIES?
#NA in "No.statistical.test" indicates some statistical test was performed
study.review$stat.boolean<-"no"
study.review$stat.boolean[is.na(study.review$No.statistical.test)]<-"yes"
study.review$stat.boolean<-factor(study.review$stat.boolean)

#summarize data of interest
study.x.year<-study.review %>%
  group_by(Year, Type.of.study, stat.boolean) %>%
  summarize(n())
names(study.x.year)[4]<-"n.studies"

## Visualizations
## stacked
plotnostatsgroup<-ggplot(study.x.year, aes(fill=(stat.boolean), y=n.studies, x=Year)) + 
  geom_bar(stat="identity") +
  ylab("Number of Studies")

#Facet by study type
plotnostatsgroup + facet_grid(Type.of.study ~ .)

# Stacked Percent
plotnostatsgroupProp<-ggplot(study.x.year, aes(fill=(stat.boolean), y=n.studies, x=Year)) + 
  geom_bar( stat="identity", position="fill") + 
  ylab("Proportion of Studies")
#Facet by study type
plotnostatsgroupProp + facet_grid(Type.of.study ~ .)



# Look at what Type of statistics were used (Correlation, GOF, Regression, Other)
study.review$stat.type<-as.factor("none")
levels(study.review$stat.type)<-c("none","correlation","regression","gof","multiple","other")
study.review$stat.type[study.review$Correlation==1]<-"correlation"
study.review$stat.type[study.review$GOF==1 & is.na(study.review$Correlation)]<-"gof"
study.review$stat.type[study.review$GOF==1 & study.review$Correlation==1]<-"multiple"
study.review$stat.type[study.review$Regression==1 & is.na(study.review$GOF) & is.na(study.review$Correlation)]<-"regression"
study.review$stat.type[study.review$Regression==1 & study.review$Correlation==1]<-"multiple"
study.review$stat.type[study.review$Regression==1 & study.review$GOF==1]<-"multiple"
study.review$stat.type[study.review$stat.type=="none" & study.review$stat.boolean=="yes"]<-"other"


#summarize data of interest
test.x.year<-study.review %>%
  group_by(Year, Type.of.study, stat.type) %>%
  summarize(n())
names(test.x.year)[4]<-"n.studies"

## Visualizations
# Try this palette with black from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## stacked
plotstattypes<-ggplot(test.x.year, aes(fill=(stat.type), y=n.studies, x=Year)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=cbbPalette) +
  ylab("Number of Studies")

#Facet by study type
plotstattypes + facet_grid(Type.of.study ~ .)

# Stacked Percent
plotstattypesProp<-ggplot(test.x.year, aes(fill=(stat.type), y=n.studies, x=Year)) + 
  geom_bar( stat="identity", position="fill") + 
  scale_fill_manual(values=cbbPalette) +
  ylab("Proportion of Studies")
#Facet by study type
plotstattypesProp + facet_grid(Type.of.study ~ .)


### Class Activities:
#Look at the proportion of regression studies over time that:
#  act as if all assumptions met
#  deal with non-normal errors 
#  use different approached to deal with non-normal errors (Xform, NP, AltDist)
#  deal with non-independence (mixed model)


#####################  END INTRO TO STAT REVIEW ###################




## 3. Intro to Github: Instructions 
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



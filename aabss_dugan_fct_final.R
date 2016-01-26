# The following syntax is intended to provide a demonstration of several nonparametric regression 
# measures and accompanies a paper written by the author of this file for a course in his Master's
# program at Ball State University, IN. Advice and input is welcome! Contact me at bjdugan@bsu.edu.

# IF YOU'VE RUN THIS ONCE BEFORE, begin with loading CSV (2nd section/line 114)!

#####
## DOWNLOADING DATASETS, MERGING, RECODING

# The syntax draws from available R packages, as well as functions written by Dr. R. Wilcox, USC 
# Dornsife. For more information on downloading these functions refer to this site: 
# http://dornsife.usc.edu/labs/rwilcox/software/ , which includes detailed instructions for several
# methods of obtaining these functions. 

# The data for this project come from the CDC's National Health Interview Survey 2011, a freely 
# available dataset which includes a variety of information on health-related behaviors, conditions,
# and outcomes, as well as basic demographic information. These data can be downloaded in ASCII .dat
# format, along with documentation and SPSS, SAS, and STATA syntax files, from the NHIS website: 
# http://www.cdc.gov/nchs/nhis/nhis_2011_data_release.htm. 

# Data can be downloaded in a more (R) user-friendly .Rda format (helpful for us beginners!) from 
# the GitHub account of Anthony Joseph Damico (ajdamico@gmail.com), whose code was used to download
# the NHIS data in an R-ready format. Many thanks, Mr. Damico.

# Run the following, written by A. J. Damico. This will set up the download for the specified year
# to the folder you specify with setwd(). You may need to install packages "downloader," "RCurl,"
# "SAScii," if you have not already. This may take some time!!

    #install.packages(c("downloader","RCurl","SAScii")) # uncomment to download packages
    library(downloader)        
    setwd( "C:/Users/YOURNAME/YOUR_WORKING_DIRECTORY")
    nhis.years.to.download <- 2011
    source_url("https://raw.github.com/ajdamico/asdfree/master/National%20Health%20Interview%20Survey/download%20all%20microdata.R",
               prompt = TRUE, echo = TRUE)

# Again, this may take some time - the data and documentation are downloading. Grab a cup of coffee.
# Once finished, you can check your working directory; a folder "2011" should have appeared. If need
# be, you can manually delete any files you won't use; just keep the samadult and personsx files!

# Load in the sample adult and the person-level files, located in the 2011 folder in your current dir.
    load("./2011/samadult.rda")
    load("./2011/personsx.rda")

# The data are collected at several levels (e.g., person, household, sample child, and sample adult),
# and contain different variables. E.g., the sample adult data contains variables on tobacco and
# alcohol use, but not personal earnings, which is included in the person-level data. Thus, the two 
# datasets will be joined on several identifying variables, described below.

# First, however, we'll make a subset of either dataset including only the variables we need to keep
# things simple: the variables to merge/join on, and the variables of interest in this project, as
# well as some demographic variables: Household, family, and personal identification; 2010 earnings
# (in $5- and $10,000 increments), education (in years), sex, age, race, and marital status; number 
# of cigarettes mysampled a day in the past 30 days for occassional and regular mysamplers, number 
# of days per week in the past year respondent had an alcoholic beverage, the number of beverages 
# (in case you're interested in running similar analyses).

# It is crucial to keep the first three (ID vars) in the same order and position. More about each of
# these variables can be learned from the documentation, "samadult_summary" and "personsx_summary."
    personx <- subset(NHIS.11.personsx.df, select=c(hhx, fmx, fpx, ernyr_p, educ1, sex, age_p,
                                                mracrpi2, r_maritl), na.omit=TRUE)
    sadult <- subset(NHIS.11.samadult.df, select=c(hhx, fmx, fpx, cigsday, alc12mwk, alcamt), 
                     na.omit=TRUE)

# Now the subsets are ready for merging. Because the sample adult dataset is actually a subset of 
# all individual survey respondents (each case in the person-level dataset), the cases in the merged 
# dataset should not exceed that of the samadult. (There may be fewer since we removed cases with 
# any missing value of our variables). This can be easily verified with a case count of any var.
    mergedNHIS11 <- merge(personx, sadult)

# We'll recode only the analysis variables, cigsday (# cigarettes mysampled) and ernyr_p (earnings). 
# The original cases (including those with missing values) can be restored with the previous code. 
    summary(mergedNHIS11$cigsday)       # 97-99 coded as NA, refused, etc.; these can be excluded.
    mergedNHIS11 <- mergedNHIS11[which(mergedNHIS11$cigsday<=95),]
    summary(mergedNHIS11$cigsday)       # to see remaining cases - only those who mysample.

    summary(mergedNHIS11$ernyr_p)       # 11 should be highest category for those who 
                                        # reported (those we'll keep)
    mergedNHIS11 <- mergedNHIS11[which(mergedNHIS11$ernyr_p<90),]
    summary(mergedNHIS11$ernyr_p)     # again, to check our work. 

# Recoding earnings using midpoints of $10,000 categories across the board, as new variable.
# (e.g., 1=2.5, or $2,500; 2 = 9.9995, or $9,9995)
    # Median                                     New Coding    Old Coding
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==1] <-  2.5000    # 01 $01-$4,999

    mergedNHIS11$earn[mergedNHIS11$ernyr_p==2] <-  9.9995    # 02 $5,000-$14,999
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==3] <-  9.9995    # 03 $10,000-$14,999

    mergedNHIS11$earn[mergedNHIS11$ernyr_p==4] <- 19.9995    # 04 $15,000-$19,999
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==5] <- 19.9995    # 05 $20,000-$24,999

    mergedNHIS11$earn[mergedNHIS11$ernyr_p==6] <- 29.9995    # 06 $25,000-$34,999
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==7] <- 39.9995    # 07 $35,000-$44,999
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==8] <- 49.9995    # 08 $45,000-$54,999
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==9] <- 59.9995    # 09 $55,000-$64,999
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==10] <-69.9995    # 10 $65,000-$74,999  
    mergedNHIS11$earn[mergedNHIS11$ernyr_p==11] <-79.9995    # 11 $75,000 and over
    # Compare tables for accuracy of recoding
    table(mergedNHIS11$earn)     
    table(mergedNHIS11$ernyr_p)

# One last check of everything before we start analysis
    summary(mergedNHIS11)
    length(mergedNHIS11$cigsday); length(mergedNHIS11$cigsday) == length(mergedNHIS11$earn)

# Now that we have the data how we want it in R, save a copy for backup. This subset can be loaded on
# its own in the future and save the hassle of all the preceding. This file appears in the current
# directory, defined earlier.
    write.csv(mergedNHIS11, "myNHIS11.csv")

# Clear the working environment of all extraneous objects, to save RAM and stay organized.
    rm(list=ls())
    gc()
#####
## IMPORTING DATA AND CREATING SAMPLE

# Load the data, a subset of the NHIS2011 data with recoded variables and complete cases (see above)
    myNHIS11 <- read.csv("myNHIS11.csv", header=TRUE)
    head(myNHIS11)      # R added a column at the beginning, which can be removed by keeping the 
                        # original 13; we can also now omit the 3 columns used for merging

    myNHIS11 <- myNHIS11[,5:14]     # If we don't care about other variables, they can be removed in 
                                    # a similar fashion.

# Create a small random sample for our demonstration. Keep the seed at 2077 for replicability. 
    set.seed(2077)
    mysample <- myNHIS11[sample(1:3489, 100, replace=FALSE),]       # Draws 100 cases randomly
    summary(mysample$earn); summary(mysample$cigsday)               # Summary info on our main vars
    rm(myNHIS11)                                                    # Clean up a tad
#####
## INSTALLING PACKAGES AND PRODUCING SUMMARY OUTPUT

# The following codes will run various regression models over our subsample, beginning with OLS and 
# continuing with the various nonparametric. You will likely need to install and load a few packages
# before continuing, as well as load the functions written by Dr. R. Wilcox, discussed above. Ensure
# this .txt file is located in your working directory.

    #install.packages(c("MASS", "quantreg", "robustbase"))     # to install
    library(MASS); library(quantreg); library(robustbase)
    source("./Rallfun-v27.txt")

# We'll be checking for regression assumptions with graphs; Make a folder for these to be sent to.
    plots.dir <- paste0(getwd(),"/","plots")
    dir.create(plots.dir)       # Only needs done once.
    
# Dump output in a text file.
    sink(file="output.txt", append=FALSE, split=TRUE)

# Obtain some summary statistics on the variables (by column location) and some histograms. 
# Note that width & height default is 480 and can be set by omitting arguments. 
    attach(mysample)
    paste("Descriptive Statistics for Daily Cigarette Consumption & Reported Earnings")
    summary(mysample[c(7,10)])
    png(file=paste0(plots.dir,"/mysample_cigsday.png"), width=600, height=600)
    hist(cigsday,col="slategray3", main="Daily Cigarette Consumption, All Smokers"); dev.off()
    png(file=paste0(plots.dir,"/mysample_earn.png"), width=600, height=600)
    hist(earn, col="slategray3", main="2010 Earnings, in $10,000 Categories"); dev.off()

# For each type of regressions, we'll a) create the model, b) get summary information if available, 
# c) produce plots to check for outliers and homogeneity of variances.
#####
## STANDARD PARAMETRIC APPROACH

# Ordinary Least Squares Regression
    mysample.ols <- lm(cigsday~earn)
    summary(mysample.ols)
    anova(mysample.ols)
    png(file=paste0(plots.dir,"/ols_rf.png"), width=600, height=600)
    plot(mysample.ols$residuals, mysample.ols$fitted.values, col="slategray", xlab="Residuals", 
         ylab="Fitted Values"); dev.off()
    png(file=paste0(plots.dir,"/ols_qq.png"), width=600, height=600)
    qqnorm(mysample.ols$residuals, col="slategray", main="Normal Q-Q Plot OLS")
    qqline(mysample.ols$residuals, col="tomato3"); dev.off()
#####
## FUNCTIONS FOR CALCULATING SIGNFICANCE
# Some of the provided functions only provide coefficients and lists of residuals; here we write 
# some functions to produce useful information, like F scores. 
    getF <- function(ResSumSq, ResDF, EarnMeanSq) {
        # This function produces F-value for models that anova() provides limited information. 
        # Supply anova()-provided Mean Sq. for predictor (earn), residuals sum of squares. DF = 98 
        # as 100-2=98. Prints F value, crit value, and if F >= Critical at 0.05 level (one-sided).
    
        x <- (EarnMeanSq/((ResSumSq/ResDF)))
        y <- qf(0.95, 1, 98)
        print(c("F Score",x));print(c("F Critical",y))
        x >= y
    }

# If anova() does not provide Res SS, run
    ResSS <- function (ModelRes)  sum((ModelRes)^2) # where ModelRes is data.model$residuals
# If anova() does not produce RES MQS, run
    ResMS <- function(ResSumSq, ResDF) (ResSumSq/ResDF) # in our case, DF should usually be 98.
# For SE's, run
    SEfxn <- function(x, Resdf) {
        sqrt(ResSS(x)/ResMS(ResSS(x), Resdf))       # Where x is data.model$residuals
    }
#####
## ROBUST METHODS: S, LAD, LTS

# S Estimator
    mysample.s <- lqs(cigsday~earn, method=c("S"))
    mysample.s$coefficients
    mysample.s$crit
    aov(mysample.s)
    getF(8284.992, 98, 185.918)             # Obtain these values from the above aov() output.
    png(file=paste0(plots.dir,"/s_rf.png"), width=600, height=600)
    plot(mysample.s$residuals, mysample.s$fitted.values, col="slategray", xlab="Residuals", 
         ylab="Fitted Values"); dev.off()
    png(file=paste0(plots.dir,"/s_qq.png"), width=600, height=600)
    qqnorm(mysample.s$residuals, col="slategray", main="Normal Q-Q Plot S")
    qqline(mysample.s$residuals, col="tomato3"); dev.off()
#####
# Least Absolute Deviation (LAD) Estimator
    mysample.lad <- rq(cigsday~earn)
    summary(mysample.lad)
    mysample.lad$coefficients
    aov(mysample.lad)
    getF(8284.992, 98, 185.918)             # Obtain these values from the above aov() output.
    png(file=paste0(plots.dir,"/lad_rf.png"), width=600, height=600)
    plot(mysample.lad$residuals, mysample.lad$fitted.values, col="slategray", xlab="Residuals", 
         ylab="Fitted Values"); dev.off()
    png(file=paste0(plots.dir,"/lad_qq.png"), width=600, height=600)
    qqnorm(mysample.lad$residuals, col="slategray", main="Normal Q-Q Plot LAD")
    qqline(mysample.lad$residuals, col="tomato3"); dev.off()
#####
# Least Trimmed Squares (LTS) Estimators
    mysample.lts <- ltsReg(cigsday~earn)
    summary(mysample.lts)
    png(file=paste0(plots.dir,"/lts_rf.png"), width=600, height=600)
    plot(mysample.lts.rb$residuals, mysample.lts.rb$fitted.values, col="slategray", xlab="Residuals", 
         ylab="Fitted Values"); dev.off()
    png(file=paste0(plots.dir,"/lts_qq.png"), width=600, height=600)
    qqnorm(mysample.lts.rb$residuals, col="slategray",  main="Normal Q-Q Plot LTS")
    qqline(mysample.lts.rb$residuals, col="tomato3"); dev.off()
    png(file=paste0(plots.dir,"/lts_plot.png"), width=600, height=600)
    ltsPlot(mysample.lts.rb); dev.off()

# LTS, with higher H values
    mysample.lts.a8 <- ltsReg(cigsday~earn, alpha=.8)
    summary(mysample.lts.a8)
    png(file=paste0(plots.dir,"/ltsa8_rf.png"))
    plot(mysample.lts.a8$residuals, mysample.lts.a8$fitted.values, col="slategray", xlab="Residuals",
         ylab="Fitted Values"); dev.off()
    png(file=paste0(plots.dir,"/ltsa8_qq.png"))
    qqnorm(mysample.lts.a8$residuals, col="slategray", main="Normal Q-Q Plot LTS a=0.8")
    qqline(mysample.lts.a8$residuals, col="tomato3"); dev.off()
    png(file=paste0(plots.dir,"/ltsa8_plot.png"))
    ltsPlot(mysample.lts.a8); dev.off()
#####
sink()
    

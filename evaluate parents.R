#
# Di Cook, Ian Lyttle, and Alex Shum also contributed to this code
# 
#
setwd("...PISA 2012/RDA files/")
library(stringr)
library(ggplot2)
library(reshape)
library(dplyr)
library(lubridate)
#for ggvis package
# devtools::install_github(c("rstudio/rmarkdown", "rstudio/ggvis"),build_vignettes = FALSE)
library(ggvis)
library(doBy)
library(maps)
library(ggmap)
library(rworldmap)
sessionInfo()
################## IMPORTANT #######################
# ************************************************
# My file path has the rda files in the main folder.
# They are not in a "data" folder.
# ************************************************
####################################################
# data can be downloaded from http://beta.icm.edu.pl/PISAcontest/data/

sets <- c("item", "parent", "school", "scoredItem", "student")

# function to build the file names
fn_build <- function(file_name) {

    template <- c("2012.rda", "2012dict.rda")

    file_name %>% vapply(str_join, template, template) %>% file.path(".", "", 
        .)
}

# load the data
sets %>% fn_build %>% lapply(load, .GlobalEnv)


fn_make_df <- function(named_vector){
  data.frame(
    variable = attr(named_vector, "names"),
    description = named_vector,
    row.names = NULL
  )
}

# there's a clever way to do this, but beyond me for naw
dict_item2012 <- fn_make_df(item2012dict) 
dict_parent2012 <- fn_make_df(parent2012dict) 
dict_school2012 <- fn_make_df(school2012dict) 
dict_scoredItem2012 <- fn_make_df(scoredItem2012dict) 
dict_student2012 <- fn_make_df(student2012dict) 

# clean
rm(fn_make_df)
rm(item2012dict, parent2012dict, school2012dict, scoredItem2012dict, student2012dict)
load("student2012pvmeans.Rdata")
student2012 <- cbind(student2012,a)

student2012$ST15Q01 <- addNA(student2012$ST15Q01)
student2012$ST19Q01 <- addNA(student2012$ST19Q01)

student2012$name <- as.character(student2012$CNT)
# unique(anti_join(student2012.sub, world.polys)[1])
student2012$name[student2012$name=="United Arab Emirates"] <- "UAE"
student2012$name[student2012$name=="United Kingdom"] <- "UK"
student2012$name[student2012$name=="Serbia"] <- "Serbia"
student2012$name[student2012$name=="Korea"] <- "South Korea"
student2012$name[student2012$name=="Chinese Taipei"] <- "Taiwan"
student2012$name[student2012$name=="Slovak Republic"] <- "Slovakia"
student2012$name[student2012$name=="Russian Federation"] <- "Russia"
student2012$name[student2012$name=="Perm(Russian Federation)"] <- "Russia"
student2012$name[student2012$name=="Hong Kong-China"] <- "Hong Kong"
student2012$name[student2012$name=="China-Shanghai"] <- "China"
student2012$name[student2012$name=="Macau"] <- "China"
student2012$name[student2012$name=="Connecticut (USA)"] <- "USA"
student2012$name[student2012$name=="Florida (USA)"] <- "USA"
student2012$name[student2012$name=="Massachusetts (USA)"] <- "USA"
student2012$name[student2012$name=="United States of America"] <- "USA"
unique(student2012$name)
student2012$name <- factor(student2012$name)


#########################################
#
# Evaluating the role of a parents job status
#
#########################################


# Mother's Occupation ST15Q01, Father's ST19Q01
aa1 <- a1 <- summaryBy(data=student2012, pvM + ESCS ~ ST15Q01 +ST19Q01 + CNT,FUN=mean)
#a2 <- melt(a1)
levels(a1$ST15Q01) <- levels(a1$ST19Q01)<- c("Full-time","Part-time","Unemployed","Other","NA")
#a1$ST15Q01 <- factor(factor(a1$ST15Q01),levels=rev(levels(a1$ST15Q01)))
#a1$ST19Q01 <- factor(factor(a1$ST19Q01),levels=rev(levels(a1$ST19Q01)))

ind <- which(a1$ST15Q01=="Full-time")
a1$CNT <- factor(a1$CNT,levels(a1$CNT)[order(a1$pvM.mean[ind])])
#a1$CNT[ind] <- reorder(factor(a1$CNT[ind]),a1$pvM.mean[ind],mean)
p <- qplot(CNT, pvM.mean, col=ST15Q01, data = a1) +coord_flip()+ facet_grid(~ST19Q01);p
#ggsave(p,file="Parentmathachievement.pdf",height=10,width=18)

a1 <- cbind(student2012[,c("name","ST15Q01")],Parent="Mother")
a2 <- cbind(student2012[,c("name","ST19Q01")],Parent="Father")
levels(a1$ST15Q01) <-  c("Full-time","Part-time","Unemployed","Other","NA")
levels(a2$ST19Q01) <- c("Full-time","Part-time","Unemployed","Other","NA")

names(a1) <- names(a2) <- c("Country","Occupation","Parent")
a3 <- rbind(a1,a2)
p <- ggplot(a3, aes(Occupation, fill=Parent)) + geom_bar(position="dodge")
#ggsave(p,file="ParentJobBar.pdf",width=6,height=4)


# Mathematics
a6 <- summaryBy(data=student2012, pvM + ESCS ~ ST15Q01+ name,FUN=mean,rm.na=T)
a7 <- summaryBy(data=student2012, pvM + ESCS ~ ST19Q01+ name,FUN=mean,rm.na=T)
colnames(a6) <- colnames(a7) <- c("Job.Status","Country","Math","ESCS")
levels(a6$Job.Status) <- levels(a7$Job.Status)  <- c("Full-Time","Part-Time","Unemployed","Other","NA")

a8 <- rbind(cbind(a6,Parent="Mother"),cbind(a7,Parent="Father"))
#a2 <- melt(a1)
ind <- which(a8$Job.Status=="Other"& a8$Parent=="Mother")
a8$Country <- factor(a8$Country,levels(a8$Country)[order(a8$Math[ind])])
p <- qplot(Country, Math, col=Job.Status, data = a8) +coord_flip() + facet_wrap(~Parent);p
#ggsave(p,file="Parentmathachievement.pdf",height=9,width=11)


############################
# Reading
############################
a6 <- summaryBy(data=student2012, pvR + ESCS ~ ST15Q01+ name,FUN=mean,rm.na=T)
a7 <- summaryBy(data=student2012, pvR + ESCS ~ ST19Q01+ name,FUN=mean,rm.na=T)
colnames(a6) <- colnames(a7) <- c("Job.Status","Country","Reading","ESCS")
levels(a6$Job.Status) <- levels(a7$Job.Status)  <- c("Full-Time","Part-Time","Unemployed","Other","NA")

a8 <- rbind(cbind(a6,Parent="Mother"),cbind(a7,Parent="Father"))
#a2 <- melt(a1)
ind <- which(a8$Job.Status=="Other"& a8$Parent=="Mother")
a8$Country <- factor(a8$Country,levels(a8$Country)[order(a8$Reading[ind])])
p <- qplot(Country, Reading, col=Job.Status, data = a8) +coord_flip() + facet_wrap(~Parent);p
#ggsave(p,file="ParentReadachievement.pdf",height=9,width=11)


############################
# Science
############################
a6 <- summaryBy(data=student2012, pvS + ESCS ~ ST15Q01+ name,FUN=mean,rm.na=T)
a7 <- summaryBy(data=student2012, pvS + ESCS ~ ST19Q01+ name,FUN=mean,rm.na=T)
colnames(a6) <- colnames(a7) <- c("Job.Status","Country","Science","ESCS")
levels(a6$Job.Status) <- levels(a7$Job.Status)  <- c("Full-Time","Part-Time","Unemployed","Other","NA")

a8 <- rbind(cbind(a6,Parent="Mother"),cbind(a7,Parent="Father"))
#a2 <- melt(a1)
ind <- which(a8$Job.Status=="Other"& a8$Parent=="Mother")
a8$Country <- factor(a8$Country,levels(a8$Country)[order(a8$Science[ind])])
p <- qplot(Country, Science, col=Job.Status, data = a8) +coord_flip() + facet_wrap(~Parent);p
#ggsave(p,file="ParentScienceachievement.pdf",height=9,width=11)





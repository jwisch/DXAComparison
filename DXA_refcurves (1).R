library(gamlss)
library(tableone)
library(ggplot2)
library(gridExtra)
library(ppcor)
library(ggpubr)
#https://rdrr.io/cran/gamlss/man/lms.html
#Replicating this paper in our HECS cohort:  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2737140/
FILEPATH_DATA<-"W:/ances/julie/Data/HIV/"
FILEPATH_CODE<-"W:/ances/julie/HIV/HECS/"
source(paste(FILEPATH_CODE, "Boxplot_Production_DEXA.R", sep = ""))

df<-read.csv(paste(FILEPATH_DATA, "HECS_DEXA.csv", sep = ""))
demogs<-read.csv(paste(FILEPATH_DATA, "HECS_baseline_data_80062_2019_01_30.csv", sep = ""))
demogs<-demogs[,c("partID", "age", "gender", "race", "education", "height", "duration_of_infection", "recent_cd4", "nadir_cd4",
                  "recent_viral_load")]

#Cleaning data - setting it all to the right datatype for analysis
df$related_study_id<-as.character(df$related_study_id)
df$pid_scandate<-as.character(df$pid_scandate)
df$visit<-substrRight(df$related_study_id, 2)
df$id<-substr(df$related_study_id, 1, 5)
df$scandate<-substrRight(df$pid_scandate, 6)
df$scandate<-as.Date(df$scandate, format = "%m%d%y")


df$id<-paste("HECS", df$id, sep = "")

#Combining data 
df<-merge(df, demogs, by.x = "id", by.y = "partID", all = FALSE)
df<-df[complete.cases(df$age),]
df<-df[complete.cases(df$trunk_tissue_fat_hecs),]
df$agecat<-cut(df$age, breaks = c(50, 59, 71))
df$lean_mass_kg_hecs<-df$lean_mass_g_hecs/1000

#Generating demographics table...doing it only for males since that's all we have sufficient samples of
listVars<-c("age", "race",  "height_hecs", "weight_hecs", "bmi_hecs", "lean_mass_kg_hecs", "education",  "duration_of_infection", "recent_cd4", "nadir_cd4", "recent_viral_load")
catVars<-c( "race")
CreateTableOne(vars = listVars, data = df[df$gender == 0,], factorVars = catVars, strata = c("agecat"))


#FMI - body fat mass in kg divided by height in meters squared
df$FMI<-(df$total_right_tiss_fat_hecs + df$total_left_tiss_fat_hecs)/(2.2*(df$height/39.37)^2)
df$totalbodyfatperc<-df$fat_mass_g_hecs/(1000*df$total_mass_kg_hecs)
df$trunk_to_leg<-df$trunk_tissue_fat_hecs/df$leg_tissue_fat_hecs
df$trunk_to_limb<-df$trunk_tissue_fat_hecs/df$arms_tissue_fat_hecs

df_all<-df
#Dropping to baseline scans only
df<-df[df$visit == "T1",]
df<-df[df$gender == 0,] #Dropping to males only
#Creating curves like they do in: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2737140/
# foranalysis<-df[df$gender == 0,c("age", "FMI")]
# m1<-lms(FMI, age, data = foranalysis, cent = c(3, 5, 25, 50 , 75, 95, 97)) #Using centiles from the DXA paper
# 
# foranalysis<-df[df$gender == 0,c("age", "totalbodyfatperc")]
# m2<-lms(totalbodyfatperc, age, data = foranalysis, cent = c(3, 5, 25, 50 , 75, 95, 97)) #Using centiles from the DXA paper
# 
# foranalysis<-df[df$gender == 0,c("age", "a_g_ratio_hecs")]
# m3<-lms(a_g_ratio_hecs, age, data = foranalysis, cent = c(3, 5, 25, 50 , 75, 95, 97))
# 
# foranalysis<-df[df$gender == 0,c("age", "trunk_to_leg")]
# m4<-lms(trunk_to_leg, age, data = foranalysis, cent = c(3, 5, 25, 50 , 75, 95, 97))
# 
# foranalysis<-df[df$gender == 0,c("age", "trunk_to_limb")]
# m5<-lms(trunk_to_limb, age, data = foranalysis, cent = c(3, 5, 25, 50 , 75, 95, 97))

#Comparing to https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5384668/
#T test to compare our age stuff to the published paper

####################################################################################################
####################################################################################################
#Testing for significant differences between our cohort and the NHANES cohort...manually pulled all NHANES value from published manuscript
#output of "ComparetoPaper is the p value. If p < .05 our cohort singificantly differs from the NHANES cohort
ComparetoPaper("(50,59]", "totalbodyfatperc", 0.309, 7.9, 214)
ComparetoPaper("(59,71]", "totalbodyfatperc", 0.310, 7.8, 236)

hold2<-PreptoPlot(mean(df[df$agecat == "(50,59]", "totalbodyfatperc"], na.rm = TRUE), 
                  sd(df[df$agecat == "(50,59]", "totalbodyfatperc"], na.rm = TRUE),
                  mean(df[df$agecat == "(59,71]", "totalbodyfatperc"], na.rm = TRUE),
                  sd(df[df$agecat == "(59,71]", "totalbodyfatperc"], na.rm = TRUE),
                  0.309, .079, 0.310, .078)
p1<-ggplot(hold2, aes(x=Age, ymin = LOWER, lower = TWENTYFIVE, middle = FIFTY,
                  upper = SEVENTYFIVE, ymax = UPPER, fill = Group)) +
  geom_boxplot(stat = "identity") + ggtitle("Total Body Fat Percentage") + theme(legend.position = "bottom")

ComparetoPaper("(50,59]", "FMI", 8.9, 3.4, 214)
ComparetoPaper("(59,71]", "FMI", 8.9, 3.4, 236) #Different

hold2<-PreptoPlot(mean(df[df$agecat == "(50,59]", "FMI"], na.rm = TRUE), 
                  sd(df[df$agecat == "(50,59]", "FMI"], na.rm = TRUE),
                  mean(df[df$agecat == "(59,71]", "FMI"], na.rm = TRUE),
                  sd(df[df$agecat == "(59,71]", "FMI"], na.rm = TRUE),
                  8.9, 3.4,  8.9, 3.4)
p2<-ggplot(hold2, aes(x=Age, ymin = LOWER, lower = TWENTYFIVE, middle = FIFTY,
                  upper = SEVENTYFIVE, ymax = UPPER, fill = Group)) +
  geom_boxplot(stat = "identity") + ggtitle("Fat Mass Index [kg/m2]") + theme(legend.position = "bottom")


ComparetoPaper("(50,59]", "a_g_ratio_hecs", 0.73, 0.21, 214) #These are totally different
ComparetoPaper("(59,71]", "a_g_ratio_hecs", 0.77, 0.20, 236) #Were they calculated the same way? Doesn't seem right

hold2<-PreptoPlot(mean(df[df$agecat == "(50,59]", "a_g_ratio_hecs"], na.rm = TRUE), 
                  sd(df[df$agecat == "(50,59]", "a_g_ratio_hecs"], na.rm = TRUE),
                  mean(df[df$agecat == "(59,71]", "a_g_ratio_hecs"], na.rm = TRUE),
                  sd(df[df$agecat == "(59,71]", "a_g_ratio_hecs"], na.rm = TRUE),
                  0.73, 0.21,  0.77, 0.20)
p3<-ggplot(hold2, aes(x=Age, ymin = LOWER, lower = TWENTYFIVE, middle = FIFTY,
                  upper = SEVENTYFIVE, ymax = UPPER, fill = Group)) +
  geom_boxplot(stat = "identity") + ggtitle("Android to Gynoid Ratio") + theme(legend.position = "bottom")


ComparetoPaper("(50,59]", "trunk_to_limb", 1.71, 0.38, 214) #Very different
ComparetoPaper("(59,71]", "trunk_to_limb", 1.78, 0.44, 236) #Very different

hold2<-PreptoPlot(mean(df[df$agecat == "(50,59]", "trunk_to_limb"], na.rm = TRUE), 
                  sd(df[df$agecat == "(50,59]", "trunk_to_limb"], na.rm = TRUE),
                  mean(df[df$agecat == "(59,71]", "trunk_to_limb"], na.rm = TRUE),
                  sd(df[df$agecat == "(59,71]", "trunk_to_limb"], na.rm = TRUE),
                  1.71, 0.38,  1.78, 0.44)
p4<-ggplot(hold2, aes(x=Age, ymin = LOWER, lower = TWENTYFIVE, middle = FIFTY,
                  upper = SEVENTYFIVE, ymax = UPPER, fill = Group)) +
  geom_boxplot(stat = "identity") + ggtitle("Trunk to Limb Fat Ratio") + theme(legend.position = "bottom")


ComparetoPaper("(50,59]", "trunk_to_leg", 1.39, 0.23, 214) #Very different
ComparetoPaper("(59,71]", "trunk_to_leg", 1.43, 0.27, 236) #Very different

hold2<-PreptoPlot(mean(df[df$agecat == "(50,59]", "trunk_to_leg"], na.rm = TRUE), 
                  sd(df[df$agecat == "(50,59]", "trunk_to_leg"], na.rm = TRUE),
                  mean(df[df$agecat == "(59,71]", "trunk_to_leg"], na.rm = TRUE),
                  sd(df[df$agecat == "(59,71]", "trunk_to_leg"], na.rm = TRUE),
                  1.39, 0.23,  1.43, 0.27)
p5<-ggplot(hold2, aes(x=Age, ymin = LOWER, lower = TWENTYFIVE, middle = FIFTY,
                  upper = SEVENTYFIVE, ymax = UPPER, fill = Group)) +
  geom_boxplot(stat = "identity") + ggtitle("Trunk to Leg Fat Ratio") + theme(legend.position = "bottom")


grid.arrange(p1, p2, p3, p4, p5, nrow = 2)

rm(p1, p2, p3, p4, p5, m1, m2, m3, m4, m5, hold2)
####################################################################################################
####################################################################################################

####################################################################################################
####################################################################################################
#Comparing body composition to cognition

cog<-read.csv(paste(FILEPATH_DATA, "HECS_baseline_data_80062_2019_01_30.csv", sep = ""))
cog<-cog[,c(1, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 67:71)]

df<-merge(df, cog, by.x = "id", by.y = "partID")

CompareRegions<-function(REGION){
p1<-ggplot(df, aes(x = totalbodyfatperc, y = REGION)) + geom_point() + geom_smooth(method = "lm")
p2<-ggplot(df, aes(x = FMI, y = REGION)) + geom_point() + geom_smooth(method = "lm")
p3<-ggplot(df, aes(x = a_g_ratio_hecs, y = REGION)) + geom_point() + geom_smooth(method = "lm")
p4<-ggplot(df, aes(x = trunk_to_limb, y = REGION)) + geom_point() + geom_smooth(method = "lm")
p5<-ggplot(df, aes(x = trunk_to_leg, y = REGION)) + geom_point() + geom_smooth(method = "lm")
p<-list(p1, p2, p3, p4, p5)
return(p)}

p<-CompareRegions(df$Hippocampus_norm_T1)
do.call("grid.arrange", c(grobs = p))

index<-75
#linear model to test for differences
summary(lm(df[,index] ~ totalbodyfatperc + age + education, df))
summary(lm(df[,index] ~ FMI + age + education, df))
summary(lm(df[,index] ~ a_g_ratio_hecs + age + education, df))
summary(lm(df[,index] ~ trunk_to_limb + age + education, df))
summary(lm(df[,index] ~ trunk_to_leg + age + education, df))

#Testing to see if there is a relationship between any of the body compositions and if someone is considered cognitively impaired
mylogit <- glm(impaired_T1 ~ totalbodyfatperc + age + education, data = df, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

mylogit <- glm(impaired_T1 ~ FMI + age + education, data = df, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

mylogit <- glm(impaired_T1 ~ a_g_ratio_hecs + age + education, data = df, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

mylogit <- glm(impaired_T1 ~ trunk_to_limb + age + education, data = df, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

mylogit <- glm(impaired_T1 ~ trunk_to_leg + age + education, data = df, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

df<-df[complete.cases(df$age),]
df<-df[complete.cases(df$education),]

#Testing for correlations between body composition and cognition, correcting for age and education
for(i in 62:75){
  print(names(df[i]))
print(pcor.test(df[complete.cases(df[,i]),"trunk_to_leg"], df[complete.cases(df[,i]),i], df[complete.cases(df[,i]),c("age", "education")], method = "spearman"))}
#Only significant result is shown here:
#PM_GDS_T1 ~ Trunk to limb, spearman correlation R = -0.437, p = 0.005

#trunktolimb, i = 74
pcor.test(df[complete.cases(df[,"PM_GDS_T1"]),"trunk_to_limb"], df[complete.cases(df[,"PM_GDS_T1"]),"PM_GDS_T1"], df[complete.cases(df[,"PM_GDS_T1"]),c("age", "education")], method = "spearman")
####################################################################################################
####################################################################################################


####################################################################################################
####################################################################################################
###Doing paired testing
#Comparing Pre- and Post- testing
#Grouping
group<-read.csv("W:/ances/julie/Data/HIV/HECS_baseline_data_80062_2019_01_30.csv")
group<-group[,c("partID", "groupID")] #1 = exercise, 2 = stretch

df_all<-df_all[df_all$totalbodyfatperc > 0.03,]
T1<-unique(df_all[df_all$visit == "T1", "id"])
T3<-unique(df_all[df_all$visit == "T3", "id"])
T3<-T3[T3 %in% T1]
df_plot<-df_all[df_all$id %in% T3,]
df_plot<-data.frame(rbind(df_plot[df_plot$visit == "T1", c("id", "visit", "FMI", "totalbodyfatperc", "trunk_to_leg", "trunk_to_limb", "a_g_ratio_hecs")],
                          df_plot[df_plot$visit == "T3", c("id", "visit", "FMI", "totalbodyfatperc", "trunk_to_leg", "trunk_to_limb", "a_g_ratio_hecs")]))
df_plot<-merge(df_plot, group, by.x = "id", by.y = "partID", all = FALSE)

df_plot<-df_plot[with(df_plot, order(visit, id)),]

p1<-ggpaired(df_plot[df_plot$groupID == 1,], x = "visit", y = "totalbodyfatperc",
         color = "visit", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Total Body Fat Percentage")

p2<-ggpaired(df_plot[df_plot$groupID == 1,], x = "visit", y = "FMI",
         color = "visit", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Fat Mass Index [kg/m2]")

p3<-ggpaired(df_plot[df_plot$groupID == 1,], x = "visit", y = "a_g_ratio_hecs",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Android to Gynoid Ratio")

p4<-ggpaired(df_plot[df_plot$groupID == 1,], x = "visit", y = "trunk_to_limb",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Trunk to Limb Fat Ratio")

p5<-ggpaired(df_plot[df_plot$groupID == 1,], x = "visit", y = "trunk_to_leg",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Trunk to Leg Fat Ratio")

grid.arrange(p1, p2, p3, p4, p5, nrow = 2, top = textGrob("Exercise Participants",gp=gpar(fontsize=20,font=3)))


p1<-ggpaired(df_plot[df_plot$groupID == 2,], x = "visit", y = "totalbodyfatperc",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Total Body Fat Percentage")

p2<-ggpaired(df_plot[df_plot$groupID == 2,], x = "visit", y = "FMI",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Fat Mass Index [kg/m2]")

p3<-ggpaired(df_plot[df_plot$groupID == 2,], x = "visit", y = "a_g_ratio_hecs",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Android to Gynoid Ratio")

p4<-ggpaired(df_plot[df_plot$groupID == 2,], x = "visit", y = "trunk_to_limb",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Trunk to Limb Fat Ratio")

p5<-ggpaired(df_plot[df_plot$groupID == 2,], x = "visit", y = "trunk_to_leg",
             color = "visit", line.color = "gray", line.size = 0.4,
             palette = "jco")+
  stat_compare_means(paired = TRUE) + ggtitle("Trunk to Leg Fat Ratio")

grid.arrange(p1, p2, p3, p4, p5, nrow = 2, top = textGrob("Stretching Participants",gp=gpar(fontsize=20,font=3)))
####################################################################################################
####################################################################################################
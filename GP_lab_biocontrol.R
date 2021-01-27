# Laboratory evaluation of Gryon pennsylvanicum on Leptoglossus zonatus
# Rob Straser
# 3-11-2020

# set working directory
setwd("~/Documents/Wilson Lab/Dissertation/Ch1_biocontrol/Gryon life parameters/life history/null parameters")

# load packages
library(ggplot2)
library(viridis)
library(pipeR)
library(ggpubr)
library(survival)
library(survminer)




# function to calculate the mean and the SD or SEM for each group
# varname : the name of a column containing the variable to be summarized
# groupnames : vector of column names to be used as grouping variables

# for generating standard deviation
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))}
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}

# for generating standard error
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]]/sqrt(length(x[[col]])), na.rm=TRUE))}
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)}



###
###
###



# generate life history parameters

# read in "GP.life.tables" data
GP.lt <- read.csv ("GP.life.parameters.csv")
head(GP.lt)
# set "treatment" as factor variable
GP.lt$treatment <- as.factor(GP.lt$treatment)



# adult female longevity + SE
longevity <- data_summary(GP.lt, varname="longev",
                         groupnames=c("treatment"))

# ovipositional period + SE
oviposition <- data_summary(GP.lt, varname="ovi.per",
                          groupnames=c("treatment"))

# post-reporoductive period + SE
post.repo <- data_summary(GP.lt, varname="post.repo",
                            groupnames=c("treatment"))

# total female progeny + SE
fem.progeny <- data_summary(GP.lt, varname="tot.fem",
                        groupnames=c("treatment"))

# total male progeny + SE
male.progeny <- data_summary(GP.lt, varname="tot.male",
                        groupnames=c("treatment"))


# total progeny + SE
progeny <- data_summary(GP.lt, varname="tot.prog",
                          groupnames=c("treatment"))

# total dissected progeny + SE
diss.progeny <- data_summary(GP.lt, varname="tot.diss",
                        groupnames=c("treatment"))

# developement time (female) + SE
dev.time <- data_summary(GP.lt, varname="dev.time",
                        groupnames=c("treatment"))

# sex ratio + SE
sex.ratio <- data_summary(GP.lt, varname="sex.ratio",
                        groupnames=c("treatment"))



# print results from all variables + SE for honey-fed parasitoids
longevity
oviposition
post.repo
fem.progeny
male.progeny
progeny
diss.progeny
dev.time
sex.ratio


###
###
###



# Create figure on parasitoid fecundity (offspring/female/day)
# and female longevity (%) using "GP fucundity data" and "life table data"

# read in "GP fecundity data"
ltp <- read.csv ("GP.fecundity.csv")
head(ltp)

# set "treatment" as factor variable
ltp$treatment <- as.factor(ltp$treatment)

# subset treatment "3" - feed diet x3 per week
df1 <- subset(ltp, treatment == 3)
# create data_summary
df2 <- data_summary(df1, varname="GP.tot", 
                    groupnames=c("day"))

# read in "life table data"
life_table <- read.csv ("GP.life.tables.csv")
# subset treatment "3"
df3 <- subset(life_table, treatment == 3)


# indicate zero GP emergance on days 53 & 55 to df2
newRow1 <- data.frame(day = 57, GP.tot=0, se=0) 
newRow2 <- data.frame(day = 59, GP.tot=0, se=0) 
df2 <- rbind(df2, newRow1, newRow2)

# bind dataframe "df2" with "lx" column from "life table data"
newdf2 <- cbind(df2, df3$lx)


# create plot on fecundity & longevity per day
fec_long <- newdf2 %>>% ggplot() + 
  aes(x=day, y=GP.tot) + 
  geom_line() +
  geom_errorbar(aes(ymin=GP.tot-se, ymax=GP.tot+se), width=.75, position=position_dodge(0))+
  geom_point(shape=21, size=2, fill="white") +
  geom_point(aes(x = day, y = df3$lx * 20), shape=21, size=2, fill="black") + 
  geom_line(aes(x = day, y = df3$lx * 20)) +
  xlab("Days") + 
  ylab("Offspring / female / day") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 5, name = "Female survival (%)"))+
  scale_x_continuous(breaks = seq(1, 56, by = 5)) +
  theme_pubr()
fec_long

# add border
fec_long + theme(axis.text.y   = element_text(size=14),
                   axis.text.x   = element_text(size=14),
                   axis.title.y  = element_text(size=14),
                   axis.title.x  = element_text(size=14),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   panel.border = element_rect(colour = "black", fill=NA, size=0.75))





###
###
###



# calculate parasitoid life table parameters

# subset treatment "3" from life_table 
life_table3 <- subset(life_table, treatment == 3)

# construct life table
lt3 <- life_table3 %>%
  mutate("lx*mx"=lx*mx,
         "x*lx*mx"=Age*lx*mx,
         "R0"=sum(lx*mx_net),
         "T"=sum(Age*lx*mx)/(sum(lx*mx)),
         "r"=log(R0)/T,
         "rm"=(log(sum(lx*mx)))/T,
         "lambda"=exp(rm),
         "Td"=(log(2))/rm,
         "GRR"=sum(mx_net),
         "lx*mx*e^-rx"= lx*mx*exp(-r*Age))
lt3


# calculate life table values with SD

# read in cummulative life table data 
life_table_x <- read.csv ("Tot.life.tables.csv")
life_table_x


# for generating life table values
function.lt <- function(data){
  require(dplyr)
  lt <- data %>%
    group_by(ID) %>%
    mutate("Treat" == Treat,
           "lx*mx"=lx*mx,
           "x*lx*mx"=Age*lx*mx,
           "R0"=sum(lx*mx_net),
           "T"=sum(Age*lx*mx)/(sum(lx*mx)),
           "r"=log(R0)/T,
           "rm"=(log(sum(lx*mx)))/T,
           "lambda"=exp(rm),
           "Td"=(log(2))/rm,
           "GRR"=sum(mx_net),
           "lx*mx*e^-rx"= lx*mx*exp(-r*Age))
  return(lt)}

# use function on life table data
lt_x <- function.lt(life_table_x)


# collate life table values from all desired variables for each parasitoid ID
df.sum <- lt_x %>%
  select(Treat, ID, R0, T, r, rm, lambda, Td, GRR) %>% # select variables to summarise
  summarise_each(funs(x = mean)) # takes the average value (however each value within life table variable is identical)
df.sum


# calculate mean and SD (to determine spread of data) for life table variables
library(doBy)

rm.sum <- summaryBy(rm_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
lambda.sum <- summaryBy(lambda_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
T.sum <- summaryBy(T_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
Td.sum <- summaryBy(Td_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
R0.sum <- summaryBy(R0_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
GRR.sum <- summaryBy(GRR_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))




###
###
###




# create kaplan survivalship curves for female and male wasps
# under diet treatments (h20, honey, honey&host)

# read in "Glongev_fec_female_male" data
longev <- read.csv ("GP.longev.fec.csv")
# label variables as factor or numeric
longev$futime <- as.numeric(longev$futime)
longev$fustat <- as.numeric(longev$fustat)
longev$treat <- as.factor(longev$treat)
longev$sex <- as.factor(longev$sex)
longev$ID <- as.factor(longev$ID)

# Change the levels of factor "treat"
levels(longev$treat) <- c("water-host","honey","honey-host")

# subset female and male wasps
fem <- subset(longev, sex == "f")
mal <- subset(longev, sex == "m")


# FEMALE kaplan survival curves (h20, honey, honey-host)
# create survfit data structure
surv_object_fem <- Surv(time = fem$futime, event = fem$fustat)
fit_fem <- survfit(surv_object_fem ~ treat, data = fem)
summary(fit_fem)


# plot FEMALE survival curve
ggsurvplot(fit_fem, 
           size = 0.5, conf.int = TRUE,
           linetype = 1,
           xlab="Days",
           xlim=c(0,140),
           break.time.by = 10,
           ylab = "Female survival probability (%)",
           ggtheme = theme(axis.text.y = element_text(size=14),
                           axis.text.x   = element_text(size=14),
                           axis.title.y  = element_text(size=14),
                           axis.title.x  = element_text(size=14),
                           panel.background = element_blank(),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           axis.line = element_line(colour = "black"),
                           panel.border = element_rect(colour = "black", fill=NA, size=0.75)),
           palette = c("lightgrey","darkgrey","black"),
           fun = function(y) y*100)

# statistics
library(survival)
# Pairwise survdiff between treatments
res_fem <- pairwise_survdiff(Surv(futime, fustat) ~ treat, data = fem)
res_fem


# MALE kaplan survival curves (h20, honey, honey-host)
# create survfit data structure
surv_object_mal <- Surv(time = mal$futime, event = mal$fustat)
fit_mal <- survfit(surv_object_mal ~ treat, data = mal)
summary(fit_mal)

#plot MALE survival curve
ggsurvplot(fit_mal, 
           size = 0.5, conf.int = TRUE,
           linetype = 1,
           xlab="Days",
           xlim=c(0,140),
           break.time.by = 10,
           ylab = "Male survival probability (%)",
           ggtheme = theme(axis.text.y = element_text(size=14),
                           axis.text.x   = element_text(size=14),
                           axis.title.y  = element_text(size=14),
                           axis.title.x  = element_text(size=14),
                           panel.background = element_blank(),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           axis.line = element_line(colour = "black"),
                           panel.border = element_rect(colour = "black", fill=NA, size=0.75)),
           palette = c("lightgrey","darkgrey","black"),
           fun = function(y) y*100)

# statistics
library(survival)
# Pairwise survdiff between treatments
res_mal <- pairwise_survdiff(Surv(futime, fustat) ~ treat, data = mal)
res_mal





###
###
###



# creat barplot on wasp longevity between diet treatments


df.long <- data_summary(longev, varname="futime", 
                  groupnames=c("sex", "treat"))

# Change the levels of factor "treat"
levels(df.long$treat) <- c("water-host","honey","honey-host")

# creat barplot
bp.long <- ggplot(df.long, aes(x=treat, y=futime, fill=sex)) +
           geom_bar(stat="identity", color="black", position=position_dodge()) +
           geom_errorbar(aes(ymin=futime, ymax=futime + se), 
                width=0.2, position=position_dodge(width=0.9)) +
           scale_y_continuous(name="Adult longevity (days)", breaks = seq(0, 120, by = 20)) +
           scale_fill_manual(name="", labels=c("female", "male"), values=c("white","black")) +
           theme(axis.title.x = element_blank()) +
           theme_pubr()
bp.long


# Alternative bar plot of means with SE
# Change the levels of factor "treat"
levels(longev$treat) <- c("water-host","honey","honey-host")
levels(longev$sex) <- c("female","male")


a <- ggbarplot(longev, x = "treat", y = "futime", 
          fill = "sex", color = "black", palette = c("darkgrey", "white"),
          add = c("mean_se"),
          position = position_dodge(0.75),
          ylim = c(0, 140),
          yticks.by = 20,
          ylab = "Adult longevity (days)")
        

# add border
a +  theme(axis.text.y = element_text(size=14),
                   axis.text.x   = element_text(size=14),
                   axis.title.y  = element_text(size=14),
                   axis.title.x  = element_text(size=14),
                   panel.background = element_blank(),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   axis.line = element_line(colour = "black"),
                   panel.border = element_rect(colour = "black", fill=NA, size=0.75))



# statistics

# merge 'sex' with 'treat' as joined variable
longev$sex_treat <- paste(longev$sex,longev$treat)
longev

# test normality within groups
with(longev, tapply(futime, sex_treat, shapiro.test))
# some variables deviate from normality, use nonparametric test

# Kruskal Wallace test for difference of means
kruskal.test(futime ~ sex_treat, data = longev)

# Dunn test for mulitple comparisons of means
library(FSA)
dunnTest(futime ~ sex_treat, data = longev, method = "bonferroni")







###
###



# Assess wasp sex ratio (% female) through time (days)

# create column for sex ratio (proportion female/ total)
ltp$sex.ratio <- (ltp$GP.em.F / ltp$GP.tot)* 100
head(ltp)

ltp.sub <- subset(ltp, treatment == 3)

# create data summery
df.sex <- data_summary(ltp, varname="sex.ratio", 
                    groupnames=c("day"))

# plot scatter plot for sex ratio through time (days)
sex.ratio <- ggplot(df.sex, aes(x=day, y=sex.ratio)) + 
             geom_errorbar(aes(ymin=sex.ratio-se, ymax=sex.ratio+se), width=.2) +
             geom_point(shape=21, size=2, fill="white") +
             geom_smooth(method = "lm", color = "black", size=0.5, lty=2, se = F) +
             scale_x_continuous(name="Days", breaks = seq(1, 11, by = 2), limits=c(1, 11)) +
             scale_y_continuous(name="Sex ratio (% female)", limits=c(60, 100)) +
             theme_pubr()
sex.ratio

# add border
sex.ratio + theme(axis.text.y   = element_text(size=14),
                  axis.text.x   = element_text(size=14),
                  axis.title.y  = element_text(size=14),
                  axis.title.x  = element_text(size=14),
                  panel.background = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour = "black"),
                  panel.border = element_rect(colour = "black", fill=NA, size=0.75))

# statistics
ltp3.sub <- subset(ltp.sub, day < 13)


# Kruskal Wallace test for difference of means
kruskal.test(sex.ratio ~ day, data=ltp3.sub)
# no significant difference 

# regression model
fit <- lm(sex.ratio ~ day, data=ltp3.sub)
summary(fit) # show results


###
###
###


# Non-reproductive parasitism (% sterile eggs) over wasp age (days)


# read in "GP fecundity data"
ltp <- read.csv ("GP.fecundity.csv")
head(ltp)

# set "treatment" as factor variable
ltp$treatment <- as.factor(ltp$treatment)

# subset treatment "3" (fed honey 3X per week)
ltp.nr <- subset(ltp, treatment == 3)

# create data_summary
ltp.sum <- data_summary(ltp.nr, varname="sterile.perc", 
                    groupnames=c("treatment", "day"))
ltp.tot.sum <- data_summary(ltp.nr, varname="Tot.mort.perc", 
                        groupnames=c("treatment", "day"))

# plotting

r<- ggplot(ltp.sum, aes(x=day, y=sterile.perc)) + 
  geom_errorbar(aes(ymin=sterile.perc-se, ymax=sterile.perc+se), width=.75) +
  geom_point(shape=21, size=2, fill="black") +
  geom_line() +
  scale_x_continuous(name="Days", breaks = seq(0,50,5), limits = c(0, 50)) +
  scale_y_continuous(name="Non-reproductive mortality (%)", limits=c(0, 100)) +
  theme_pubr() 
r

s<- ggplot(ltp.tot.sum, aes(x=day, y=Tot.mort.perc)) + 
  geom_errorbar(aes(ymin=Tot.mort.perc-se, ymax=Tot.mort.perc+se), width=.75) +
  geom_point(shape=21, size=2, fill="black") +
  geom_line() +
  scale_x_continuous(name="Days", breaks = seq(0,50,5), limits = c(0, 50)) +
  scale_y_continuous(name="Non-reproductive mortality (%)", limits=c(0, 100)) +
  theme_pubr() 
s



# joining plots

# reconfigure data frames: add factor 'variable' column, rename 'mean's column
ltp.sum$variable  <- 'sterile'
names(ltp.sum)[3] <- "mean"
ltp.sum

ltp.tot.sum$variable  <- 'total mortality'
names(ltp.tot.sum)[3] <- "mean"
ltp.tot.sum

# combine data frames by row (identical column names)
ltp.combine <- rbind(ltp.tot.sum, ltp.sum)




t<- ggplot(ltp.combine, aes(x=day, y=mean, color = variable, group = variable)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.75) +
  geom_point(aes(fill = variable)) +
  geom_line(aes(linetype = variable)) +
  scale_color_manual(values=c("black", "grey"), 
                     name="",
                     breaks=c("sterile", "total mortality"),
                     labels=c("non-reproductive", "total mortality")) +
  scale_fill_manual(values=c("white", "white")) +
  scale_x_continuous(name="Days", breaks = seq(0,50,5), limits = c(0, 50)) +
  scale_y_continuous(name="Host egg mortality (%)", limits=c(0, 100)) +
  theme_pubr() 
t




t <- ggplot(ltp.combine, aes(x=day, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.75) +
  geom_line(aes(linetype = variable)) +
  geom_point(aes(colour=factor(variable), 
                 fill = factor(variable)), shape=21) + 
  scale_fill_manual(values=c("black", "white")) + 
  scale_colour_manual(values=c("black", "black"), name="",
                      breaks=c("sterile", "total mortality"),
                      labels=c("non-reproductive", "total mortality")) +
  scale_x_continuous(name="Days", breaks = seq(0,50,5), limits = c(0, 50)) +
  scale_y_continuous(name="Host egg mortality (%)", limits=c(0, 100)) +
  theme_pubr() 
t

# add border
t + theme(axis.text.y = element_text(size=14),
          axis.text.x   = element_text(size=14),
          axis.title.y  = element_text(size=14),
          axis.title.x  = element_text(size=14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.75))



# statistics

head(ltp.nr) 
table(ltp.nr$treatment)
        
# Kruskal Wallace test for difference of means
kruskal.test(sterile ~ treatment, data=ltp.nr)
# significant difference 

# regression model
ltp3.nr <- subset(ltp.nr, treatment == 3)
fit2 <- lm(sterile ~ day, data=ltp3.nr)
summary(fit2) # show results






###
### host egg age viability
###


### read in egg viability dataset
egg.age.t <- read.csv ("lfb_egg_age_parasitism_trans.csv", sep=",", header=TRUE)
head(egg.age.t)


# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
library(Rmisc) 
para.se.t <- summarySE(egg.age.t, measurevar="value", groupvars=c("treatment", "var"), na.rm = TRUE)

para.se.t <- subset(para.se.t, var == "lfb.perc" |
                               var == "un.lfb.perc"|
                               var == "tot.gp.perc"|
                               var == "un.gp.perc"|
                               var == "sterile.perc")


levels(para.se.t$var) <- list("LZ nymph" = "lfb.perc", "LZ unemerged" = "un.lfb.perc", 
                              "GP adult" = "tot.gp.perc", "GP unemerged" = "un.gp.perc", "sterile" = "sterile.perc")



# Default bar plot
p<- ggplot(para.se.t, aes(x=var, y=value, fill=treatment)) + 
    labs(x="", y = "Host egg product (%)")+
    geom_bar(stat="identity", color="black", position=position_dodge(width = 0.8), width=0.7) +
    geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.2, position=position_dodge(width = 0.8)) +
    scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
    theme_pubr() + scale_fill_manual(values=c("grey30", "grey50", "grey80", "white"))

p

# add border
p + theme(axis.text.y = element_text(size=14),
          axis.text.x   = element_text(size=14),
          axis.title.y  = element_text(size=14),
          axis.title.x  = element_text(size=14),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.75))



#statistics

# subset each variabile
LZ.sub <- subset(egg.age.t, var == "lfb.perc")
LZun.sub <- subset(egg.age.t, var == "un.lfb.perc")
GP.sub <- subset(egg.age.t, var == "tot.gp.perc")
GPun.sub <- subset(egg.age.t, var == "un.gp.perc")
sterile.sub <- subset(egg.age.t, var == "sterile.perc")

# test normality within groups
with(LZ.sub, tapply(value, treatment, shapiro.test))
with(LZun.sub, tapply(value, treatment, shapiro.test))
with(GP.sub, tapply(value, treatment, shapiro.test))
with(GPun.sub, tapply(value, treatment, shapiro.test))
with(sterile.sub, tapply(value, treatment, shapiro.test))
# some variables deviate from normality, use nonparametric test


# "LZ numph" Kruskal Wallace test for difference of means
kruskal.test(value ~ treatment, data = LZ.sub)
# no significant difference between treatments

# "LZ unemerged" Kruskal Wallace test for difference of means
kruskal.test(value ~ treatment, data = LZun.sub)
# no significant difference between treatments

# "GP adult" Kruskal Wallace test for difference of means
kruskal.test(value ~ treatment, data = GP.sub)
# no significant difference between treatments

# "GP unemerged" Kruskal Wallace test for difference of means
kruskal.test(value ~ treatment, data = GPun.sub)
# no significant difference between treatments

# "sterile" Kruskal Wallace test for difference of means
kruskal.test(value ~ treatment, data = sterile.sub)
# no significant difference between treatments






# subset to include only variables of interest
egg.age.sub <- subset(egg.age.t, var == "lfb.perc" |
                                 var == "un.lfb.perc" |
                                 var == "tot.gp.perc" |
                                 var == "un.gp.perc" |
                                 var == "sterile.perc")


# merge 'sex' with 'treat' as joined variable
egg.age.sub$var_treat <- paste(egg.age.sub$variable, egg.age.sub$treatment)
head(egg.age.sub)

# Kruskal Wallace test for difference of means
kruskal.test(value ~ var_treat, data = egg.age.sub)

# Dunn test for mulitple comparisons of means
library(FSA)
dunnTest(value ~ var_treat, data = egg.age.sub, method = "bonferroni")



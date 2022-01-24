#********************
# reproductive parameters
#********************

# read in data
GP.lt <- read.csv ("GP.life.parameters.csv")


#********************
# data prep
#********************

# set "treatment" as factor variable
GP.lt$treatment <- as.factor(GP.lt$treatment)



#********************
# analyses
#********************

# adult female longevity + SE
longevity <- data_summary(GP.lt, 
                          varname="longev",
                          groupnames=c("treatment"))

# ovipositional period + SE
oviposition <- data_summary(GP.lt, 
                            varname="ovi.per",
                            groupnames=c("treatment"))

# post-reporoductive period + SE
post.repo <- data_summary(GP.lt, 
                          varname="post.repo",
                          groupnames=c("treatment"))

# total female progeny + SE
fem.progeny <- data_summary(GP.lt, 
                            varname="tot.fem",
                            groupnames=c("treatment"))

# total male progeny + SE
male.progeny <- data_summary(GP.lt, 
                             varname="tot.male",
                             groupnames=c("treatment"))

# total progeny + SE
progeny <- data_summary(GP.lt, 
                        varname="tot.prog",
                        groupnames=c("treatment"))

# total dissected progeny + SE
diss.progeny <- data_summary(GP.lt, 
                             varname="tot.diss",
                             groupnames=c("treatment"))

# developement time (female) + SE
dev.time <- data_summary(GP.lt, 
                         varname="dev.time",
                         groupnames=c("treatment"))

# sex ratio + SE
sex.ratio <- data_summary(GP.lt, 
                          varname="sex.ratio",
                          groupnames=c("treatment"))

# print
longevity
oviposition
post.repo
fem.progeny
male.progeny
progeny
diss.progeny
dev.time
sex.ratio


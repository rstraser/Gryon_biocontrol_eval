# ********************
# parasitoid survival
# ********************

# read in data
longev <- read.csv ("GP.longev.fec.csv")



#********************
# data prep
#********************

# classify variable structure
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



#********************
#  female analyses & plot
#********************

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

# statistics - pairwise survdiff between treatments
res_fem <- pairwise_survdiff(Surv(futime, fustat) ~ treat, data = fem)

# print
res_fem







#********************
#  male analyses & plot
#********************


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

# statistics - pairwise survdiff between treatments
res_mal <- pairwise_survdiff(Surv(futime, fustat) ~ treat, data = mal)

# print
res_mal



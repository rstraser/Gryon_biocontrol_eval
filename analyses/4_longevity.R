#********************
# parasitoid longevity
#********************


# read in data
longev <- read.csv ("GP.longev.fec.csv")


#********************
# data prep
#********************

# adult longevity + SE by sex within treatment
df.long <- data_summary(longev, 
                        varname="futime", 
                        groupnames=c("sex", "treat"))

# change the levels of factor "treat"
levels(df.long$treat) <- c("water-host","honey","honey-host")

# change the levels of factor "treat"
levels(longev$treat) <- c("water-host","honey","honey-host")
levels(longev$sex) <- c("female","male")



#********************
# plot
#********************

# create barplot
longev_barplot <- ggbarplot(longev, x = "treat", y = "futime", 
                             fill = "sex", 
                             color = "black", 
                             palette = c("darkgrey", "white"),
                             add = c("mean_se"),
                             position = position_dodge(0.75),
                             ylim = c(0, 130),
                             yticks.by = 20,
                             ylab = "Adult longevity (days)")

# clean plot structure
longev_bar <- longev_barplot +  theme(axis.text.y = element_text(size=14),
                                 axis.text.x   = element_text(size=14),
                                 axis.title.y  = element_text(size=14),
                                 axis.title.x  = element_text(size=14),
                                 panel.background = element_blank(),
                                 panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 axis.line = element_line(colour = "black"),
                                 panel.border = element_rect(colour = "black", fill=NA, size=0.75))

# print
longev_bar



#********************
# statistics
#********************

mod <- glmer(futime ~ sex*treat + (1|ID), family=Gamma(link="identity"), data=longev)
summary(mod)
drop1(mod, test="Chisq")

# check model assumptions
mod.res <- resid(mod)
qqnorm(mod.res)
plot(density(mod.res))


# Tukey's test
# merge 'sex' with 'treat'
longev$sex_treat <- paste(longev$sex,longev$treat)
# model
ph.mod <- glmer(futime ~ sex_treat + (1|ID), family=Gamma(link="identity"), data=longev)
summary(ph.mod)
tuk <- glht(ph.mod, linfct=mcp(sex_treat="Tukey")) 
summary(tuk)
tuk.cld <- cld(tuk) 
tuk.cld









#********************
# host age
#********************


# read in data
egg.age.t <- read.csv ("lfb_egg_age_parasitism_trans.csv")
head(egg.age.t)



#********************
# data prep
#********************

# summarize
para.se.t <- summarySE(egg.age.t, 
                       measurevar="value", 
                       groupvars=c("treatment", "var"), 
                       na.rm = TRUE)


# subset variables
para.se.t <- subset(para.se.t, var == "lfb.perc" |
                               var == "un.lfb.perc"|
                               var == "tot.gp.perc"|
                               var == "un.gp.perc"|
                               var == "sterile.perc")

# assign levels
levels(para.se.t$var) <- list("LZ nymph" = "lfb.perc", 
                              "LZ unemerged" = "un.lfb.perc", 
                              "GP adult" = "tot.gp.perc", 
                              "GP unemerged" = "un.gp.perc", 
                              "sterile" = "sterile.perc")


# summarize means of each group
all.avg <- data_summary(para.se.t, 
                    varname="value", 
                    groupnames=c("var"))




#********************
# plot
#********************

egg_age <- ggplot(para.se.t, aes(x=var, y=value, fill=treatment)) + 
                  labs(x="", y = "Host egg product (%)")+
                  geom_bar(stat="identity", 
                           color="black", 
                           position=position_dodge(width = 0.8), width=0.7) +
                  geom_errorbar(aes(ymin=value, ymax=value+se), 
                                width=.2, 
                                position=position_dodge(width = 0.8)) +
                  scale_y_continuous(limits = c(0, 80), 
                                     breaks = seq(0, 80, by = 10)) +
                  theme_pubr() + 
                  scale_fill_manual(values=c("black", "grey65", "grey85", "white"))

# clean plot structure
age_plot <- egg_age + theme(axis.text.y = element_text(size=14),
                            axis.text.x   = element_text(size=14),
                            axis.title.y  = element_text(size=14),
                            axis.title.x  = element_text(size=14),
                            panel.background = element_blank(),
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            axis.line = element_line(colour = "black"),
                            panel.border = element_rect(colour = "black", fill=NA, size=0.75))

# print
age_plot




#********************
# statistics
#********************

# subset variables 
egg.age.sub <- subset(egg.age.t, var == "lfb.perc" |
                                 var == "un.lfb.perc" |
                                 var == "tot.gp.perc" |
                                 var == "un.gp.perc" |
                                 var == "sterile.perc")

# merge 'sex' with 'treat'
egg.age.sub$var_treat <- paste(egg.age.sub$variable, egg.age.sub$treatment)
head(egg.age.sub)

# transform proportion data to 0-1
egg.age.sub$prop <- egg.age.sub$value / 100


# subset variables
LZ.sub <- subset(egg.age.sub, var == "lfb.perc")
LZun.sub <- subset(egg.age.sub, var == "un.lfb.perc")
GP.sub <- subset(egg.age.sub, var == "tot.gp.perc")
GPun.sub <- subset(egg.age.sub, var == "un.gp.perc")
sterile.sub <- subset(egg.age.sub, var == "sterile.perc")


#********************


# LZ nymphs model
mod5 <- glmer(prop ~ var_treat + (1|ID), family=binomial(link = "logit"), data=LZ.sub)
summary(mod5)

# tukey test
tuk <- glht(mod5, linfct=mcp(var_treat="Tukey")) #this is the actual Tukey's analysis
summary(tuk)
tuk.cld <- cld(tuk) #assign the significance letters
tuk.cld


# check assumptions
mod5.res <- resid(mod5)
qqnorm(mod5.res)
plot(density(mod5.res))



#********************


# LZ unemerged model
mod6 <- glmer(prop ~ var_treat + (1|ID), family=binomial(link = "logit"), data=LZun.sub)
summary(mod6)

# check assumptions
mod6.res <- resid(mod6)
qqnorm(mod6.res)
plot(density(mod6.res))



#********************


# GP adults model
mod7 <- glmer(prop ~ var_treat + (1|ID), family=binomial(link = "logit"), data=GP.sub)
summary(mod7)

# check assumptions
mod7.res <- resid(mod7)
qqnorm(mod7.res)
plot(density(mod7.res))


#********************

# GP unemerged model
mod8 <- glmer(prop ~ var_treat + (1|ID), family=binomial(link = "logit"), data=GPun.sub)
summary(mod8)

# check assumptions
mod8.res <- resid(mod8)
qqnorm(mod8.res)
plot(density(mod8.res))


#********************

# sterile eggs model
mod9 <- glmer(prop ~ var_treat + (1|ID), family=binomial(link = "logit"), data=sterile.sub)
summary(mod9)

# check assumptions
mod9.res <- resid(mod9)
qqnorm(mod9.res)
plot(density(mod9.res))




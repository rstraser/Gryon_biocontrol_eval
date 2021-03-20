#********************
# host age
#********************


# read in data
egg.age.t <- read.csv ("lfb_egg_age_parasitism_trans.csv")


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




#********************
# plot
#********************

egg_age <- ggplot(para.se.t, aes(x=var, y=value, fill=treatment)) + 
                  labs(x="", y = "Host egg product (%)")+
                  geom_bar(stat="identity", 
                           color="black", 
                           position=position_dodge(width = 0.8), width=0.7) +
                  geom_errorbar(aes(ymin=value-se, ymax=value+se), 
                                width=.2, 
                                position=position_dodge(width = 0.8)) +
                  scale_y_continuous(limits = c(0, 80), 
                                     breaks = seq(0, 80, by = 20)) +
                  theme_pubr() + 
                  scale_fill_manual(values=c("grey30", "grey50", "grey80", "white"))

# clean plot structure
age_plot <- ege_age + theme(axis.text.y = element_text(size=14),
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

# subset variables
LZ.sub <- subset(egg.age.sub, var == "lfb.perc")
LZun.sub <- subset(egg.age.sub, var == "un.lfb.perc")
GP.sub <- subset(egg.age.sub, var == "tot.gp.perc")
GPun.sub <- subset(egg.age.sub, var == "un.gp.perc")
sterile.sub <- subset(egg.age.sub, var == "sterile.perc")


#********************

# LZ nymphs model
mod5 <- glmer(value ~ var_treat + (1|ID), family=poisson, data=LZ.sub)
summary(mod5)
drop1(mod5, test="Chisq") #under the output 'LRT' is the chi-square

# Tukey's test
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
mod6 <- glmer(value ~ var_treat + (1|ID), family=poisson, data=LZun.sub)
summary(mod6)
drop1(mod6, test="Chisq") #under the output 'LRT' is the chi-square

# Tukey's test
tuk <- glht(mod6, linfct=mcp(var_treat="Tukey")) #this is the actual Tukey's analysis
summary(tuk)
tuk.cld <- cld(tuk) #assign the significance letters
tuk.cld


#********************

# GP adults model
mod7 <- glmer(value ~ var_treat + (1|ID), family=poisson, data=GP.sub)
summary(mod7)
drop1(mod7, test="Chisq") #under the output 'LRT' is the chi-square

# Tukey's test
tuk <- glht(mod7, linfct=mcp(var_treat="Tukey")) #this is the actual Tukey's analysis
summary(tuk)
tuk.cld <- cld(tuk) #assign the significance letters
tuk.cld


#********************

# GP unemerged model
mod8 <- glmer(value ~ var_treat + (1|ID), family=poisson, data=GPun.sub)
summary(mod8)
drop1(mod8, test="Chisq") #under the output 'LRT' is the chi-square

# Tukey's test
tuk <- glht(mod8, linfct=mcp(var_treat="Tukey")) #this is the actual Tukey's analysis
summary(tuk)
tuk.cld <- cld(tuk) #assign the significance letters
tuk.cld


#********************

# sterile eggs model
mod9 <- glmer(value ~ var_treat + (1|ID), family=poisson, data=sterile.sub)
summary(mod9)
drop1(mod9, test="Chisq") #under the output 'LRT' is the chi-square

# Tukey's test
tuk <- glht(mod9, linfct=mcp(var_treat="Tukey")) #this is the actual Tukey's analysis
summary(tuk)
tuk.cld <- cld(tuk) #assign the significance letters
tuk.cld




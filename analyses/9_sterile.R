#********************
# sterile: non-reproductive mortality
#********************


# read in data
ltp <- read.csv ("GP.fecundity.csv")



#********************
# data prep
#********************

# set "treatment" as factor variable
ltp$treatment <- as.factor(ltp$treatment)

# subset data: remove treatment "0" (water diet)
ltp.nr <- subset(ltp, treatment == "3" | treatment == "cont")

# create data_summary
ltp.sum <- data_summary(ltp.nr, varname="sterile.perc", 
                        groupnames=c("treatment", "day"))
ltp.tot.sum <- data_summary(ltp.nr, varname="Tot.mort.perc", 
                            groupnames=c("treatment", "day"))


# reconfigure data frames: 
# add factor 'variable' column, rename 'mean's column
ltp.sum$variable  <- 'sterile'
names(ltp.sum)[3] <- "mean"
ltp.sum

ltp.tot.sum$variable  <- 'total mortality'
names(ltp.tot.sum)[3] <- "mean"
ltp.tot.sum

# combine data frames by row (identical column names)
ltp.combine <- rbind(ltp.tot.sum, ltp.sum)
# label control treatment as "cont" under "variable"
ltp.combine$variable <- ifelse(ltp.combine$treatment %in% 
                                 "cont", "cont", ltp.combine$variable)



# subset to exclude control
ltp.combine2 <- subset(ltp.combine, variable == "sterile" |
                         variable == "total mortality")
ltp.combine2$variable <- factor(ltp.combine2$variable, levels = c('total mortality', 'sterile'))




#********************
# plot
#********************


ster_barplot <- ggplot(ltp.combine2, aes(x=day, y=mean, fill=variable)) + 
                geom_bar(stat="identity", color="black", 
                position=position_dodge()) +
                xlab(" ") + ylab("Percentage (%)") + 
                geom_errorbar(aes(ymin=mean, ymax=mean+se), width= 1,
                position=position_dodge(1.7)) +
                scale_fill_manual(values=c('grey80','black')) +
                scale_x_continuous(name="Days", 
                     breaks = seq(1,39,2), 
                     limits = c(0, 40))+
                scale_y_continuous(breaks = seq(0,100,20), 
                     limits = c(0, 100))
# clean plot structure
ster_bar <- ster_barplot +  theme(axis.text.y = element_text(size=14),
                                  axis.text.x   = element_text(size=14),
                                  axis.title.y  = element_text(size=14),
                                  axis.title.x  = element_text(size=14),
                                  panel.background = element_blank(),
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(colour = "black"),
                                  panel.border = element_rect(colour = "black", fill=NA, size=0.75))

# print
ster_bar






#********************
# statistics
#********************

# test difference of means between 
# sterile eggs and unexposed eggs on day 1

# subset day 1
para.day1 <- subset(ltp.nr, day == 1)


# model
mod2 <- glmer(sterile ~ treatment + (1|ID), family=poisson, data=para.day1)
summary(mod2)
drop1(mod2, test="Chisq") #under the output 'LRT' is the chi-square

### check mod assumptions
mod2.res <- resid(mod2)
qqnorm(mod2.res)





# test proportion of sterile eggs ~ days
# create dataframe: proportion of sterile to total egg mortality
ltp.nr3 <- subset(ltp.nr, treatment == 3)
prop.ltp.nr <- ltp.nr3[, c("ID", "day", "Tot.mort.perc", "sterile.perc")]
prop.ltp.nr$prop.sterile <- prop.ltp.nr$sterile.perc / prop.ltp.nr$Tot.mort.perc
head(prop.ltp.nr)

# model
prop.fit <- glmer(prop.sterile ~ day + (1|ID), family=binomial(link = "logit"), data=prop.ltp.nr)
summary(prop.fit)
drop1(prop.fit, test="Chisq") #under the output 'LRT'

# check assumptions
prop.fit.res <- resid(prop.fit)
qqnorm(prop.fit.res)
plot(density(prop.fit.res))






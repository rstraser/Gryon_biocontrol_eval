#********************
# sex ratio
#********************

# read in data
ltp <- read.csv ("GP.fecundity.csv")


#********************
# data prep
#********************

# create column for sex ratio (proportion female/ total)
ltp$sex.ratio <- (ltp$GP.em.F / ltp$GP.tot)* 100

# subset treatment "3"
ltp.sub <- subset(ltp, treatment == 3)

# create data summery
df.sex <- data_summary(ltp, 
                       varname="sex.ratio", 
                       groupnames=c("day"))


#********************
# plot
#********************

sex.ratio <- ggplot(df.sex, aes(x=day, y=sex.ratio)) + 
                    geom_errorbar(aes(ymin=sex.ratio-se, 
                                      ymax=sex.ratio+se), 
                                      width=.2) +
                    geom_point(shape=21, 
                               size=2, 
                               fill="white") +
                    geom_smooth(method = "lm", 
                                color = "black", 
                                size=0.5, 
                                lty=2, 
                                se = F) +
                    scale_x_continuous(name="Days", 
                                       breaks = seq(1, 11, by = 2), 
                                       limits=c(1, 11)) +
                    scale_y_continuous(name="Sex ratio (% female)", 
                                       limits=c(60, 100)) +
                    theme_pubr()

# clean plot structure
sex.plot <- sex.ratio + theme(axis.text.y   = element_text(size=14),
                              axis.text.x   = element_text(size=14),
                              axis.title.y  = element_text(size=14),
                              axis.title.x  = element_text(size=14),
                              panel.background = element_blank(),
                              panel.grid.major = element_blank(), 
                              panel.grid.minor = element_blank(),
                              axis.line = element_line(colour = "black"),
                              panel.border = element_rect(colour = "black", 
                                                          fill=NA, 
                                                          size=0.75))

# print
sex.plot


#********************
# statistics
#********************

# subset days of parasitism events
ltp3.sub <- subset(ltp.sub, day < 13)

# regression
fit <- lm(sex.ratio ~ day, data=ltp3.sub)
summary(fit)


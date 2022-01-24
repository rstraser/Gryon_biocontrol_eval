#********************
# parasitoid fecundity
#********************

# read in fecundity data
ltp <- read.csv ("GP.fecundity.csv")
surv <- read.csv ("survival.csv")



#********************
# data prep
#********************

# set "treatment" as factor variable
ltp$treatment <- as.factor(ltp$treatment)

# subset treatment "3" - feed diet x3 per week
df1 <- subset(ltp, treatment == 3)

# data_summary fecundity
df2 <- data_summary(df1, 
                    varname="GP.tot", 
                    groupnames=c("day"))

# subset days < 53
df2 <- subset(df2, day < 53)

# add zero values for fecundity to match number of days
# survived data frame
new_df <- data.frame(c(53, 55, 57, 59),
                     c(0, 0, 0, 0),
                     c(0,0,0, 0)) 
names(new_df) <- c("day", "GP.tot", "se")  
df2 <- rbind(df2, new_df)



# data_summary survival
df3 <- data_summary(surv, 
                    varname="survival", 
                    groupnames=c("day"))

# bind dataframe "df2" with "lx" column from "life table data"
newdf <- cbind(df2, df3)
colnames(newdf) <- c("day", "Gp.tot", "Gp.se", "day2", "Surv", "Surv.se")
newdf






#********************
# plot
#********************


# subset dataframe to exclude 59th day where survival was zero
newdf2 <- subset(newdf, day < 59)


# plot fecundity & longevity per day
fec_long <- newdf2 %>>% 
  ggplot() + 
  aes(x=day, y=Gp.tot) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge(.9), width=1) +
  geom_errorbar(aes(ymin=Gp.tot, ymax=Gp.tot+Gp.se), width=.3,
                position=position_dodge(.9)) +
  geom_point(aes(x = day, y = Surv * 20), 
             shape=21, 
             size=2, 
             fill="black") + 
  geom_errorbar(aes(ymin=Surv*20 - Surv.se*20, ymax=Surv*20+Surv.se*20), 
                 width=.3, 
                position=position_dodge(0))+
  geom_line(aes(x = day, y = Surv * 20)) +
  xlab("Days") + 
  ylab("Offspring / female / day") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 5, 
                                         name = "Female survival (%)"))+
  scale_x_continuous(breaks = seq(1, 57, by = 2)) +
  theme_pubr()


# clean plot structure
fec_long_plot <- fec_long + theme(axis.text.y = element_text(size=14),
                                  axis.text.x   = element_text(size=14),
                                  axis.title.y  = element_text(size=14),
                                  axis.title.x  = element_text(size=14),
                                  panel.background = element_blank(),
                                  panel.grid.major = element_blank(), 
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(colour = "black"),
                                  panel.border = element_rect(colour = "black", fill=NA, size=0.75))

# print
fec_long_plot



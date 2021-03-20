#********************
# parasitoid fecundity
#********************

# read in fecundity data
ltp <- read.csv ("GP.fecundity.csv")
life_table <- read.csv ("GP.life.tables.csv")



#********************
# data prep
#********************

# set "treatment" as factor variable
ltp$treatment <- as.factor(ltp$treatment)

# subset treatment "3" - feed diet x3 per week
df1 <- subset(ltp, treatment == 3)

# data_summary
df2 <- data_summary(df1, 
                    varname="GP.tot", 
                    groupnames=c("day"))

# subset treatment "3"
df3 <- subset(life_table, treatment == 3)

# join datasets
# indicate zero Gp emergance on days 53 & 55 to df2
newRow1 <- data.frame(day = 57, GP.tot=0, se=0) 
newRow2 <- data.frame(day = 59, GP.tot=0, se=0) 
df2 <- rbind(df2, newRow1, newRow2)

# bind dataframe "df2" with "lx" column from "life table data"
newdf2 <- cbind(df2, df3$lx)



#********************
# plot
#********************

# plot fecundity & longevity per day
fec_long <- newdf2 %>>% 
              ggplot() + 
              aes(x=day, y=GP.tot) + 
              geom_line() +
              geom_errorbar(aes(ymin=GP.tot-se, ymax=GP.tot+se), 
                                width=.75, 
                                position=position_dodge(0))+
              geom_point(shape=21, 
                         size=2, 
                         fill="white") +
              geom_point(aes(x = day, y = df3$lx * 20), 
                         shape=21, 
                         size=2, 
                         fill="black") + 
              geom_line(aes(x = day, y = df3$lx * 20)) +
              xlab("Days") + 
              ylab("Offspring / female / day") +
              scale_y_continuous(sec.axis = sec_axis(~ . * 5, 
                                 name = "Female survival (%)"))+
              scale_x_continuous(breaks = seq(1, 56, by = 5)) +
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




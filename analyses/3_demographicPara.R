#********************
# demographic parameters
#********************

# read in life table data
life_table <- read.csv ("GP.life.tables.csv")



#********************
# data prep
#********************

# subset treatment "3"
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


# calculate life table values with SD
# read in cummulative life table data 
life_table_x <- read.csv ("Tot.life.tables.csv")
life_table_x

# function for generating life table values
function.lt <- function(data){
  require(dplyr)
  lt <- data %>%
    group_by(ID) %>%
    mutate("Treat" = Treat,
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

# collate life table values for each specimen
df.sum <- lt_x %>%
          select(Treat, ID, R0, T, r, rm, lambda, Td, GRR) %>%
          summarise_each(funs(x = mean, sd = sd))
df.sum


#********************
# analyses
#********************

# calculate mean and SD
rm.sum <- summaryBy(rm_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
lambda.sum <- summaryBy(lambda_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
T.sum <- summaryBy(T_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
Td.sum <- summaryBy(Td_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
R0.sum <- summaryBy(R0_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))
GRR.sum <- summaryBy(GRR_x ~ Treat, data=df.sum, FUN=c(length,mean,sd))

# print
rm.sum
lambda.sum
T.sum
Td.sum
R0.sum
GRR.sum

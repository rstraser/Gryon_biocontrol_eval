#********************
# demographic parameters
#********************

# read in life table data
life_table_x <- read.csv ("Tot.life.tables.csv")
life_table_x


#********************
# data prep
#********************

# function for generating life table values
function.lt <- function(data){
  require(dplyr)
  lt <- data %>%
    group_by(ID) %>%
    mutate("Treat" = Treat,
           "lx*mx"=lx*mx_net,
           "x*lx*mx"=Age*lx*mx,
           "R0"=sum(lx*mx_net),
           "T"=sum(Age*lx*mx)/(sum(lx*mx)),
           "r"=log(R0)/T,
           "rm"=(log(sum(lx*mx_net)))/T,
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
          summarise_each(funs(x = mean))
df.sum




#********************
# analyses
#********************

# calculate mean and SD
rm.sum <- summaryBy(rm_x ~ Treat, data=df.sum, FUN=c(length,mean,std.error))
lambda.sum <- summaryBy(lambda_x ~ Treat, data=df.sum, FUN=c(length,mean,std.error))
T.sum <- summaryBy(T_x ~ Treat, data=df.sum, FUN=c(length,mean,std.error))
Td.sum <- summaryBy(Td_x ~ Treat, data=df.sum, FUN=c(length,mean,std.error))
R0.sum <- summaryBy(R0_x ~ Treat, data=df.sum, FUN=c(length,mean,std.error))

# print
rm.sum
lambda.sum
T.sum
Td.sum
R0.sum


#1 - Project of Commodities import and export - data form Bureau of labor statistics data

#To create TIME SERIES of data - ts()

## we used the xts and zoo packages that handle tiem series data explicitly
#install.packages("zoo")
#install.packages("xts")

#Adding the data
input_data = read.csv("EIUIR.csv")
head(input_data)
#cleaning the data
xmprice = na.omit(input_data)
str(xmprice)

#Now want to calcuate the month to month growth rate - by computing natural logarithm of price and their difference
xmprice.r = as.zoo(na.omit((diff(log(xmprice$Value))))) #compute the rates
head(xmprice.r)
#simple plot - monthly rate over the year from 2006-2012
png("Monthly2006-2012 - 1.png")
plot(xmprice.r,type = "l",col = "blue",xlab = "Date",main = "Monthly 2006-2012")
dev.off()
#Adding the month to month growth rate to table and create data frame
xmprice.r.df = data.frame(xmprice.r,Date = index(xmprice.r), Rate = xmprice.r[,1],
                          Rate.abs = abs(xmprice.r[,1])) #abs used to get convert all negative value as positive value
head(xmprice.r.df)
str(xmprice.r.df)

#building the graph by usign ggplot2 package
library(ggplot2)
#graph - 2 - line
png("Rate_Date - 2.png")
ggplot(xmprice.r.df,aes(x = Date, y = Rate))+geom_line(col = "blue")
dev.off()
#graph - 3 - bar graph of absolute price rate
png("Rate.ads_Date -3.png")
ggplot(xmprice.r.df,aes(x = Date, y = Rate.abs))+geom_bar(stat = "identity",colour = "green")
dev.off()
#graph - 4 - bar + line graph of the universal export-import ltd supply chin
png("Rate_ - 4")
ggplot(xmprice.r.df,aes(Date,Rate.abs))+ geom_bar(stat = "identity",col = "green")+ geom_line(data = xmprice.r.df,aes(Date,Rate),colour = "blue")
dev.off()
#graph - 5 - We import goods as input to our manufacturing process. want to know the odds that a very high export and import rate might occure.
png("cfd_rate - 5.png")
ggplot(xmprice.r.df,aes(Rate)) + stat_ecdf(col = "blue")
dev.off()

# Questions - 
#1. Suppose the procurement team's delegation of authority remit states: "Procurement
#may approve input invoices when there is only a 5% chance that prices will rise any
#higher than the price rate associated with that tolerance. If input prices do rise higher
#than the tolerable rate, you must get divisional approval."
#2. Plot a vertical line to indicate the maximum tolerable rate for procurement using the
#BLS EIUR data from 2000 to the present.
png("Rate_rolerablerate - 6.png")
r.tol.pct = 0.95
r.tol = quantile(xmprice.r.df$Rate, r.tol.pct)
r.tol.label = paste("Tolerable Rate = ", round(r.tol,2))
ggplot(xmprice.r.df, aes(Rate)) + stat_ecdf(colour = "blue",size = 1.5) + geom_vline(xintercept = r.tol,
      colour = "red", size = 1.5) + annotate("text", x = r.tol - 0.05, y = 0.75, label = r.tol.label, colour = "darkred")
                                                                                                           
dev.off()
                                                                                                           
#This may be a little more than we bargained for originally. We used the paste and round
#(to two, 2, decimal places) functions to make a label. We made much thicker lines (size =1.5). At 2% we drew a line with geom_vline() and annotated the line with text.
                                                                                     




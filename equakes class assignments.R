
#Home WORK
library(e1071)
e_quakes<-datasets::quakes
e_quakes
####Top 10 rows and last 10 rows
head(e_quakes,10) # calling only top 10 rows with column
tail(e_quakes,10) # calling only last 10 rows with column


######Columns
e_quakes[,c(1,2)]  # calling for all row and only 1 & 2 no column
e_quakes<-e_quakes[,-5] # deleting the column no 5
e_quakes

summary(e_quakes[]) #summary command will gives the mean, median, mean, ist quartile, 3rd quartile, min & max
e_quakes$lat
summary(e_quakes$lat[])
e_quakes
e_quakes

########################
#####################
plot(e_quakes$lat)
plot(e_quakes$lat, e_quakes$long,type="p") ### comparison of 2 columns in a data frame where "p" stands for points



plot(e_quakes)


# points and lines 
plot(e_quakes$lat, type= "l")           # l: lines plot
plot(e_quakes$lat, type= "p")           # p: points plot
plot(e_quakes$lat, type= "b")           # p: points plot


plot(e_quakes$long,main = 'e_quakes Longitude', xlab= 'Longitude of e_quakes', ylab = 'Longitude', col= 'blue')

#### Horizontal bar plot
barplot(e_quakes$long, main='e_quakes Longitude',xlab= 'Longitude of e_quakes',
        ylab = 'Longitude', col= 'blue',horiz =F  ,axes=T) ## here in axes T means i am showing the axes i.e X & Y. If we place F in place of T in axes it will not show us the axes.


###Histogram
hist(e_quakes$depth)
hist(e_quakes$depth, main = 'e_quakes Depth', xlab = 'Depth of e_quakes',col = 'blue',ylab = 'Frequency')

###Single box plot
boxplot(e_quakes$mag, main='Boxplot')
boxplot.stats(e_quakes$mag)$out  #### this command with boxplot.stats() is used for checking the outliers in the data


boxplot(e_quakes$lat , main='Boxplot')
boxplot.stats(e_quakes$lat)$out


boxplot(e_quakes$depth, main='Boxplot')
boxplot.stats(e_quakes$depth)$out 


boxplot(e_quakes$long , main='Boxplot')
boxplot.stats(e_quakes$long)$out 


#### Multiple box plots
boxplot(e_quakes[,1:4], main='Multiple')



 
#this (mfrow) means no of rows and columns, 
#this (mar) is used for margin of the grid
#this (bty) is used whether a border/boxes is to be included. (bty='o') means included & (bty='n') means not included boxes/border
#and position of the 
#this las command is used for labels whether needs to be vertical or horizontal labels(las: 1 for horizontal, las: 0 for vertical)
#bty - box around the plot

par(mfrow=c(3,3), mar=c(2,5,2,1),las=1, bty='o')  ### par() function is used to Quarry Graphical Parameters/ its means you are making the mattrices of your data sets
plot(e_quakes$lat)    
plot(e_quakes$lat, e_quakes$long)
plot(e_quakes$lat, type="l")
plot(e_quakes$lat, type="l")
plot(e_quakes$lat, type="l")
barplot(e_quakes$lat,main='e_quakes Latitude',xlab= 'Latitude of e_quakes',
        ylab = 'Latitude', col= 'orange',horiz =F  ,axes=T)
hist(e_quakes$depth)
boxplot(e_quakes$depth)
boxplot(e_quakes[,0:4], main='Multiple Box Plots')


##################variance###############################
var(e_quakes$lat)
var(e_quakes$long)
var(e_quakes$depth)
var(e_quakes$mag)

#########################################################
#########################################################



#Column Lat
#######Skewness#########################################

lat<-e_quakes$lat #storing the vale of lat column in lat.
mean(lat)         # calculating mean of lat
skewness(lat) ##Print Skewness of distribution
## the value of column late is -0.67 which is less than zero. Also the graph is said to be Negatively skewed wit the majority of data vales greater than mean. most of the values are concentrated on the right side of the graph

#Histogram of distribution
hist(e_quakes$lat,main = 'Latitude of e_quakes is -vely Skewed', xlab = 'Latitude',col = 'blue',)

#######Kurtosis#########################################
kurtosis(e_quakes$lat) #print Kurtosis of distribution

#Histogram of distribution
hist(e_quakes$lat,main = 'Latitude of e_quakes is Platykurtic', xlab = 'Latitude',col = '#ffa600',)

#Density Plot
plot(e_quakes$lat, main = 'Density plot of Latitude')
plot(e_quakes$lat, main = 'Density plot of Latitude',type = 'l')
polygon(e_quakes$lat, col="red", border="blue")
#############################################################################################################
#############################################################################################################



#Column Long
#######Skewness#########################################

long<-e_quakes$long #storing the vale of lat column in lat.
mean(long)         # calculating mean of lat
skewness(long) ##Print Skewness of distribution
## the value of column late is -1.161 which is less than zero. Also the graph is said to be Negatively skewed wit the majority of data vales greater than mean. most of the values are concentrated on the right side of the graph

#Histogram of distribution
hist(e_quakes$long,main = 'Longitude of e_quakes is -vely Skewed', xlab = 'Longitude',col = 'blue',)

#######Kurtosis#########################################
kurtosis(e_quakes$long) #print Kurtosis of distribution

#Histogram of distribution
hist(e_quakes$long,main = 'Longitude of e_quakes is Platykurtic', xlab = 'longitude',col = '#ffa600',)

#Density Plot
plot(e_quakes$long, main = 'Density plot of Longitude')
polygon(e_quakes$long, col="red", border="blue")
plot(e_quakes$long, main = 'Density plot of Longitude',type = 'l')
#############################################################################################################
#############################################################################################################



#Column Depth
#######Skewness#########################################

mean(e_quakes$depth)         # calculating mean of lat
skewness(e_quakes$depth) ##Print Skewness of distribution
## the value of column late is 0.197 which is greater than zero. Also the graph is said to be Positively skewed with the majority of data vales greater than mean. most of the values are concentrated on the left side of the graph

#Histogram of distribution
hist(e_quakes$depth,main = 'Depth of e_quakes is Poitively Skewed', xlab = 'Depth',col = 'blue')

#######Kurtosis#########################################
kurtosis(e_quakes$depth) #print Kurtosis of distribution

#Histogram of distribution
hist(e_quakes$depth,main = 'Depth of e_quakes is Platykurtic', xlab = 'Depth',col = '#ffa600',)

#Density Plot
plot(e_quakes$depth, main = 'Density plot of Depth')
polygon(e_quakes$depth, col="red", border="blue")
plot(e_quakes$depth, main = 'Density plot of Depth',type = 'l')
#############################################################################################################
#############################################################################################################




#Column Magnitude
#######Skewness#########################################

mean(e_quakes$mag)         # calculating mean of lat
skewness(e_quakes$mag) ##Print Skewness of distribution
## the value of column late is 0.767 which is greater than zero. Also the graph is said to be Positively skewed with the majority of data vales greater than mean. most of the values are concentrated on the left side of the graph

#Histogram of distribution
hist(e_quakes$mag,main = 'Magnitude of e_quakes is Positively Skewed', xlab = 'Magnitude',col = 'blue')

#######Kurtosis#########################################
kurtosis(e_quakes$mag) #print Kurtosis of distribution

#Histogram of distribution
hist(e_quakes$mag,main = 'Magnitude of e_quakes is Platykurtic', xlab = 'Magnitude',col = '#ffa600',)

#Density Plot
plot(e_quakes$mag, main = 'Density plot of Magnitude')
polygon(e_quakes$mag, col="red", border="blue")
plot(e_quakes$mag, main = 'Density plot of Magnitude',type = 'l')
##############################################################################################################
##############################################################################################################



#Column Stations
#######Skewness#########################################
sat<-e_quakes$stations
mean(e_quakes$stations)# calculating mean of lat
mean(sat)# calculating mean of lat
skewness(sat) ##Print Skewness of distribution
## the value of column late is 1.614 which is greater than zero. Also the graph is said to be Positively skewed with the majority of data vales greater than mean. most of the values are concentrated on the left side of the graph

#Histogram of distribution
hist(sat,main = 'Stations of e_quakes is Positively Skewed', xlab = 'Stations',col = '#58508d')

#######Kurtosis#########################################
kurtosis(sat) #print Kurtosis of distribution

#Histogram of distribution
hist(sat,main = 'Stations of e_quakes is Platykurtic', xlab = 'Stations',col = '#ffa600',)

#Density Plot
plot(sat, main = 'Density plot of Stations')
polygon(sat, col="red", border="blue")
plot(sat, main = 'Density plot of Magnitude',type = 'l')

 
     
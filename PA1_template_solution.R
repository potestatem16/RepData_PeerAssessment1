library(ggplot2)
library(data.table)
library(ggthemes)


####################
#______Loading Data
"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" -> url1

setwd("C:/Users/aleja/Documents/Cursos/Coursera R pratices/RepData_PeerAssessment1")

ifelse(!dir.exists(file.path(getwd(), "Data")), 
       dir.create(file.path(getwd(), "Data")), FALSE)


download.file(url = url1, file.path("./Data","Assesment1_data.zip" ),
              method = "curl")


unzip("Assesment1_data.zip", exdir = "./Data")
list.files(path = "./Data")


fread("./Data/activity.csv")-> df

head(df)


###########################
#___________What is mean total number of steps taken per day?

with(df, aggregate(steps, by=list(date), FUN = sum, na.rm=T))->total_steps
total_steps$Group.1<-as.Date(total_steps$Group.1,"%Y-%m-%d");head(total_steps)
names(total_steps)<-c("Date","N_steps")

ggplot(total_steps, aes(x = N_steps)) +
     geom_histogram(fill = "red", binwidth = 1000) +
     labs(title = "Steps by Date", x = "Steps", y = "Frequency")+
   theme_economist_white()

cat("Mean of Steps", mean(total_steps$N_steps, na.rm = T), sep = "\n")
cat("Median of Steps", median(total_steps$N_steps, na.rm = T), sep = "\n")


#########################
#____What is the average daily activity pattern?

df[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), 
   by = .(interval)]-> activity_pattern

ggplot(activity_pattern, aes(x = interval , y = steps)) ->g1

g1 + 
     geom_line(color="red", size=1) + 
     labs(title = "Average Daily Activity Pattern", x = "Interval", y = "Average Steps per day")+
     theme_igray()

activity_pattern[steps == max(steps), .(maximun_interval = interval)]



#########################
#__________Imputing missing values


     #Calculate and report the total number of missing values in the dataset 
     #(i.e. the total number of rows with 𝙽𝙰s)

sapply(df, function(x) sum(is.na(x)))

     #Devise a strategy for filling in all of the missing values in the dataset. 
     #The strategy does not need to be sophisticated. For example, 
     #you could use the mean/median for that day, or the mean for 
     #that 5-minute interval, etc.

df[,lapply(.SD,function(x){ifelse(is.na(x),median(df$steps, na.rm = T),x)})]
     #replace missiong values with the median



     #Create a new dataset that is equal to the original dataset but 
     #with the missing data filled in.
df[,lapply(.SD,function(x){ifelse(is.na(x),median(df$steps, na.rm = T),x)})]->df2
sapply(df2, function(x) sum(is.na(x)))


     #Make a histogram of the total number of steps taken each day 
     #and Calculate and report the mean and median total number of steps 
     #taken per day. 
     #Do these values differ from the estimates from the first part of the assignment?
     #What is the impact of imputing missing data on the estimates of 
     #the total daily number of steps?

with(df2, aggregate(steps, by=list(date), FUN = sum, na.rm=T))->total_steps2
total_steps2$Group.1<-as.Date(total_steps2$Group.1,"%Y-%m-%d");head(total_steps2)
names(total_steps2)<-c("Date","N_steps")

ggplot(total_steps2, aes(x = N_steps)) +
     geom_histogram(fill = "red", binwidth = 1000) +
     labs(title = "Steps by Date", x = "Steps", y = "Frequency")+
   theme_gdocs()

cat("Mean of Steps without NA", mean(total_steps2$N_steps, na.rm = T), sep = "\n")
cat("Median of Steps without NA", median(total_steps2$N_steps, na.rm = T), sep = "\n")



################################
#___Are there differences in activity patterns between weekdays and weekends?


Sys.setlocale("LC_TIME", "C")

df[, date := as.POSIXct(date, format = "%Y-%m-%d")]
df[, `Day of Week`:= weekdays(x = date)]
df[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", 
         x = `Day of Week`), "weekday/weekend"] <- "weekday"
df[grepl(pattern = "Saturday|Sunday", 
         x = `Day of Week`), "weekday/weekend"] <- "weekend"
df[, `weekday or weekend` := as.factor(`weekday/weekend`)]
head(df, 10)

df[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]->df[is.na(steps), "steps"]

df[, c(lapply(.SD, mean, na.rm = TRUE)), 
           .SDcols = c("steps"), 
           by = .(interval, `weekday/weekend`)] ->activity_pattern_diff 

ggplot(activity_pattern_diff , aes(x = interval , y = steps, color=`weekday/weekend`))->g2
g2 + 
     geom_line() + 
     labs(title = "Average for Daily Steps, by Week Type", x = "Intervals", y = "Number of Steps") + 
     facet_wrap(~`weekday/weekend` , ncol = 1, nrow=2)+
     theme_excel_new()

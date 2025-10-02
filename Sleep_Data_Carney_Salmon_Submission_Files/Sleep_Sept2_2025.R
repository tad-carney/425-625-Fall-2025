#' ---
#' title: "Sleep Study"
#' date: "`r Sys.Date()`"
#' author: "Statistical Case Studies"
#' output: pdf_document
#' ---
#' 
#' 
#' # Sleep Study Graphical Exploration Challenge
#'
#' - See `SleepStudy.html` for a description of the research problem.
#' - See `SleepData/` for 6 days of data, each day including two data
#'   files plus a README file.  There is a zip file of the data, too.
#'

days <- dir("SleepData")

#
# Read in one day of data for starters...
#

thisday <- days[2]
files <- dir(file.path("SleepData", thisday),
             full.names = TRUE, pattern = "^[^R]")

x <- read.table(files[1], as.is = TRUE, skip = 1, header = FALSE)
names(x) <- "angle"

y <- read.csv(files[2], as.is = TRUE)
names(y)[2] <- "angle"

#'
#' ## Challenge 1
#' 
#' Create a list of length 6 (there are 6 days of data).
#' Each element of the list should be a list of two data frames.
#' Use suitable names for both the main list and the internal lists.
#' This should (might?) save you time down the road...
#'
sleepdata <- vector("list", length(days))
names(sleepdata) <- days
for(d in days) {
  files <- dir(file.path("SleepData", d),
               full.names = TRUE, pattern = "^[^R]")
  x <- read.table(files[1], as.is = TRUE, skip = 1, header = FALSE)
  names(x) <- "angle"
  y <- read.csv(files[2], as.is = TRUE)
  names(y)[2] <- "angle"
  sleepdata[[d]] <- list(Embletta = x, SomnoPose = y)
}


#'
#' ## Challenge 2
#' 
#' Try to do a basic graphical exploration of the data
#' from any one of the days.  For this to make any sense at all, you'll
#' need to read (carefully) `SleepStudy.html` and perhaps look at the
#' contents of the README files (in the data directories).
#'

# Taking the day of November 19 as an example
sleepdayNov19 <- sleepdata[[4]]

Embletta_data <- sleepdayNov19$Embletta
max(Embletta_data$angle)
SomnoPose_data <- sleepdayNov19$SomnoPose



# From Claude.AI
library(lubridate)
library(ggplot2)
library(dplyr)

#from Claude.ai
# 1. Fix the Embletta data timestamps
# Using 10 measurements per second starting at 10PM on Nov 19, 2011, got this from Claude.AI
generate_datetime_timestamps <- function(start_datetime_str, measurements_per_second, total_measurements) {
  start_datetime <- ymd_hms(start_datetime_str)
  increment <- 1 / measurements_per_second
  timestamps <- start_datetime + seconds(0:(total_measurements-1) * increment)
  return(timestamps)
}

# Apply timestamps to Embletta data (starting at 10PM on Nov 19, 2011)
start_datetime <- "2011-11-19 22:00:00"
Embletta_data$datetime <- generate_datetime_timestamps(start_datetime, 10, nrow(Embletta_data))

SomnoPose_data <- SomnoPose_data %>%
  mutate(Date = case_when(
    Time_of_day >= "23:24:00" & Time_of_day <= "23:59:59" ~ as.Date("2011-11-19"),
    TRUE ~ as.Date("2011-11-20")
  ))


SomnoPose_data$datetime <- as.POSIXct(
  paste(SomnoPose_data$Date, SomnoPose_data$Time_of_day), 
  format="%Y-%m-%d %H:%M:%OS", tz="UTC")


Embletta_data <- Embletta_data %>%
  filter(angle >= -180, angle <= 180) 


# Function for plotting overnight data, we had Claude.AI help us in this step
create_overnight_plot <- function(Embletta_data, SomnoPose_data, date_str) {
  ggplot() +
    # Add Embletta data
    geom_line(data = Embletta_data, 
              aes(x = datetime, y = angle, color = "Embletta"),
              alpha = 0.7) +
    
    # Add SomnoPose data
    geom_line(data = SomnoPose_data, 
              aes(x = datetime, y = angle, color = "SomnoPose"),
              alpha = 0.7) +
    
    # Customize colors and labels
    scale_color_manual(values = c("Embletta" = "blue", "SomnoPose" = "red"),
                       name = "Device") +
    
    # Add appropriate labels with date information
    labs(title = paste0("Overnight Angle Measurements", date_str),
         subtitle = "Embletta (10 measurements/second) vs. SomnoPose (1 measurement/2 seconds)",
         x = "Time",
         y = "Angle") +
    
    # Use a proper datetime scale for x-axis
    scale_x_datetime(date_labels = "%b %d %H:%M", date_breaks = "2 hour") +
    
    # Improve appearance
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_line(color = "gray90"),
      panel.grid.major = element_line(color = "gray85")
    )
}

overnight_plot1 <- create_overnight_plot(Embletta_data, SomnoPose_data, " (Nov 19-20, 2011)")
print(overnight_plot1)


## OTHER DATES

# Taking the day of November 15 as an example
sleepdayNov15 <- sleepdata[[1]]

Embletta_data <- sleepdayNov15$Embletta
#max(Embletta_data$angle)
SomnoPose_data <- sleepdayNov15$SomnoPose

#dim(Embletta_data)

#dim(SomnoPose_data)

#which(abs(Embletta_data$angle[330000:length(Embletta_data$angle)]) > 180)

#Embletta_data$angle[330000:length(Embletta_data$angle)]
# Row 322127+20000 is where it peaks at 300 degrees - after looking at the plot of the angles,
# we decided this would be a good place to start

Embletta_data <- Embletta_data[1:342127, ]
Embletta_data <- as.data.frame(Embletta_data)

names(Embletta_data)[1] <- "angle"

start_datetime <- "2011-11-15 22:00:00"
Embletta_data$datetime <- generate_datetime_timestamps(start_datetime, 10, nrow(Embletta_data))

SomnoPose_data <- SomnoPose_data %>%
  mutate(Date = case_when(
    Time_of_day >= "22:24:00" & Time_of_day <= "23:59:59" ~ as.Date("2011-11-15"),
    TRUE ~ as.Date("2011-11-16")
  ))

SomnoPose_data$datetime <- as.POSIXct(
  paste(SomnoPose_data$Date, SomnoPose_data$Time_of_day), 
  format="%Y-%m-%d %H:%M:%OS", tz="UTC")

Embletta_data <- Embletta_data %>%
  filter(angle >= -180, angle <= 180)

overnight_plot2 <- create_overnight_plot(Embletta_data, SomnoPose_data, " (Nov 15-16, 2011)")
print(overnight_plot2)



### November 23

sleepdayNov23 <- sleepdata[[5]]
Embletta_data <- sleepdayNov23$Embletta
#max(Embletta_data$angle)
SomnoPose_data <- sleepdayNov23$SomnoPose

#iTouch recording:

#Data start time: Nov 23, 2011 12:32:00 AM Eastern Standard Time
#Data end time:  Nov 23, 2011 7:32:51 AM Eastern Standard Time


#which(Embletta_data$angle[1:length(Embletta_data$angle)] < -100)


#plot(Embletta_data$angle[25972:length(Embletta_data$angle)], type='l')

#We had initially thought the misalignment of our plots was due to a timing issue, but 
# upone futher investigation, we found that the Embletta data was simply off by a constant factor of -1.
# this suggests that the two devices were on opposite sides of the body (e.g., front vs back).

Embletta_data$angle <- -1*(Embletta_data$angle)

#Embletta_data <- Embletta_data[20000:nrow(Embletta_data),  ]
#Embletta_data <- as.data.frame(Embletta_data)
#names(Embletta_data)[1] <- "angle"

Embletta_data <- Embletta_data %>%
  filter(angle >= -180, angle <= 180)

start_datetime <- "2011-11-23 00:32:00"
Embletta_data$datetime <- generate_datetime_timestamps(start_datetime, 10, nrow(Embletta_data))

SomnoPose_data <- SomnoPose_data %>%
  mutate(Date = as.Date("2011-11-23")
  )

SomnoPose_data$datetime <- as.POSIXct(
  paste(SomnoPose_data$Date, SomnoPose_data$Time_of_day), 
  format="%Y-%m-%d %H:%M:%OS", tz="UTC")


overnight_plot3 <- create_overnight_plot(Embletta_data, SomnoPose_data, " (Nov 23, 2011)")
print(overnight_plot3)
#ggplot() +
 # geom_line(data = Embletta_data, aes(x = 1:nrow(Embletta_data), y = angle))






## November 24

#Data start time: Nov 24, 2011 12:38:19 AM Eastern Standard Time
#Data end time:  Nov 24, 2011 7:32:58 AM Eastern Standard Time

sleepdayNov24 <- sleepdata[[6]]
Embletta_data <- sleepdayNov24$Embletta
#max(Embletta_data$angle)
SomnoPose_data <- sleepdayNov24$SomnoPose

# Again, we looked into if there was the same positioning issue as Nov 23, and it seems like there is.

plot(-1*(Embletta_data$angle), type='l', main = "Embletta Data Nov 24", xlab = "Index", ylab = "Angle")
plot(SomnoPose_data$angle, type='l', main = "SomnoPose Data Nov 24", xlab = "Index", ylab = "Angle")

# correcting of the issue so that we can visually compare the data
Embletta_data$angle <- -1*(Embletta_data$angle)

Embletta_data <- Embletta_data %>%
  filter(angle >= -180, angle <= 180)

# We them had to do some horizontal adjustments, in which we tried to match large movements in both datasets

which(Embletta_data$angle[1:length(Embletta_data$angle)] > 70)

Embletta_data <- Embletta_data[38153:nrow(Embletta_data),  ]

Embletta_data <- as.data.frame(Embletta_data)
names(Embletta_data)[1] <- "angle"

start_datetime <- "2011-11-24 00:38:19"
Embletta_data$datetime <- generate_datetime_timestamps(start_datetime, 10, nrow(Embletta_data))

SomnoPose_data <- SomnoPose_data %>%
  mutate(Date = as.Date("2011-11-24")
  )

SomnoPose_data$datetime <- as.POSIXct(
  paste(SomnoPose_data$Date, SomnoPose_data$Time_of_day), 
  format="%Y-%m-%d %H:%M:%OS", tz="UTC")

overnight_plot4 <- create_overnight_plot(Embletta_data, SomnoPose_data, " (Nov 24, 2011)")
print(overnight_plot4)



# November 17

sleepdayNov17 <- sleepdata[[2]]
Embletta_data <- sleepdayNov17$Embletta
#max(Embletta_data$angle)
SomnoPose_data <- sleepdayNov17$SomnoPose

start_datetime <- "2011-11-17 22:00:00"
Embletta_data$datetime <- generate_datetime_timestamps(start_datetime, 10, nrow(Embletta_data))

SomnoPose_data <- SomnoPose_data %>%
  mutate(Date = case_when(
    Time_of_day >= "22:28:00" & Time_of_day <= "23:59:59" ~ as.Date("2011-11-17"),
    TRUE ~ as.Date("2011-11-18")
  ))

SomnoPose_data$datetime <- as.POSIXct(
  paste(SomnoPose_data$Date, SomnoPose_data$Time_of_day), 
  format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#which(Embletta_data$datetime >= as.POSIXct('2011-11-18 02:19:00'))
#tail(Embletta_data$datetime)

#aligning the data on overlapping time frames

Embletta_data <- Embletta_data[Embletta_data$datetime >= as.POSIXct("2011-11-17 22:28:29", tz="UTC") & 
                                 Embletta_data$datetime <= as.POSIXct('2011-11-18 02:19:00', tz="UTC"), ]

Embletta_data <- Embletta_data %>%
  filter(angle >= -180, angle <= 180)

overnight_plot5 <- create_overnight_plot(Embletta_data, SomnoPose_data, " (Nov 17-8, 2011)")
print(overnight_plot5)



# November 18

sleepdayNov18 <- sleepdata[[3]]
Embletta_data <- sleepdayNov18$Embletta
#max(Embletta_data$angle)
SomnoPose_data <- sleepdayNov18$SomnoPose

start_datetime <- "2011-11-18 22:00:00"
Embletta_data$datetime <- generate_datetime_timestamps(start_datetime, 10, nrow(Embletta_data))



SomnoPose_data <- SomnoPose_data %>%
  mutate(Date = as.Date("2011-11-19")) |>
  filter(Time_of_day < "07:00:00")

SomnoPose_data$datetime <- as.POSIXct(
  paste(SomnoPose_data$Date, SomnoPose_data$Time_of_day), 
  format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#which(Embletta_data$datetime >= as.POSIXct('2011-11-18 02:19:00'))
#tail(Embletta_data$datetime)

Embletta_data <- Embletta_data[Embletta_data$datetime >= as.POSIXct("2011-11-19 00:41:56", tz="UTC"), ]

Embletta_data <- Embletta_data %>%
  filter(angle >= -180, angle <= 180)

overnight_plot6 <- create_overnight_plot(Embletta_data, SomnoPose_data, " (Nov 18-9, 2011)")
print(overnight_plot6)

# Setting up the data so we can compare the two datasets statistically
# since there are 10 measurements for every second in the Embletta data and 
# one measurement every 2 seconds in the SomnoPose data, we will average
# over each 20 measurements in the Embletta data to get a comparable dataset

group_size <- 20
n_groups <- floor(nrow(Embletta_data) / group_size)

# Calculate averages for each group
embletta_averaged <- data.frame(
  angle = sapply(1:n_groups, function(i) {
    start_idx <- (i-1) * group_size + 1
    end_idx <- i * group_size
    mean(Embletta_data$angle[start_idx:end_idx])
  })
)

# there was an error in dimension, so we just looked into the first 11235 rows
cor.test(embletta_averaged$angle, SomnoPose_data$angle[1:11235], use = "complete.obs")


#' \clearpage
#'
#' ## ULTIMATE GOAL
#' 
#' Ideally, produce a 6-page-plus PDF file (maybe more if it includes the
#' processed report) that includes one graphic per day
#' that could be used to help the researchers visualize the study results
#' and decide how to proceed with future research.  Be as professional as
#' possible with the graphics and the short report, where
#' you should discuss and try to answer the questions posed by the
#' researcher in `SleepStudy.html`.  Yes, this could be a very short report
#' (though plots and explorations consume quite a bit of space)!
#'
#' Guidelines:
#'
#' - You may use any graphical tools or packages that you want, ___BUT___
#'   if you use non-base R or non-`ggplot` graphics or some package to assist with the
#'   processing, your code should be particularly beautiful and well-documented
#'   to the extent that we might use it as a learning example to be shared
#'   with the whole class!  It should also answer the question, "Why do you think
#'   this is better than using base R or `ggplot`?"  
#'   The answer is hopefully more interesting than, "I don't know how to do this in base R."  
#'   So, you don't have to receive permission -- the answer is yes **as long
#'   as** you agree to the guideline above.
#'
#' - Eventually, your group should submit your script containing all your work;
#'   team members should be clearly listed at the top.  Only the group
#'   leader should submit this R script and an accompanying PDF file (of either
#'   6 plots or a processed report, whatever you want).  You should each come
#'   to the "extra sessions" with your group script
#'   and you should be comfortable with everything in the script in case
#'   I want to ask questions.  Saying, "Oh, the group leader did that and I'm
#'   not sure!" isn't ideal, needless to say.
#'   
#' - The PDF should not identify your group or your group members, so
#'   student assessments of submissions will be anonymous.
#'
#'
#' # How?
#' 
#' Starting Tuesday, September 2: You should form groups of 3 if possible. There may be one or two groups of 2. 
#'
#' This will continue on Thursday, September 4 (maybe along with some other
#' things during class time), with a final assignment submission (group) due
#' before class on Tuesday, September 9.
#'

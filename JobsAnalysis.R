# ---------------------------
# HR Job Requisition Analysis
#       October 23, 2015
# ---------------------------

## Load Packages
library(ggplot2)
library(ggthemes)
library(lubridate)

## Read in Data
dat <- read.csv('http://api.louisvilleky.gov/api/File/DownloadFile?fileName=Requisition_Log.csv',
                header = TRUE)

# Build list of what each column is
namesDF <- names(dat)

# Fixing stupid difference between number of columns of data  and number of columns of names..
names(dat) <- namesDF[-1]

# Calculate time from slot vacated to start date for new hire
dat$Duration <- dat$Duration <- as.numeric(ymd(substr(dat$START.DATE, 0, 10)) - ymd(substr(dat$DATE.SLOT.VACATED, 0, 10)))/(60*60*24)

# Build 'isUnion' variable
dat$isUnion <- ifelse(dat$UNION == 'NU', 
                      'nonUNION', 
                      'UNION')

# Plot duration of hire by union vs non-union
ggplot(subset(dat, Duration > 0), aes(x = SLOT.., y = Duration)) +
  geom_point() +
  geom_hline(yintercept = mean(na.omit(subset(dat, isUnion == 'UNION' & Duration > 0)$Duration)), colour = 'Blue') +
  geom_hline(yintercept = mean(na.omit(subset(dat, isUnion == 'nonUNION' & Duration > 0)$Duration)), colour = 'Red') +
  facet_grid(~isUnion) +
  theme_fivethirtyeight()

# Build a frequency table by union/non-union
UnionTable <- table()

# Plot duration of hire by recruiter
ggplot(subset(dat, Duration > 0), aes(x = DATE.REC.D, y = Duration)) +
  geom_point(size = 2.75, alpha = 0.25, colour = '#50b8c5') +
  facet_grid(~RECRUITER) +
  theme_economist_white() +
  theme(
    panel.background = element_rect(colour = 'Blue'), 
    plot.background = element_blank(), 
    legend.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
    )

# Build a frequency table by recruiter
RecruiterTable <- table(dat$RECRUITER) # JA is apparently a rockstar recruiter

# Plot duration of hire by type of hire
ggplot(subset(dat, Duration > 0), aes(x = DATE.REC.D, y = Duration)) +
  geom_point(size = 2.75, alpha = 0.25, colour = '#50b8c5') +
  facet_grid( ~ TYPE.OF.HIRE) +
  theme(
    panel.background = element_rect(colour = 'Grey'), 
    plot.background = element_blank(), 
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(colour = 'white')
  )

# Build a frequency table by type of hire
TypeOfHireTable <- table(dat$TYPE.OF.HIRE)

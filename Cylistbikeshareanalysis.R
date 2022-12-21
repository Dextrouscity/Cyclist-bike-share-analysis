### Installing packages
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(ggplot2)
library(mapview)
library(rmarkdown)
rm(list = ls())
### Importing CSV files
dir("CSV",full.names = TRUE)
### Data Source From https://divvy-tripdata.s3.amazonaws.com/index.html
df1 <- read.csv("CSV/202110-divvy-tripdata.csv")
df2 <- read.csv("CSV/202111-divvy-tripdata.csv")
df3 <- read.csv("CSV/202112-divvy-tripdata.csv")
df4 <- read.csv("CSV/202201-divvy-tripdata.csv")
df5 <- read.csv("CSV/202202-divvy-tripdata.csv")
df6 <- read.csv("CSV/202203-divvy-tripdata.csv")
df7 <- read.csv("CSV/202204-divvy-tripdata.csv")
df8 <- read.csv("CSV/202205-divvy-tripdata.csv")
df9 <- read.csv("CSV/202206-divvy-tripdata.csv")
df10 <- read.csv("CSV/202207-divvy-tripdata.csv")
df11 <- read.csv("CSV/202208-divvy-tripdata.csv")
df12 <- read.csv("CSV/202209-divvy-publictripdata.csv")
### Combining all 12 data.frames into one data.frame
tripdata <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
dim(tripdata)
### Removing empty rows and columns
tripdata <- janitor::remove_empty(
  tripdata,which = c(
    "rows"
    )
  )
tripdata <- janitor::remove_empty(
  tripdata,which = c(
    "cols"
  )
)
### Converting the date/time stamp to an actual date/time stamp
tripdata$started_at <- lubridate::mdy_hm(
  tripdata$started_at
)
tripdata$ended_at <-lubridate::mdy_hm(
  tripdata$ended_at
)
### Creating an hour column
tripdata$start_hour <- lubridate::hour(
  tripdata$started_at
)
tripdata$end_hour <- lubridate::hour(
  tripdata$ended_at
)
### Creating a date column
tripdata$start_date <- lubridate::as_date(
  tripdata$started_at
)
### Creating a column for months
tripdata$ride_month <- lubridate::month(
  tripdata$started_at,
  label = TRUE,
  abbr = FALSE
)
### Creating a column for weekdays
tripdata$weekdays <- lubridate::wday(
    tripdata$started_at,
    label = TRUE,
    abbr = FALSE
)
### Creating a column for days of the year
tripdata$day <- lubridate::day(
  tripdata$started_at
)
### Creating a column for weeks of the year
tripdata$weeks <- strftime(
  tripdata$started_at,
  format = "%V"
)
### Creating a column for trip length
tripdata$trip_duration <- difftime(
  tripdata$ended_at,
  tripdata$started_at,
  units = "mins"
)
### Checking for negative values
tripdata$trip_duration[
  (
    tripdata$trip_duration < 0
  )
]
### Creating a new data frame with positive trip duration
tripdata_positive <- tripdata[
  !(
    tripdata$trip_duration <= 0
    ),
]
### Formatting Biker type and Rider type
tripdata$rideable_type <- as.factor(
  tripdata$rideable_type
)
tripdata$member_casual <- as.factor(
  tripdata$member_casual
)
### Renaming columns
tripdata <- rename(
  tripdata,
  bike_types = rideable_type,
  user_type = member_casual,
  month = ride_month
)
### Recoding Bike types column
tripdata$bike_types <- recode(
  tripdata$bike_types,
  "classic_bike" = "classic",
  "electric_bike" = "electric",
  "docked_bike" = "docked"
)
### Checking for columns with NA values
colSums(
  is.na(
    tripdata
  )
)
### Creating a new data frame 
tripdata_time <- tripdata %>% 
  drop_na(
    end_lat,
    end_lng
  ) %>% 
  select(
    ride_id,
    bike_types,
    start_hour,
    weekdays,
    month,
    day,
    weeks,
    trip_duration,
    user_type
  )
### Creating a new data frame 
tripdata_location <- select(
  tripdata_positive,
  ride_id,
  start_station_name,
  end_station_name,
  start_lat,
  start_lng,
  end_lat,
  end_lng,
  user_type,
  trip_duration
)
### Creating a new data frame
tripdata_hour <- tripdata_time %>% 
  group_by(
    user_type,
    start_hour
  ) %>% 
  summarize(
    number_of_trips = n(),
    average_duration = mean(
      trip_duration
    ),
    total_duration = sum(
      trip_duration
    )
  )
### Data summary and structure
str(
  tripdata_positive
)
glimpse(
  tripdata_positive
)
summary(
  tripdata_positive
)
head(
  tripdata_positive
)
### Checking for duplicate rows
distinct(
  tripdata_positive
)
### plotting number of trips by hour 
ggplot(
  tripdata_hour
) +
  geom_col(
    mapping = aes(
      y = number_of_trips,
      x = start_hour,
      fill = user_type
    ),
    position = "dodge"
  ) +
  theme_classic(
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of Rides by Hour",
    caption = "Data source by Motivate International Inc"
  ) +
  ylab(
    "Number of Rides"
  ) +
  xlab(
    "Hour of the day"
  )
### Plotting number of rides by month
tripdata_month <- tripdata_time %>% 
  group_by(
    user_type,
    month
  ) %>% 
  summarize(
    number_of_trips = n()
  )
ggplot(
  tripdata_month
) +
  geom_col(
    mapping = aes(
      y = number_of_trips,
      x = month,
      fill = user_type
    ),
    position = "dodge"
  ) +
  theme_classic(
  ) +
  theme(
    axis.text.x = element_text(
      angle = 50,
      hjust = 1
    )
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of Rides by Months",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Months"
  ) +
  ylab(
    "Number of Rides"
  )
### Plotting Number of rides by weekdays and weeks of the year
tripdata_week <- tripdata_time %>% 
  group_by(
    user_type,
    weekdays,
    weeks
  ) %>% 
  summarize(
    number_of_rides = n()
  )
ggplot(
  tripdata_week
) +
  geom_col(
    mapping = aes(
      y = number_of_rides,
      x = weekdays,
      fill = user_type
    ),
    position = "dodge"
  ) +
  theme_classic(
  ) +
  theme(
    axis.text.x = element_text(
      angle = 50,
      hjust = 1
    )
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of Rides by Weekdays",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Weekdays"
  ) +
  ylab(
    "Number of Rides"
  )
ggplot(
  tripdata_week
) +
  geom_col(
    mapping = aes(
      y = number_of_rides,
      x = weeks,
      fill = user_type
    ),
    position = "dodge"
  ) +
  theme_classic(
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 0
    )
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of Rides by Weeks of the Year",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Weeks of the Year"
  ) +
  ylab(
    "Number of Rides"
  )
### Plotting of number of trips by days of the year 
tripdata_day <- tripdata_time %>% 
  group_by(
    user_type,
    day
  ) %>% 
  summarize(
    number_of_trips = n()
  )
ggplot(
  tripdata_day
) +
  geom_col(
    mapping = aes(
      y = number_of_trips,
      x = day,
      fill = user_type
    ),
    position = "dodge"
  ) +
  facet_wrap(
    ~user_type
  ) +
  theme_classic(
  ) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 1,
      color = "purple"
    ),
    axis.text.y = element_text(
      color = "purple"
    )
  ) +
  labs(
    title = "Number of trips by Days of the year",
    caption = "data source by Motivate International Inc"
  ) +
  xlab(
    "Days of the Year"
  ) +
  ylab(
    "Number of trips"
  )
### Plotting of number of trips by bike type
tripdata_biketypes <- tripdata %>% 
  group_by(
    bike_types,
    user_type
  ) %>% 
  summarize(
    number_of_trips = n(),
    average_trip_duration = mean(
      trip_duration
    ),
    total_trip_duration = sum(
      trip_duration
    )
  )
ggplot(
  tripdata_biketypes
) +
  geom_col(
    mapping = aes(
      x = average_trip_duration,
      y = bike_types,
      fill = user_type
    ),
    position = "dodge"
  ) +
  theme_classic(
  ) +
  scale_x_continuous(
    labels = comma
  ) +
  labs(
    title = "Average Trip Duration by Bike Types",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Average Trip Duration"
  ) +
  ylab(
    "Bike Type"
  )
ggplot(
  tripdata_biketypes
) +
  geom_col(
    mapping = aes(
      x = number_of_trips,
      y = bike_types,
      fill = user_type
    ),
    position = "dodge"
  ) +
  theme_classic(
  ) +
  scale_x_continuous(
    labels = comma
  ) +
  labs(
    title = "Number of trips by Bike Types",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Number of Trips"
  ) +
  ylab(
    "Bike Type"
  )
ggplot(
  tripdata_biketypes
) +
  geom_col(
    mapping = aes(
      x = total_trip_duration,
      y = bike_types,
      fill = user_type
    ),
    position = "dodge"
  ) +
  theme_classic(
  ) +
  scale_x_continuous(
    labels = comma
  ) +
  labs(
    title = "Total trip duration by Bike Types",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Total trip duration"
  ) +
  ylab(
    "Bike Type"
  )
### Plotting for most popular start station
colSums(
  is.na(
    tripdata_positive
  )
)
popular_start_stations <- tripdata_positive %>% 
  group_by(
    start_lat,
    start_lng,
    start_station_name,
    user_type
  ) %>% 
  drop_na(
    start_station_name
  ) %>% 
  summarize(
    number_of_trips = n()
  ) %>% 
  arrange(
    -number_of_trips
  )
ggplot(
  popular_start_stations[1:10,]
) +
  geom_col(
    mapping = aes(
      x = number_of_trips,
      y = start_station_name,
      fill = user_type
    ),
    position = "dodge"
  ) +
  scale_x_continuous(
    labels = comma
  ) +
  theme_classic(
  ) +
    theme(
      axis.text.x = element_text(
        color = "purple"
      ),
      axis.text.y = element_text(
        color = "purple"
      )
    ) +
  labs(
    title = "Must popular start station",
    subtitle = "Top 10 start stations",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Number of trips"
  ) +
  ylab(
    "Start stations"
  )
### Plotting for the most popular end stations
popular_end_stations <- tripdata_positive %>% 
  group_by(
    user_type,
    end_lat,
    end_lng,
    end_station_name
  ) %>% 
  summarize(
    number_of_trips = n()
  ) %>% 
  arrange(
    -number_of_trips
  ) %>% 
  drop_na(
    end_station_name
  )
ggplot(
  popular_end_stations[1:10,]
) +
  geom_col(
    mapping = aes(
      x = number_of_trips,
      y = end_station_name,
      fill = user_type
    ),
    position = "dodge"
  ) +
  scale_x_continuous(
    labels = comma
  ) +
  theme_classic(
  ) +
  theme(
    axis.text.x = element_text(
      color = "purple"
    ),
    axis.text.y = element_text(
      color = "purple"
    )
  ) +
  labs(
    title = "Must popular end station",
    subtitle = "Top 10 end stations",
    caption = "Data source by Motivate International Inc"
  ) +
  xlab(
    "Number of trips"
  ) +
  ylab(
    "End stations"
  )
mapview(
  popular_start_stations[
    1:10,
  ],
  xcol = "start_lng",
  ycol = "start_lat",
  alpha = 0.5,
  crs = 4269,
  grid = FALSE,
  legend = TRUE,
  cex = "number_of_trips",
  layer.name = "Most popular start stations"
)
mapview(
  popular_end_stations[
    1:10,
  ],
  xcol = "end_lng",
  ycol = "end_lat",
  alpha = 0.5,
  crs = 4269,
  grid = FALSE,
  legend = TRUE,
  cex = "number_of_trips",
  layer.name = "Most popular end stations"
)
### Exporting CSV files for further analysis
write.csv(
  popular_start_stations,
  file = "popular_start_station.csv"
)
write.csv(
  popular_end_stations,
  file = "popular_end_stations.csv"
)
write.csv(
  tripdata_time,
  file = "tripdata_time.csv"
)
write.csv(
  tripdata_location,
  file = "tripdata_location.csv"
)
write.csv(
  tripdata,
  file = "tripdata.csv"
)
write.csv(
  tripdata_biketypes,
  file = "tripdata_biketypes.csv"
)
write.csv(
  tripdata_day,
  file = "tripdata_day.csv"
)
write.csv(
  tripdata_hour,
  file = "tripdata_hour.csv"
)
write.csv(
  tripdata_month,
  file = "tripdata_month.csv"
)
write.csv(
  tripdata_week,
  file = "tripdata_week.csv"
)

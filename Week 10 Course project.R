IG <- read.csv("instagram_usage_lifestyle.csv")


head(IG)
summary(IG)


mean(IG$daily_active_minutes_instagram, na.rm = TRUE)
median(IG$daily_active_minutes_instagram, na.rm = TRUE)
sd(IG$daily_active_minutes_instagram, na.rm = TRUE)
cor(IG$daily_active_minutes_instagram, IG$sleep_hours_per_night, use = "complete.obs")

hist(IG$daily_active_minutes_instagram,
     main = "Distribution of Instagram Usage",
     xlab = "Minutes per Day",
     col = "lightblue")

# This histogram shows how Instagram usage is distributed among users.
# Most users appear to spend either zero or 200 minutes per day,
# indicating that pretty either moderate/heavy or no usage is most common.

boxplot(IG$daily_active_minutes_instagram,
        main = "Instagram Usage Spread",
        col = "lightgreen")

# The boxplot displays the spread of Instagram usage and highlights any outliers.
# Some users spend significantly more time than others,
# suggesting extreme or heavy usage in certain cases.


plot(IG$daily_active_minutes_instagram, IG$sleep_hours_per_night,
     main = "Instagram Usage vs Sleep",
     xlab = "Usage (minutes)",
     ylab = "Sleep Hours",
     col = "purple")

# This scatter plot shows the relationship between Instagram usage and sleep.
# The trend suggests a no clear relationship there are just dots everywhere,
# meaning that as usage increases, sleep tends to be unpredictable


barplot(table(IG$gender),
        main = "Gender Distribution",
        col = "orange")

# This bar chart shows the number of users in each gender category.
# It helps us understand whether the dataset is balanced or dominated by one group.
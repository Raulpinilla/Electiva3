#' R DATA ANALYSIS
#'
#In this workshop, we performed an analysis of flight data using the nycflights13 package and the tidyverse collection of packages in the R programming language.
#'
#' @param exercise_num The number of the exercise (between 1 and 6).
#' @return The solution for the specified exercise.
#' @export
#'
#' @examples
#' retrieve_answer(2)
#' retrieve_answer(5)
# Function to select questions and answers according to exercise number

retrieve_answer <- function(exercise_num) {
  # Exercise 5.2.4: Items 1 y 2
  if (exercise_num == "1") {
    f_d <- nycflights13::flights
    myDf1 <- filter(f_d, arr_delay >= 2)
    myDf2 <- filter(f_d, dest == "HOU")
    mensaje1 <- "This code block solves item 1 of the exercise. We are using the filter() function of the tidyverse package to select only the flights_data rows where the value in the arr_delay (delay on arrival) column is greater than or equal to 2. The result is stored in the filtered_delayed_flights object./n"
    return(list(myDf1 = myDf1, myDf2 = myDf2))
  }

  # Exercise 5.3.1: All items
  if (exercise_num == "2") {
    sorted_flights_missing_first <- flights %>% arrange(desc(is.na(dep_time)))
    most_delayed_flights <- flights %>% arrange(desc(arr_delay))
    fastest_flights_desc <- flights %>% mutate(speed = distance / air_time) %>% arrange(desc(speed))
    farthest_flights <- flights %>% arrange(desc(distance))
    closest_flights <- flights %>% arrange(distance)
    mensaje2 <- "Item 1: In this code block, we are performing the following actions:\n

We take the data set flights and pass it through the %>% operator, which allows us to chain operations.\n

We use the arrange() function to sort the rows in descending order based on the is.na(dep_time) column. This column is logical and returns TRUE if dep_time (departure time) is absent and FALSE if it is not. This means that we are sorting the flights so that those with missing values in dep_time appear first.\n

item 2: In this code block:\n

We take the flights dataset and pass it through the %>% operator.\n

We use the arrange() function to sort the rows in descending order based on the arr_delay column. This means that flights with longer arrival delays will appear first.\n

item 3: In this code:\n

We take the flights dataset and pass it through the %>% operator.\n

We use the mutate() function to create a new column called speed. We calculate the speed by dividing the distance column by the air_time column. This will give us the speed of each flight.\n

After creating the speed column, we use the arrange() function to sort the rows in descending order based on the speed column. This means that flights with the highest speed will appear first.\n

item 4: In this code:\n

We take the flights dataset and pass it through the %>% operator.\n

We use the arrange() function to sort the rows in descending order based on the distance column. This means that flights with the longest distances will appear first.\n

Item 5: In this code:\n

We take the flights dataset and pass it through the %>% operator.\n

We use the arrange() function to sort the rows in ascending order based on the distance column. This means that flights with the shortest distances will appear first.\n"
    cat(mensaje2)

    return(list(
      sorted_flights_missing_first = sorted_flights_missing_first,
      most_delayed_flights = most_delayed_flights,
      fastest_flights_desc = fastest_flights_desc,
      farthest_flights = farthest_flights,
      closest_flights = closest_flights
    ))
  }
  #5.4.1 Exercises: Items 2, 3, and 4#####
  if (exercise_num == "3") {

    mensaje3 <- "R's select() function, if you include the name of a variable multiple times, it will appear multiple times in the resulting output. For example:\n

{r message=FALSE}\n
select(flights, dep_time, dep_time)\n

This would include the dep_time column twice in the resulting output.\n

The any_of() function is used to select columns from a dataframe based on a character vector of column names. It's helpful when you have a vector of column names and want to select only those columns that match any of the names in the vector. For instance:\n

{r message=FALSE}\n
vars <- c(year, month, day, dep_delay, arr_delay)\n
select(flights, any_of(vars))\n

This code selects columns with names year, month, day, dep_delay, and arr_delay from the flights dataframe.\n

The contains() helper function selects columns that contain a specified string in their names. By default, it's case-insensitive. For example:\n

{r message=FALSE}\n
select(flights, contains(TIME))\n

This code selects columns whose names contain the string TIME, such as dep_time and arr_time.\n"

    cat(mensaje3)
  }
  # Exercises 5.5.2: Items 1 y 2
  if (exercise_num == "4") {
    flights_modified <- flights %>%
      mutate(
        dep_time_mins = (dep_time %/% 100) * 60 + dep_time %% 100,
        sched_dep_time_mins = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100)

    comparison_result <- flights_modified %>%
      mutate(arr_dep_time_diff = arr_time - dep_time_mins) %>%
      filter(!is.na(air_time) & !is.na(arr_dep_time_diff)) %>%
      select(air_time, arr_dep_time_diff)
    mensaje4 <-"Item 1:\n
      In this code block:\n

We take the flights dataset and pass it through the %>% operator.\n

We use the mutate() function to add new columns to the data. We're creating two new columns: dep_time_mins and sched_dep_time_mins.\n

For each of these columns, we're performing calculations to convert time values (stored in HHMM format) into minutes since midnight. We use the %/% operation to get the hours and % to get the minutes.\n

After executing this code block, the flights_modified dataset will contain the original columns along with the new dep_time_mins and sched_dep_time_mins columns, representing the departure times (scheduled and actual) in minutes since midnight.\n
Item 2: \n
In this second code block:\n

We take the flights_modified dataset (the result of the previous block) and pass it through the %>% operator.\n

We use the mutate() function to create a new column called arr_dep_time_diff. We're calculating the difference between the arrival time (arr_time) and the departure time in minutes since midnight (dep_time_mins)\n.

Next, we use filter() to remove rows where there are missing values in the air_time or arr_dep_time_diff columns.\n

Finally, we use select() to choose only the air_time and arr_dep_time_diff columns.\n"


    cat(mensaje4)

    return(comparison_result)
  }
  #5.6.7 Exercises: item 1#
  if (exercise_num == "5") {
    mensaje5 <- "Median Arrival Delay: Calculate the median arrival delay for the group of flights. This provides a central value that represents the typical delay experienced upon arrival.\n

Proportion of Flights with Specific Delays: Determine the percentage of flights that arrive either 15 minutes early or 15 minutes late, 30 minutes early or 30 minutes late, or 2 hours late. This helps understand the distribution of different delay scenarios within the group.\n

Average Departure Delay: Calculate the average departure delay for the group of flights. This gives insight into the average delay that occurs before a flight takes off.\n

Punctuality Percentage: Calculate the percentage of flights that are punctual (no arrival delay) and compare it to the percentage of flights that are extremely delayed (2 hours late). This provides an understanding of how often flights are on time versus significantly delayed.\n

Arrival Delay Distribution: Create a histogram or density plot showing the distribution of arrival delays across all flights. This visual representation helps identify common delay ranges and outliers.\n

Question: What's More Important - Arrival Delay or Departure Delay?\n

This question addresses whether arrival delay or departure delay has a greater impact on the overall flight experience. It depends on various factors. Arrival delay affects passengers' schedules, connecting flights, and ground transportation plans. Departure delay can impact scheduling and cause inconvenience, but it might be easier to manage in some cases compared to arrival delays that can ripple through subsequent plans.\n

Both aspects are important, but the significance may vary depending on passengers' priorities and the nature of their travel plans.\n"
    cat(mensaje5)
  }

  # Exercise 5.7.1: Item 2
  if (exercise_num == "6") {
    worst_punctuality <- flights %>%
      group_by(tailnum) %>%
      summarize(
        total_flights = n(),
        punctual_flights = sum(arr_delay <= 0, na.rm = TRUE),
        punctuality_percentage = (punctual_flights / total_flights) * 100
      ) %>%
      arrange(punctuality_percentage) %>%
      filter(!is.na(punctuality_percentage))
    mensaje6 <- "In this code:\n

We start with the flights dataset and use the %>% operator to chain subsequent operations.\n

We group the data by aircraft tail number (tailnum). This means that subsequent calculations will be performed separately for each aircraft.\n

We use the summarize() function to calculate summary statistics for each group (aircraft). Within the summarize() function:\n

total_flights is calculated using the n() function, which gives the total number of flights for each aircraft.\n

punctual_flights is calculated using the sum() function. It counts the number of flights where the arrival delay (arr_delay) is less than or equal to 0 (indicating on-time or early arrivals). The na.rm = TRUE argument handles missing values in the arr_delay column.\n

punctuality_percentage is calculated as the ratio of punctual flights to total flights, multiplied by 100 to get the percentage.\n

After the summarize() operation, we use arrange() to sort the groups (aircraft) based on their punctuality percentages in ascending order. This means aircraft with the lowest punctuality percentages will appear first.\n

We use filter() to remove rows where the punctuality_percentage is not available (NA).\n

The resulting dataset is assigned to worst_punctuality, and it contains aircraft tail numbers, the total number of flights, the number of punctual flights, and the corresponding punctuality percentages.\n

Finally, we display the worst_punctuality dataset to see the tail numbers of aircraft with the lowest punctuality percentages.\n"
    cat(mensaje6)
    return(worst_punctuality)
  }
  # If don´t found the exercise
  return("Ejercicio no encontrado.")
}

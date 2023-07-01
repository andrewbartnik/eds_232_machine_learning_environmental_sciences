

library("tidymodels")
library("tidyverse")
library("dplyr")
library("janitor")
library("corrplot")
dat <- read_csv(file = "https://raw.githubusercontent.com/MaRo406/eds-232-machine-learning/main/data/pumpkin-data.csv")

# Clean names to the snake_case convention
pumpkins <- dat %>% clean_names(case = "snake")
# Return column names
pumpkins %>% names()


pumpkins <- pumpkins %>% select(variety, city_name, package, low_price, high_price, date)
## Print data set
pumpkins %>% slice_head(n = 5)


## Load lubridate
library(lubridate)
# Extract the month and day from the dates and add as new columns
pumpkins <- pumpkins %>%
  mutate(date = mdy(date),  
         day = yday(date),
         month = month(date))
pumpkins %>% 
  select(-day)
## View the first few rows
pumpkins %>% slice_head(n = 7)




# Create a new column price
pumpkins <- pumpkins %>% 
  mutate(price = (low_price+ high_price)/2)


# Scatter plot
ggplot(data = pumpkins, aes(x = day, y = price)) + geom_point() + 
  labs(title = "Pumpkin Price by Date", 
       x = "Day",
       y = 'Price ($)')


# Verify the distinct observations in Package column
pumpkins %>% 
  distinct(package)


# Retain only pumpkins with "bushel" in the package column
new_pumpkins <- pumpkins |> 
  filter(str_detect(package, "bushel"))
# Get the dimensions of the new data
dim(new_pumpkins)
# View a few rows of the new data
new_pumpkins %>% 
  slice_head(n = 10)








# Convert the price if the Package contains fractional bushel values
new_pumpkins <- new_pumpkins %>% 
  mutate(price = case_when(
    str_detect(package, "1 1/9") ~ price/(1.1),
    str_detect(package, "1/2") ~ price*2,
    TRUE ~ price))
# View the first few rows of the data
new_pumpkins %>% 
  slice_head(n = 30)




# Set theme
theme_set(theme_light())
# Make a scatter plot of month and price
new_pumpkins %>% 
  ggplot(mapping = aes(x = day, y = price)) +
  geom_point(size = 1.6) 


  




# Find the average price of pumpkins per month
month <- new_pumpkins |>
  group_by(month) |>
  summarise(avg_price = mean(price))




# Find the average price of pumpkins per month then plot a bar chart
ggplot(data = month, aes(x = month, y = avg_price)) + geom_bar(stat = 'identity') + 
  labs(x = "Month", y = "Price ($)", title = 'Avg Pumpkin Price by Month')

# Preprocessing data for modelling using recipes



# Specify a recipe
pumpkins_recipe <- recipe(price ~ ., data = new_pumpkins) %>% 
  step_integer(all_predictors(), zero_based = TRUE)
# Print out the recipe
pumpkins_recipe


# Prep the recipe
pumpkins_prep <- prep(pumpkins_recipe)
# Bake the recipe to extract a preprocessed new_pumpkins data
baked_pumpkins <- bake(pumpkins_prep, new_data = NULL)
# Print out the baked data set
baked_pumpkins %>% 
  slice_head(n = 10)



length(unique(baked_pumpkins$city_name))



# Find the correlation between the package and the price
cor(baked_pumpkins$package, baked_pumpkins$price)




# Find the correlation between the variety, city_name and price
cor(baked_pumpkins$variety, baked_pumpkins$price)
cor(baked_pumpkins$city_name, baked_pumpkins$price)


# Load the corrplot package
library(corrplot)
# Obtain correlation matrix
corr_mat <- cor(baked_pumpkins %>% 
                  # Drop columns that are not really informative
                  select(-c(low_price, high_price)))
# Make a correlation plot between the variables
corrplot(corr_mat, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45, addCoef.col = "black", cl.pos = "n", order = "original")

set.seed(123)
# Split the data into training and test sets
pumpkins_split <- new_pumpkins %>% 
  initial_split(prop = 0.8)
# Extract training and test data
pumpkins_train <- training(pumpkins_split)
pumpkins_test <- testing(pumpkins_split)
# Create a recipe for preprocessing the data
lm_pumpkins_recipe <- recipe(price ~ package, data = pumpkins_train) %>% 
  step_integer(all_predictors(), zero_based = TRUE)
# Create a linear model specification
lm_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")


# Hold modelling components in a workflow
lm_wf <- workflow() %>% 
  add_recipe(lm_pumpkins_recipe) %>% 
  add_model(lm_spec)
# Print out the workflow
lm_wf




# Train the model
lm_wf_fit <- lm_wf %>% 
  fit(data = pumpkins_train)
# Print the model coefficients learned 
lm_wf_fit



# Make predictions for the test set
predictions <- lm_wf_fit %>% 
  predict(new_data = pumpkins_test)
# Bind predictions to the test set
lm_results <- pumpkins_test %>% 
  select(c(package, price)) %>% 
  bind_cols(predictions)
# Print the first ten rows of the tibble
lm_results %>% 
  slice_head(n = 10)


# Evaluate performance of linear regression
metrics(data = lm_results,
        truth = price,
        estimate = .pred)



# Encode package column
package_encode <- lm_pumpkins_recipe %>% 
  prep() %>% 
  bake(new_data = pumpkins_test) %>% 
  select(package)
# Bind encoded package column to the results
 plot_results <- lm_results %>%
 bind_cols(package_encode %>%
               rename(package_integer = package)) %>%
  relocate(package_integer, .after = package)
# Print new results data frame
plot_results %>%
  slice_head(n = 5)
# Make a scatter plot
plot_results %>%
  ggplot(mapping = aes(x = package_integer, y = price)) +
   geom_point(size = 1.6) +
   # Overlay a line of best fit
   geom_line(aes(y = .pred), color = "orange", size = 1.2) +
   xlab("package")


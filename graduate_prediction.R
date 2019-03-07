# Predicting Graduate Admissions

# Libraries needed
library(tidyverse)
library(tree)

# Set seed for replication 
set.seed(69)

# Let's get this data 
df <- read.csv("file:///C:/Users/ckeeter/Documents/Kaggle/Grad Admissions/Admission_Predict_Ver1.1.csv")

summary(df)

df$University.Rating <- as.factor(df$University.Rating)

df$Research <- as.factor(df$Research)

# Quickly see if it's all worth it

chance_aov <- aov(Chance.of.Admit ~ . , data = df)

summary(chance_aov)

# Answer: yes 

# Clean up some unwanted variables 

df$Serial.No. <- NULL

# Make GPA more readable 

df <- df %>% 
  mutate(CGPA = CGPA/2.5)

# Let's take a closer look at the data 

gre_score <- ggplot(
  data = df, 
  aes(
    x = GRE.Score
  )
) + 
  geom_histogram(
    binwidth = 4,
    color = "black",
    fill = "blue"
  ) + 
  labs(
    x = "GRE Score",
    title = "Distribution of Combined GRE Scores",
    y = NULL
  )

gre_score

toefl_score <- ggplot(
  data = df, 
  aes(
    x = TOEFL.Score
  )
) + 
  geom_histogram(
    binwidth = 4,
    color = "black",
    fill = "blue"
  ) + 
  labs(
    title = "Distribution of TOEFL Scores",
    x = "TOEFL Score",
    y = NULL
  )

toefl_score

rating <- ggplot(
  data = df, 
  aes(
    x = University.Rating
  )
) + 
  geom_bar(
    stat = "count",
    color = "black",
    fill = "blue"
  ) +
  labs(
    title = "University Ratings Considered in Sample",
    x = "University Rating",
    y = NULL
  )

rating

SOP <- ggplot(
  data = df,
  aes(
    x = SOP
  )
) + 
  geom_histogram(
    binwidth = .5,
    color = "black",
    fill = "blue"
  ) + 
  labs(
    title = "Distribution of Statement of Purpose Ratings",
    x = "SOP Rating (1-5)",
    y = NULL
  )

SOP

LOR <- ggplot(
  data = df, 
  aes(
    x = LOR
  )
) + 
  geom_histogram(
    binwidth = .5,
    color = "black",
    fill = "blue"
  ) + 
  labs(
    title = "Distribution of Letter of Recommendation Ratings",
    x = "Letter of Recommendation Rating (1-5)",
    y = NULL
  )

LOR

CGPA <- ggplot(
  data = df,
  aes(
    x = CGPA
  )
) + 
  geom_histogram(
    binwidth = .1,
    color = "black",
    fill = "blue"
  ) + 
  labs(
    title = "Distribution of College GPAs",
    x = "GPA (4.0 scale)",
    y = NULL
  )

CGPA

research <- ggplot(
  data = df,
  aes(
    x = Research
  )
) + 
  geom_bar(
    stat = "count",
    color = "black",
    fill = "blue"
  ) + 
  labs(
    title = "Distribution of Research Experience",
    y = NULL
  )

research

# Let's try a regression tree 

train = sample(1:nrow(df), nrow(df)*.8)

test = -train

training_data <- df[train,] 

testing_data <- df[test,]

testing_admit <- df$Chance.of.Admit[test]

model <- tree(formula = Chance.of.Admit ~ . ,
              data = training_data)

plot(model, type = "uniform");text(model, pretty = 0)

tree_predict <- predict(model, testing_data)

# Mean squared error 
mean((tree_predict - testing_admit)^2) # 0.5% MSE (killer)

# Let's prune 

cv_tree <- cv.tree(model)

plot(cv_tree$size, cv_tree$dev, type = "b")

which.min(cv_tree$dev)

cv_tree$size[which.min(cv_tree$dev)]

pruned_tree <- prune.tree(model, best = cv_tree$size[which.min(cv_tree$dev)])

plot(pruned_tree, type = "uniform"); text(pruned_tree, pretty = 0)

pruned_predict <- predict(pruned_tree, testing_data)

mean((pruned_predict - testing_admit)^2) # MSE = .5% 

# Word so this is for all universites. Let's try the highest rated school 

high_df <- df %>% 
  filter(University.Rating == "1")

# Now time for a tree 

train_high = sample(1:nrow(high_df), nrow(high_df)*.8)

test_high = -train_high

high_df_train <- high_df[train_high, ]

high_df_test <- high_df[test_high, ]

high_admit_test <- high_df$Chance.of.Admit[test]

# Time to garden 

high_model <- tree(Chance.of.Admit ~ .,
                   data = high_df_train
                   )

plot(high_model, type = "uniform"); text(high_model, pretty = 0)

predicted_high <- predict(high_model, high_df_test)

mean((predicted_high - high_admit_test)^2) # 1.6% MSE 

# Time to prune 

cv_high <- cv.tree(high_model)

plot(cv_high$size,
     cv_high$dev,
     type = "b")

pruned_high <- prune.tree(high_model, best = 4)

plot(pruned_high, type = "uniform"); text(pruned_high, pretty = 0)

high_pruned_predict <- predict(pruned_high, high_df_test)

mean((high_pruned_predict - high_admit_test)^2) # Still 1.6% MSE 

# Hella high 

# Let's try for the low rated schools 

low_df <- df %>% 
  filter(University.Rating == "5")

low_train = sample(1:nrow(low_df), nrow(low_df)*0.8)

low_test = -low_train 

low_training <- low_df[low_train, ]

low_testing <- low_df[low_test, ]

low_admit <- low_df$Chance.of.Admit[low_test]

# Time to make the tree 

low_tree <- tree(formula = Chance.of.Admit ~ . , 
                 data = low_training)

plot(low_tree, type = "uniform"); text(low_tree, pretty = 0)

predicted_low <- predict(low_tree, low_testing)

mean((predicted_low - low_admit)^2) #0.08% MSE 

## Maybe prune 

cv_low <- cv.tree(low_tree)

plot(cv_low$size,
     cv_low$dev,
     type = "b")

# No point in pruning 

# Note: the MSE for many of these trees are super low, so there's really no point in pruning

# What about  for people that don't take the TOEFL? 

# We'll just do it for the general populace regardless of type of school they want to go to

us_df <- df %>% 
  select(-TOEFL.Score, -University.Rating)

summary(us_df)

# Time to model some trees 

us_train = sample(1:nrow(us_df), nrow(us_df)*0.8)

us_test = -us_train

us_training <- us_df[us_train, ]

us_testing <- us_df[us_test, ]

us_admit_test <- us_df$Chance.of.Admit[us_test]

# Some trees 

us_tree <- tree(formula = Chance.of.Admit ~ ., 
                data = us_df)

plot(us_tree, type = "uniform"); text(us_tree, pretty = 0)

us_predicted <- predict(us_tree, us_testing)

mean((us_predicted - us_admit_test)^2) # MSE = 0.4% NICE

cv.us <- cv.tree(us_tree)

plot(cv.us$size,
     cv.us$dev,
     type = "b") # JTBS analysis 

# Let's test this data with my own shit 

carson <- as.data.frame(t(c(310, 3.8, 4.7, 3.98, 1)))

names(carson) <- c("GRE.Score", "SOP", "LOR", "CGPA", "Research")

carson$Research <- as.factor(carson$Research)

predict(us_tree, carson)

# Let's try with someone else 

test_student <- as.data.frame(t(c(260, 2, 3, 2.45, 0)))

names(test_student) <- c("GRE.Score", "SOP", "LOR", "CGPA", "Research")

test_student$Research <- as.factor(test_student$Research)

test_predicted <- predict(us_tree, test_student)

mean((test_predicted - us_admit_test)^2)

# Prediction = 91.3% chance which is probably true because I'm in grad school currently 

# Let's see what happens with a linear model 

lm_us <- lm(Chance.of.Admit ~ ., data = us_df)

summary(lm_us)

predict(lm_us, carson)

summary(lm_us)$coefficients
# Prediction = 91.8% chance 

# It looks like the linear model might be a bit more liberal in the estimation (barely)

# Time for the middle school

middle_df <- df %>% 
  filter(University.Rating == "3")

train_middle = sample(1:nrow(middle_df), nrow(middle_df)*0.8)

test_middle = -train_middle

middle_training <- middle_df[train_middle, ]

middle_testing <- middle_df[test_middle, ]

middle_admit_test <- middle_df$Chance.of.Admit[test_middle]

# Time for trees 

middle_tree <- tree(Chance.of.Admit ~ ., 
                    data = middle_training)

plot(middle_tree, type = "uniform"); text(middle_tree, pretty = 0)

predicted_middle <- predict(middle_tree, middle_testing)

mean((predicted_middle - middle_admit_test)^2) # >1% MSE HELLA 







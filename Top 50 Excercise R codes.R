# Load data
#top_50_exercises <- read.csv("C:/Users/USER/Downloads/Top 50 Excerice for your body.csv")
library()
# Display structure of the data
str(top_50_exercises)
#Objective 1
# Summary of calorie burn by exercise
summary(top_50_exercises$Burns.Calories..per.30.min.)

# Plot calorie burn by exercise name
library(ggplot2)
ggplot(top_50_exercises, aes(x = reorder(Name.of.Exercise, Burns.Calories..per.30.min.), y = Burns.Calories..per.30.min.)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(x = "Exercise", y = "Calories Burned (per 30 min)", title = "Calorie Burn by Exercise") +
  theme_minimal()

# Convert relevant variables to numeric or factor as needed
top_50_exercises$Sets <- as.numeric(top_50_exercises$Sets)
top_50_exercises$Reps <- as.numeric(top_50_exercises$Reps)
top_50_exercises$Burns.Calories..per.30.min. <- as.numeric(top_50_exercises$Burns.Calories..per.30.min.)
top_50_exercises$Difficulty.Level <- as.factor(top_50_exercises$Difficulty.Level)

# Linear regression model to predict calorie burn
calorie_model <- lm(Burns.Calories..per.30.min. ~ Sets + Reps + Difficulty.Level, data = top_50_exercises)

# Summarize the model
summary(calorie_model)





#Objective 2
# Distribution by difficulty level
difficulty_distribution <- table(top_50_exercises$Difficulty.Level)
barplot(difficulty_distribution, main = "Distribution of Exercises by Difficulty Level", col = "lightblue")

# Distribution by equipment requirements
equipment_distribution <- table(top_50_exercises$Equipment.Needed)
barplot(equipment_distribution, main = "Distribution of Exercises by Equipment Needed", col = "lightgreen")

ggplot(top_50_exercises, aes(x = Difficulty.Level, fill = Equipment.Needed)) +
  geom_bar(position = "dodge") +
  labs(title = "Exercise Difficulty Levels by Equipment Needed", x = "Difficulty Level", y = "Count") +
  theme_minimal()
# Recode difficulty into a binary variable: 1 for Intermediate/Advanced, 0 for Beginner
top_50_exercises$Difficulty.Binary <- ifelse(top_50_exercises$Difficulty.Level == "Beginner", 0, 1)

# Logistic regression model
difficulty_model <- glm(Difficulty.Binary ~ Equipment.Needed + Reps, data = top_50_exercises, family = binomial)

# Summarize the model
summary(difficulty_model)




#Objective 3
# Aggregate average sets and reps per muscle group
exercise_plan <- aggregate(cbind(Sets = as.numeric(Sets), Reps = as.numeric(Reps)) ~ Target.Muscle.Group, data = top_50_exercises, FUN = mean)

# Plot sets and reps by muscle group
ggplot(exercise_plan, aes(x = Target.Muscle.Group)) +
  geom_bar(aes(y = Sets), stat = "identity", fill = "coral") +
  geom_bar(aes(y = Reps), stat = "identity", fill = "lightblue", alpha = 0.5) +
  labs(title = "Average Sets and Reps by Target Muscle Group", x = "Target Muscle Group", y = "Average Count") +
  theme_minimal() +
  coord_flip()

# Convert sets and reps to numeric for clustering
top_50_exercises$Sets <- as.numeric(top_50_exercises$Sets)
top_50_exercises$Reps <- as.numeric(top_50_exercises$Reps)

# Select relevant columns and scale data
exercise_data <- top_50_exercises[, c("Sets", "Reps")]
exercise_data <- scale(exercise_data)

# K-means clustering
set.seed(123)
clusters <- kmeans(exercise_data, centers = 3) # Adjust 'centers' based on how many clusters are desired

# Add cluster labels to the original data
top_50_exercises$Cluster <- as.factor(clusters$cluster)

# Visualize clusters
ggplot(top_50_exercises, aes(x = Sets, y = Reps, color = Cluster)) +
  geom_point() +
  labs(title = "Clustering of Exercises by Sets and Reps", x = "Sets", y = "Reps") +
  theme_minimal()


# Select and scale data for clustering
clustering_data <- scale(top_50_exercises[, c("Sets", "Reps")])

# Apply K-means clustering
set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 3) # Set centers based on analysis

# Add cluster labels to the original data
top_50_exercises$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
library(ggplot2)
ggplot(top_50_exercises, aes(x = Sets, y = Reps, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Clustering of Exercises by Sets and Reps", x = "Sets", y = "Reps") +
  theme_minimal()


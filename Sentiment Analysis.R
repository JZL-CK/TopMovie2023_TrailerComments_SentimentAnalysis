
### Set Directory

setwd("E:/Research work/Courses-Lessons/Sentiment analysis/Sentiment analysis of YTB comments on top movies 2023/Analysis")

### Libraries

#install.packages("pheatmap")
library(syuzhet)
library(ggplot2)
library(reshape2)
library(pheatmap)

#------------------------------------------------------------------------------#


### Read file

# Vector of file names

movie_names <- c("About_dry_grasses.csv", "American_fiction.csv", "Anatomy_of_a_fall.csv", "Dungeons_and_dragons.csv", "Fallen_leaves.csv",
                "Godzilla.csv", "Guardians_of_the_Galaxy.csv", "John_wick.csv", "Killers_of_the_Flower_Moon.csv", "Mission_impossible.csv",
                "Oppenheimer.csv", "Perfect_days.csv", "Poor_things.csv", "Spiderman.csv", "The_boy_and_the_heron.csv",
                "The_holdovers.csv", "The_promised_land.csv", "The_taste_of_things.csv", "The_zone_of_interest.csv", "Past_lives.csv")



# Loop to read each CSV file and assign it to a variable

for (i in 1:length(movie_names)) {
  
  # Construct the variable name
  var_name <- paste0("data_frame_", i)
  
  # Read the CSV file
  data_frame <- read.csv(movie_names[i], stringsAsFactors = FALSE)
  
  # Assign the data frame to the variable name
  assign(var_name, data_frame)
}


#------------------------------------------------------------------------------#


### Data Transformation 


# Loop to create new data frames with transformed text data
for (i in 1:20) {
  
  # Construct the variable name for the original data frame
  original_data_frame_name <- paste0("data_frame_", i)
  
  # Get the original data frame from the environment
  original_data_frame <- get(original_data_frame_name)
  
  # Check if the original data frame has the 'text' column
  if ("text" %in% colnames(original_data_frame)) {
    
    # Extract and transform the text data
    text_data <- as.character(original_data_frame$text)
    text_data <- iconv(text_data, from = "latin1", to = "UTF-8")
    text_data <- tolower(text_data)
    
    # Create a new data frame with the transformed text data
    new_data_frame <- original_data_frame
    new_data_frame$text <- text_data
    
    # Create a variable name for the new data frame
    new_data_frame_name <- paste0("cmnt", i)
    
    # Assign the new data frame to the new variable name
    assign(new_data_frame_name, new_data_frame)
  } else {
    warning(paste("Original data frame", original_data_frame_name, "does not have a 'text' column"))
  }
}


#------------------------------------------------------------------------------#


### Sentiment Analysis


# Obtain sentiment score

get_nrc_sentiment('happy')
get_nrc_sentiment('abuse')


## Perform Sentiment analysis on all the cmnt files


# Movie names in the specified order
movie_names <- c("About_dry_grasses", "American_fiction", "Anatomy_of_a_fall", 
                 "Dungeons_and_dragons", "Fallen_leaves", "Godzilla", 
                 "Guardians_of_the_Galaxy", "John_wick", "Killers_of_the_Flower_Moon", 
                 "Mission_impossible", "Oppenheimer", "Perfect_days", 
                 "Poor_things", "Spiderman", "The_boy_and_the_heron", 
                 "The_holdovers", "The_promised_land", "The_taste_of_things", 
                 "The_zone_of_interest", "Past_lives")

# Initialize an empty list to store sentiment scores
sentiment_scores <- list()

# Loop to process each cmnt variable and calculate sentiment scores
for (i in 1:20) {
  
  # Construct the variable name for the transformed text data
  cmnt_var_name <- paste0("cmnt", i)
  
  # Get the transformed text data from the environment
  cmnt_data <- get(cmnt_var_name)
  
  # Ensure cmnt_data is a character vector
  cmnt_data <- as.character(cmnt_data)
  
  # Perform sentiment analysis on the transformed text data
  sentiment <- get_nrc_sentiment(cmnt_data)
  
  # Store the sentiment scores in the list
  sentiment_scores[[i]] <- colSums(sentiment)
}

# Combine the sentiment scores into a data frame
sentiment_scores_df <- do.call(rbind, sentiment_scores)

# Add the movie names as the first column
sentiment_scores_df <- data.frame(Movie = movie_names, sentiment_scores_df)

# Save the data frame to a CSV file
write.csv(sentiment_scores_df, "movie_sentiment_scores.csv", row.names = FALSE)


sentiment_scores_df



#------------------------------------------------------------------------------#


### Sentiment Score Percentages



# Ensure that all sentiment score columns are numeric
numeric_columns <- names(sentiment_scores_df)[2:11]  # Assuming the first column is 'Movie' and the rest are sentiment scores

sentiment_scores_df[numeric_columns] <- lapply(sentiment_scores_df[numeric_columns], function(x) as.numeric(as.character(x)))

# Function to convert sentiment scores to percentages
convert_to_percentage <- function(row) {
  print(class(row["negative"]))  # Debugging: Print the class of 'negative'
  print(class(row["positive"]))  # Debugging: Print the class of 'positive'
  
  total <- as.numeric(row['negative']) + as.numeric(row['positive'])  # Ensure values are numeric
  row[-1] <- (as.numeric(row[-1]) / total) * 100
  return(row)
}

# Apply the conversion function to each row of the sentiment_scores_df
sentiment_scores_df_percentage <- as.data.frame(t(apply(sentiment_scores_df, 1, convert_to_percentage)))

# Ensure the Movie column is retained as character type
sentiment_scores_df_percentage$Movie <- as.character(sentiment_scores_df$Movie)

# Reorder columns to have Movie first
sentiment_scores_df_percentage <- sentiment_scores_df_percentage[, c("Movie", setdiff(names(sentiment_scores_df_percentage), "Movie"))]

# Print the data frame to verify
print(sentiment_scores_df_percentage)

View(sentiment_scores_df_percentage)

# Save the data frame to a CSV file
write.csv(sentiment_scores_df_percentage, "movie_sentiment_scores_percentage.csv", row.names = FALSE)




#------------------------------------------------------------------------------#


### Sentiment Analysis Visualizations



## Barplot 



# Convert sentiment score columns to numeric if they aren't already
sentiment_columns <- names(sentiment_scores_df_percentage)[-1]  # Exclude 'Movie' column
sentiment_scores_df_percentage[sentiment_columns] <- lapply(sentiment_scores_df_percentage[sentiment_columns], function(x) as.numeric(as.character(x)))

# Melt the dataframe to long format for easier plotting
df_long <- melt(sentiment_scores_df_percentage, id.vars = "Movie")

# Create the barplot of sentiment scores per movie
V1 <- ggplot(df_long, aes(x = Movie, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment Scores per Movie", x = "Movie", y = "Score (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot as a PNG file
ggsave("sentiment_scores_per_movie.png", plot = V1, width = 10, height = 6, dpi = 300)

# Print the plot to the R console (optional)
print(V1)




## Stacked Barplot



# Filter the dataframe to include only sentiment columns (excluding 'negative' and 'positive')
sentiment_columns <- names(sentiment_scores_df_percentage)[-c(1, 10, 11)]  # Exclude 'Movie', 'negative', and 'positive'
df_sentiment_long <- melt(sentiment_scores_df_percentage, id.vars = "Movie", measure.vars = sentiment_columns)

# Stacked barplot of sentiment distribution per movie
V2 <- ggplot(df_sentiment_long, aes(x = Movie, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution per Movie", x = "Movie", y = "Sentiment Score (%)") +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot as a PNG file
ggsave("sentiment_distribution_per_movie.png", plot = V2, width = 10, height = 6, dpi = 300)

# Print the plot to the R console (optional)
print(V2)



## Heatmap



# Set rownames as Movie for heatmap
rownames(sentiment_scores_df_percentage) <- sentiment_scores_df_percentage$Movie
heatmap_data <- sentiment_scores_df_percentage[, -1]  # Exclude the Movie column

# Create the heatmap
V3 <- pheatmap(as.matrix(heatmap_data), 
               cluster_rows = FALSE, cluster_cols = FALSE,
               display_numbers = TRUE, fontsize_number = 10,
               main = "Heatmap of Sentiment Scores")

# Save the heatmap as a PNG file
png("heatmap_sentiment_scores.png", width = 10, height = 6, units = "in", res = 300)
print(V3)
dev.off()



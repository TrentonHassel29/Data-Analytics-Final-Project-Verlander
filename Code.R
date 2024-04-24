library(dplyr)
# Read the dataset
df = read_xlsx : ('/Users/trentonhassel/Desktop/verlander-pitches-2022.xlsx')
colnames(df)=df[1,]
df=df[-1,]

# Group by pitch type and calculate mean pitch speed
pitch_speed_summary = df %>% group_by(pitch_type) %>% 
  dplyr::summarise(mean_speed=mean(as.numeric(release_speed)))


# Create a bar chart
ggplot(data= pitch_speed_summary, aes(x= pitch_type, y=mean_speed)) +
  geom_bar(stat ="Identity") +
  coord_cartesian(ylim = c(70, 100))

--------------------------------------------------------------
# Calculate type of play for each pitch type by description
hit_rate_by_pitch_type = df %>% group_by(pitch_type, description) %>% 
  dplyr::summarise(type_of_play=n())

# Create a bar chart
ggplot(data= hit_rate_by_pitch_type, aes(x= type_of_play, y=paste(pitch_type,description))) +
  geom_bar(stat ="Identity") + coord_flip() +
  coord_cartesian(xlim = c(0, 425))

---------------------------------------------------------------
 # Calculate total pitch count for each pitch type
pitch_count_by_type = df %>% group_by(pitch_type) %>% 
  dplyr::summarise(count_of_amount_thrown=n())

# Create bar chart
ggplot(data= pitch_count_by_type, aes(x= pitch_type, y=count_of_amount_thrown)) +
  geom_bar(stat ="Identity") +
  coord_cartesian(ylim = c(50, 1600))

-----------------------------------------------------------------
# Check relationship between pitch speed and hit rate
Ball_Speed_Play = df %>% group_by(description) %>% 
  dplyr::summarise(mean_speed=mean(as.numeric(release_speed)))

# Create bar chart
ggplot(data= Ball_Speed_Play, aes(x= mean_speed, y=description)) +
  geom_bar(stat ="Identity") + coord_flip() +
  coord_cartesian(xlim = c(80, 100))

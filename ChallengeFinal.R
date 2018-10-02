# Finding Events in the Chicagoland area

# An attempt 3 dimentional clustering

# Finding events on the ground is hard. They take place within a specific variable
# timeframe (think music festival versus  happy hour),
# within certain variable confines (think Cubs game versus a street vendor), 
# and people say anything they want about them! Furthermore people may tweet about
# event which they are not currently located at
  
# In this challenge, I will attempt to shine some light on data taken
# from the Twitter API using a handful of statistical and graphical packages in R.
# I can port this over to python at somepoint, but as my master's program focuses
# on statprog with R I am much more comfortable with it at the moment.

require("ggmap")
require("googleway")
require("tidyverse")
require("tidytext")
require("anytime")
require("rgl")
require("wordcloud")
require("ggraph")
# Load Packages needed

map_key <- "AIzaSyCc-efpvd90bhsiSVhKAHH4HJ0UFWyIrXM" # My API key pls dont steal

#################################################################
# Chicago Dataset                                               #
#################################################################

setwd('C:/Users/Matth/Desktop/MSCA FALL18/event_challenge')
# Set working directory, you will need to change this 

Chicago_df <- read_csv("chicago.csv")
#Read in csv


Chicago_df <- Chicago_df %>%
  select(time,user,lat,lng,caption) %>%
  mutate(HumanTime = anytime(time, tz="America/Chicago")) %>%
  arrange(time)

# As an initial tidying step, I'd like to append a human readable
# timestamp to the dataframe alongside the UNIX timecode which works
# much nicer for math and plotting. We also order by time for more sensical 
# plotting

ggplot(Chicago_df, aes(x = time)) +
  geom_histogram(binwidth = 86400) +
  ggtitle("Tweet Activity")

# Histogram does not show anything particularly useful other than one spike at UNIX
# 1516500000

anytime(1516500000, tz="America/Chicago")

# A cursory google search shows this likely correlates with the US government shutdown
# Not especially useful, moving on.

# Let's plot time,lattitude, and longitude together in 3d to see if we can get any cursory
# density clusters of events (which would occur at a certain place and time)

 
open3d() 
x <- Chicago_df$time 
y <-  Chicago_df$lat
z <- Chicago_df$lng 
plot3d(x, y, z, col = "blue")

# looking at this dynamic scatter on the x vs either coordinate does not yield
# much useful information a the time is too macro at this level. We do see the
# "pulses" of a night/day cycle. Small gap near middle of plot alludes to twitter
# downtime?
# However rotating to y vs z shows a relatively detailed map of chicago with
# Tweet density - we can even see navy pier. How cool!

# Linear model

# Compute the linear regression (y = ax + bz + d)
fit <- lm(y ~ x + z)
# predict values on regular xz grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
z.pred <- seq(min(z), max(z), length.out = grid.lines)
xz <- expand.grid( x = x.pred, z = z.pred)
y.pred <- matrix(predict(fit, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)
# Add regression surface
rgl.surface(x.pred, z.pred, y.pred, color = "steelblue", 
            alpha = 0.5, lit = FALSE)  
# Add grid lines
rgl.surface(x.pred, z.pred, y.pred, color = "black",
            alpha = 0.5, lit = FALSE, front = "lines", back = "lines")

# Separating hyperlane linear model which is not extremely useful other
# than showing tweet density much higher in south Chicago - likely 
# Co-linnear with population density.




x2 <- Chicago_df$time[0:4000] 
y2 <-  Chicago_df$lat[0:4000]
z2 <- Chicago_df$lng[0:4000]
plot3d(x2, y2, z2, col = "purple")
# Smaller approach showing approx 2 day cycle

# Lets link this to google maps now
# Unfortunately, plotting all 71k entities in this fashion breaks my laptop,
# so lets take a small slice as proof of concept

Chicago_df_small <- Chicago_df[0:200,]

google_map(data = Chicago_df_small, key = map_key) %>%
  add_markers(lat = "lat", lon = "lng", info_window = "caption")

# even from the first 200 tweet sample we have taken we see a density
# which agrees with our 3d scatter of all points!


google_map(data = Chicago_df_small, key = map_key) %>%
  add_heatmap(lat = "lat", lon = "lng")

# we can also plot as heatmap

# Next lets run some basic sentiment analysis on the captions themselves to see 
# if insight can be gleaned.

# Lets tokenize our captions which is the process of splitting the
# caption string word by word and running count and sort to see what we can find.


tokenized_caption <- unnest_tokens(Chicago_df,text,caption, token = "tweets")

# As you can see we have just generated a 6 million point dataframe!
# This equates to 13 words per tweet on average

# Unfortunately we are likely to see many conjunctions and prepositions 
# (the, on, a, etc.) known as "stop words" which will hinder our insights.
# Let's filter them out!

tokenized_caption <- tokenized_caption %>%
  filter(nchar(text) > 3)

# This immediately filtered roughly 1/3rd of the data!

sort_tokens_df <- tokenized_caption %>%
  count(text, sort = TRUE) %>%
  filter(n > 75, !text %in% c("rt", "t.co"))


# A quick look through the sort shows that most tokens are seen less 10 times
# Sorting by text immediately shows hashtags, which are also a little
# stochastic 



# Below represents broken or pseudo code which I could not get working 
# before deadline






 for (i in sort_tokens_df){

  
res <- google_places(location = c(Chicago_df_small$lat[i],Chicago_df_small$lng[i]),
              radius = 1,
              key = map_key)

# Finds location of specific tweet, accurate to 1 meter

type <- access_result(res, result = "place_type")
# Queries place type from previous api call

PoI_df <- as.data.frame(type[2]) %>%
  filter("point_of_interest") %>%
 
# Determines if this was a place of interest and adds to df
}


## Sample event

Killers_df <- read_csv("sample_event.csv", col_names = c("time","UserId","lat","lng","caption"))



Killers_df <- Killers_df %>%
  select(time,UserId,lat,lng,caption) %>%
  mutate(HumanTime = anytime(time, tz="America/Chicago")) %>%
  arrange(time)


duration <- Killers_df$time[89] - Killers_df$time[1]

duration <- duration/3600
#people tweeted about event over a 21.4 hour period

open3d() 
x <- Killers_df$time 
y <-  Killers_df$lat
z <- Killers_df$lng 
plot3d(x, y, z, col = "black")

#We see tweets primarily coming from several locations only
# Although scale is a little too zoomed to be of much use
google_map(data = Killers_df, key = map_key) %>%
  add_markers(lat = "lat", lon = "lng", info_window = "caption")

tokenized_caption_event <- unnest_tokens(Killers_df,text,caption, token = "tweets")

tokenized_caption_event <- tokenized_caption_event %>%
  filter(nchar(text) > 3)

sort_tokens__event_df <- tokenized_caption_event %>%
  count(text, sort = TRUE) %>%
  filter(n > 5, !text %in% c("rt", "t.co"))


# Based off the sorted tweets, we can tell with large degree of certainty killers are
# playing at the United Center!


# Future challenges #

# Determine loop for "elastic search" in which sorts are done on progressively
# smaller radius (based off POIs) until large degree of text similarity is found.
#Code would function somewhat like:

# Groups

Elastic search until some degree of text similarity is found append median location to df

THEN
 
groups <- df$(lat,lng)
levs <- levels(groups)
group.col <- c("highsimilarityzone1", "highsimilarityzone2", "highsimilarityzone3")
# Plot observations
rgl_init()
rgl.spheres(x, y, z, r = 0.2,
            color = group.col[as.numeric(groups)]) 
rgl_add_axes(x, y, z, show.bbox = FALSE)
# Compute ellipse for each group
for (i in 1:length(levs)) {
  group <- levs[i]
  selected <- groups == group
  xx <- x[selected]; yy <- y[selected]; zz <- z[selected]
  ellips <- ellipse3d(cov(cbind(xx,yy,zz)), 
                      centre=c(mean(xx), mean(yy), mean(zz)), level = 0.95) 
  shade3d(ellips, col = group.col[i], alpha = 0.1, lit = FALSE) 
  # show group labels
  texts3d(mean(xx),mean(yy), mean(zz), text = group,
          col= group.col[i], cex = 2)
}
aspect3d(1,1,1)


# This clusters high similarity zones! 

# Unsure how to read t.co shortened links at this time.
# Improve regexp abilities to better analyze string data.
# Implement analytics on streaming tweets, which is possible
# with rtweet package.
# Streaming would also allow for real time temporal triggers.
# Graphical networks showing nodes and edges between retweets, follows, etc.
# Create custom "location seeking" lexicon in which words such as here, now, this,
# etc are very heavily weighted.
# Turn lat,lng into single column for predictions? (I believe this is somehow possible)

# Add machine learning - K-nearest or SVM?

# Thanks for the challenge project, I had fun!





# library
library(wordcloud2) 
library(viridis)

wordcloud2(data=demoFreq, size=2, color = viridis(n = 50, option = "B"),
           backgroundColor = "grey",
           shape = 'circle')

# Change the shape using your image
# This is an example. You should use something that is easy to fit the wordcloud
# in
# wordcloud2(demoFreq, 
#           figPath = "~/Pictures/Aviation pictures/._RIAT2017flights-217.jpg",
#           size = 1.5, 
#           color = "skyblue", 
#           backgroundColor="black")



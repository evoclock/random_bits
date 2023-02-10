# Sample script to reorder variables on plots.
# Some of these are adapted from Kyle W. Brown's R gallery book
# with examples that are more applicable to my work.

library(tidyr)
library(ggplot2)
library(dplyr)
library(forcats)
library(viridis)



data = iris[ , 1:4]
head(data)

# Ensure your data is in long format for gpplot to operate on like so
# key and value are arguments supplied by you
data_long = data %>% gather(key = "MeasureType", value = "Val") 


# Reorder your categorical variables
# generating a sample dataset for categorical variables.

data = data.frame(
  name = c("loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8"),
  val = sample(seq(1,10), 8 )
)

data %>% 
  mutate(name = fct_reorder(name, val)) %>%
  ggplot(aes(x = name, y = val)) +
  geom_bar(stat = "identity", fill = "#008080", alpha = .5 , width = .4 ) +
  coord_flip() +
  xlab("") +
  theme_bw()

# If you want it in descending order then you pass the numerical values to 
# the "desc()" function.

data %>% 
  mutate(name = fct_reorder(name, desc(val))) %>%
  ggplot(aes(x = name, y = val)) +
  geom_bar(stat = "identity", fill = "#008080", alpha = .5 , width = .4 ) +
  coord_flip() +
  xlab("") +
  theme_bw()

# If there are several values per level of a factor you can specify which
# function to apply to it in order to determine the order in which you want
# it plotted. For example you can use the default median, or the number of 
# observations.

# Median

mpg %>%
  mutate(class = fct_reorder(class, hwy, .fun = 'median')) %>%
  ggplot(aes(x = reorder(class, hwy), y = hwy, fill = class)) +
  geom_boxplot() +
  xlab("class") +
  theme(legend.position = "none") +
  xlab("")

# Number of observations
# x does not need to be reordered when using the number of observations

mpg %>%
  mutate(class = fct_reorder(class, hwy, .fun = 'length')) %>%
  ggplot(aes(x = class, y = hwy, fill = class)) +
  geom_boxplot() +
  xlab("class") +
  theme(legend.position = "none") +
  xlab("")

# standard deviation
# once more x does not need to be reordered when using standard deviation (sd)
mpg %>%
  mutate(class = fct_reorder(class, hwy, .fun = 'sd')) %>%
  ggplot(aes( x = class, y = hwy, fill = class)) +
  geom_boxplot() +
  xlab("class") +
  theme(legend.position = "none") +
  xlab("")

# If you want to force a specific order then you need to use fct_relevel()

plot = data %>%
  mutate(name = fct_relevel(name,
                            "loc4", 
                            "loc5", 
                            "loc6", 
                            "loc1", 
                            "loc2", 
                            "loc3", 
                            "loc7", 
                            "loc8")) %>%
  ggplot(aes (x = name, y = val)) +
  geom_bar(stat = "identity",  fill = "#008080", alpha = .5 , width = .4) +
  xlab("")

plot

# Now with base R
mpg$class = with(mpg, reorder(class, hwy, median))

plot = mpg %>%
  ggplot(aes (x = class, y = hwy, fill = class)) +
  geom_violin() +
  xlab("class") +
  theme(legend.position = "none") +
  xlab("")

plot

### How to hightlight a group you want to draw attention to
# add a column called "type" to the mpg dataset

mpg %>%
  mutate(type = ifelse(class == "suv", "Highlighted", "Normal")) %>%
  # now you need to use the "type" column as "fill"  
  ggplot(aes(x = class, y = hwy, fill = type, alpha = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#008080", "grey")) +
  scale_alpha_manual(values = c(.9, .1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("")


### Grouped boxplot on ggplot
# group must be called in the x argument and subgroups in the fill argument

# create dummy set

variety = rep(LETTERS[1:7], each = 40)
treatment = rep(c("high latitude","low latitude"), each = 20)
note = seq(1:280) + sample(1:150, 280, replace=T)
data = data.frame(variety, treatment, note)

# grouped boxplot 
ggplot(data, aes(x = variety, y = note, fill = treatment)) + 
  geom_boxplot()


# reordering category by median
# Create data : 7 varieties / 20 samples per variety / a numeric value for 
# each sample

variety = rep( c("ARNTL", "CLOCK", "PER1", "PER2", "PER3", "CRY1", "CRY2"), 
               each=20)
note = c(sample(2:5, 20 , replace=T) , sample(6:10, 40 , replace=T),
           sample(1:7, 30 , replace=T), sample(3:10, 50 , replace=T))

data = data.frame(variety, note)

# vector containing desired order

n_order = with(data, reorder(variety, note, median, na.rm = T))

# plot with base R

boxplot(data$note ~ n_order, 
        xlab = "Genes ordered per median variant length",
        ylab = "variant length", 
        col = "#008080", 
        boxwex = .4,
        main = "")

# Specifying an order by reodering the levels with the factor() function

names = c(rep("ARNTL", 20), rep("OPN4", 20), rep("PER3", 20), rep("CLOCK", 20))

value = c( sample(2:8, 20 , replace=T) , sample(1:7, 20 , replace=T), 
            sample(4:10, 20 , replace=T), sample(1:4, 20 , replace=T) )

data = data.frame(names, value)

# reoder the levels
data$names = factor(data$names, 
                    levels = c("PER3", "OPN4", "ARNTL", "CLOCK"))

boxplot(data$value ~ data$names, 
        col = rgb(0.2,0.4,0.6,0.8), 
        ylab = "value",
        xlab = "ordered gene names")

# How to group and order a boxplot in base R

genes = rep( c("ARNTL", "CLOCK", "PER1", "PER2", "PER3", "CRY1", "CRY2"), 
             each=40)
treatment = rep(c(rep("high_latitude" , 20) , 
                  rep("low_latitude" , 20)) , 7)

note = c(rep(c(sample(0:7, 20 , replace=T),
               sample(1:6, 20 , replace=T)), 2), 
         rep(c(sample(5:10, 20 , replace=T),
               sample(5:9, 20 , replace=T)),2), 
         c(sample(6:10, 20 , replace=T),
           sample(4:9, 20 , replace=T),
           rep(c(sample(3:8, 20 , replace=T),
                 sample(4:10, 20 , replace=T)),2) ))

data = data.frame(variety, treatment, note)

# Reorder varieties (group) (mixing low and high treatments for the calculations)
n_order = with(data, reorder(variety , note, mean , na.rm=T))

# Then make the boxplot, asking to use the 2 factors : 
# variety (in the good order) AND treatment :

par(mar=c(3,4,3,1))

plot = boxplot(note ~ treatment*n_order , data=data  , 
                  boxwex=0.4 , ylab="number of variants",
                  main="variant diversity of multiple genes" , 
                  col=c("slateblue1" , "tomato") ,  
                  xaxt="n")

# To add the label of x axis
names = sapply(strsplit(plot$names , '\\.') , function(x) x[[2]] )
names = names[seq(1 , length(names) , 2)]
axis(1, at = seq(1.5 , 14 , 2), 
     labels = names , 
     tick=FALSE , cex=0.3)

# Add the grey vertical lines
for(i in seq(0.5 , 20 , 2)){ 
  abline(v=i,lty=1, col="grey")
}

# Add a legend
legend("bottomright", legend = c("High latitude", "Low latitude"), 
       col=c("slateblue1" , "tomato"),
       pch = 15, bty = "n", 
       pt.cex = 3, 
       cex = 1.2,  
       horiz = F, 
       inset = c(0.1, 0.1))



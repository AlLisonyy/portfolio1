Portfolio 2
================
Allison Li
02102025

``` r
##install.packages("tidytuesdayR")
##install.packages(c("maps", "ggplot2"))

library(maps)
library(ggplot2)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2023-10-10')
haunted_places <- tuesdata$haunted_places
```

I get the haunted houses dataset from the Tidy Tuesday Github
(<https://github.com/rfordatascience/tidytuesday/blob/main/data/2023/2023-10-10/readme.md>).
My research question in general is to see where are the haunted houses
in the US!

### Haunted houses in North Caronlina

``` r
##first I want to know where are the haunted houses in North Carolina.
##I would filter the dataset into only houses in NC.
NC_haunted_places <- haunted_places %>% 
  filter(state == "North Carolina")
```

Now I know that there are 211 haunted houses in North Caronlina, which
is a litte scary. Next, I wanted to know how they are located within
North Caronlina.

### Most and Least Haunted houses in North Carolina

``` r
##After taking a look at the dataset, I want to merge Winston-Salem to Winston because they refer to the same location. 
NC_haunted_places <- NC_haunted_places %>%
mutate(
  city = case_when(
    city == "Winston-Salem" ~ "Winston",
    TRUE ~ city
  ))

##I want to see which city has the most and least haunted houses in NC. 
haunted_counts_NC <- NC_haunted_places %>%
  count(city, name = "num_haunted_houses") %>% 
  arrange(desc(num_haunted_houses))
print(haunted_counts_NC)
```

    ## # A tibble: 128 × 2
    ##    city         num_haunted_houses
    ##    <chr>                     <int>
    ##  1 Asheville                    10
    ##  2 Charlotte                     8
    ##  3 Winston                       8
    ##  4 Fayetteville                  6
    ##  5 Concord                       5
    ##  6 Morganton                     5
    ##  7 Raleigh                       5
    ##  8 Havelock                      4
    ##  9 Jacksonville                  4
    ## 10 Lenoir                        4
    ## # ℹ 118 more rows

``` r
## Mean 
mean_hauntedhousesNC <- mean(haunted_counts_NC$num_haunted_houses)
## Standard deviation 
sd_hauntedhousesNC <- sd(haunted_counts_NC$num_haunted_houses)
```

Before looking at the houses location visualization, I merged
Winston-Salem to Winston. After that, I wanted to check which cities in
NC has the most or least haunted houses. According to the dataset,
Asheville appeared to have the most haunted houses, n = 1, while Lenoir,
Jacksonville, and Havelock have the least haunted houses, n = 1. On
average, each city in North Carolina seems to has 1.65 haunted houses.

### 

``` r
##I also want to see a bar graph of which city has the most haunted houses.
top_haunted_cities <- NC_haunted_places %>%
  count(city, name = "num_haunted_houses") %>%
  arrange(desc(num_haunted_houses)) %>%
  slice_head(n = 10)

# Create the bar plot for the top ten cities 
ggplot(top_haunted_cities, aes(x = reorder(city, num_haunted_houses), y = num_haunted_houses, colour = city, fill = city)) +
  geom_bar(stat = "identity") +  
  labs(title = "Top 10 Most Haunted Cities in North Carolina",
       x = "City",
       y = "Number of Haunted Houses") +
       coord_flip()
```

![](portfolio1_files/figure-gfm/top%205%20citie%20sthat%20has%20the%20most%20or%20least%20haunted%20houses%20in%20NC-1.png)<!-- -->

### visualized map of NC

``` r
# Get North Carolina map data
nc_map <- map_data("state")

# Filter for North Carolina
nc_map <- subset(nc_map, region == "north carolina")

# Plot the map
ggplot(nc_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgray", color = "black") +
  coord_fixed(1.3) +
  ggtitle("Map of North Carolina")
```

![](portfolio1_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
ggplot(NC_haunted_places, mapping = aes(
  x = longitude,
  y = latitude,
  color = state
)) +
  geom_point(alpha = .5, color = "red") +
  labs(
    title= "North Carolina haunted houses Locations",
    x = "Longitude", 
    y = "Latitude"
    )
```

    ## Warning: Removed 23 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](portfolio1_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

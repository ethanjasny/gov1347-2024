---
title: Example Blog Post
author: Ethan and Yusuf
date: '2024-09-01'
slug: fun-time-foods
categories: []
tags: []
---



## ETHAN


```r
abbrevs <- read.csv("states.csv")
state_data <- read.csv("clean_wide_state_2pv_1948_2020 (1).csv") %>% 
  rename("State" = state) %>%
  full_join(abbrevs, by = "State")


# Define map layout 
map_layout <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white'),
  showlakes = FALSE)

# Create Voteshare Map
pv2p_map <- plot_geo(state_data, locationmode = "USA-states") %>% 
  add_trace(z = ~D_pv2p, locations = ~Abbreviation,
            color = ~D_pv2p, colors = "RdBu", frame = ~year, # The raceYear frame creates an animation based on year
            showscale = TRUE, zmin = 0, zmax = 100) %>% # Define color scale min and max
  layout(title = "US House GOP Two-Party Vote Share by State, 1948-2020", geo = map_layout)

# Swing 
state_data$D_swing <- state_data$D_pv2p - state_data$D_pv2p_lag1
swing_map <- plot_geo(state_data, locationmode = "USA-states") %>% 
  add_trace(z = ~D_swing, locations = ~Abbreviation,
            color = ~D_swing, colors = "RdBu", frame = ~year, # The raceYear frame creates an animation based on year
            showscale = TRUE, zmin = -30, zmax = 30) %>% # Define color scale min and max
  layout(title = "US House GOP Two-Party Vote Share by State, 1948-2020", geo = map_layout)
```
 
## YUSUF
 


```r
popvote_df <- read_csv("house party vote share by district 1948-2020.csv")
```

```
## Rows: 16067 Columns: 31
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (16): Office, State, Area, RepCandidate, RepStatus, DemCandidate, DemSta...
## dbl (14): raceYear, RepVotes, DemVotes, ThirdVotes, OtherVotes, PluralityVot...
## lgl  (1): CensusPop
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
house_party_vote_share_by_district_1948_2020 <- read_csv("house party vote share by district 1948-2020.csv")
```

```
## Rows: 16067 Columns: 31
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (16): Office, State, Area, RepCandidate, RepStatus, DemCandidate, DemSta...
## dbl (14): raceYear, RepVotes, DemVotes, ThirdVotes, OtherVotes, PluralityVot...
## lgl  (1): CensusPop
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
h <- house_party_vote_share_by_district_1948_2020
president_2020 <- read.csv("2020_presidential_district.csv")
```


```r
h <- h %>% 
  filter(raceYear=="2020")
president_data <- president_2020 %>% 
  filter(X2020!="Biden") %>% 
  rename(CD=X)
president_data <- left_join (president_data,h,by="CD")
president_data <- president_data %>% 
  select(X2020,X.3,State,district_num,raceYear)
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />





```r
cd117_pa <- cd117 %>% 
            filter(STATENAME=="Pennsylvania") %>%
            mutate(DISTRICT = as.character(DISTRICT))%>%
            select(DISTRICT)

# add data to plot - 2014 GOP party seat share
# reload election data - h from previous exercise
h <- president_data

# filter for 2014 election and state
R_pa_2020 <- h %>%
    filter(raceYear == 2020, State == "Pennsylvania") %>%
    select(raceYear, State, district_num, X.3, X2020) %>%
  # summarize party vote share by district
    group_by(district_num) %>%
    summarise(Trump_margin = as.numeric(X.3)-as.numeric(X2020)) %>%
  # rename district variable name to match shapefile
    rename(DISTRICT = district_num)


# change class
cd117_pa$DISTRICT <- as.numeric(cd117_pa$DISTRICT)

# join election returns with shapefiles
cd117_pa <- cd117_pa %>% left_join(R_pa_2020, by="DISTRICT")


suppressPlotlyMessage <- function(p) {
  suppressMessages(plotly_build(p))
}
# time to map!
pamap <- ggplot() + 
  geom_sf(data=cd117_pa,aes(fill=Trump_margin),
          inherit.aes=FALSE,alpha=0.9) +
  scale_fill_gradient2(high="red",mid="white",low="blue") +
  theme_void() + ggtitle ("Donald Trump Vote Share in 2020 PA Presidential Elections")
  
#fig2 <- suppressPlotlyMessage(ggplotly(pamap))
#fig2
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```r
cd117_wi <- cd117 %>% 
            filter(STATENAME=="Wisconsin") %>%
            mutate(DISTRICT = as.character(DISTRICT))%>%
            select(DISTRICT)

# add data to plot - 2014 GOP party seat share
# reload election data - h from previous exercise
h <- president_data

# filter for 2014 election and state
R_wi_2020 <- h %>%
    filter(raceYear == 2020, State == "Wisconsin") %>%
    select(raceYear, State, district_num, X.3, X2020) %>%
  # summarize party vote share by district
    group_by(district_num) %>%
     summarise(Trump_margin = as.numeric(X.3)-as.numeric(X2020)) %>%
  # rename district variable name to match shapefile
    rename(DISTRICT = district_num)


# change class
cd117_wi$DISTRICT <- as.numeric(cd117_wi$DISTRICT)

# join election returns with shapefiles
cd117_wi <- cd117_wi %>% left_join(R_wi_2020, by="DISTRICT")


suppressPlotlyMessage <- function(p) {
  suppressMessages(plotly_build(p))
}
# time to map!
wimap <- ggplot() + 
  geom_sf(data=cd117_wi,aes(fill=Trump_margin),
          inherit.aes=FALSE,alpha=0.9) +
  scale_fill_gradient2(high="red",mid="white",low="blue") +
  theme_void() + ggtitle ("Donald Trump Vote Share in 2020 WI Presidential Election")
  
#fig3 <- suppressPlotlyMessage(ggplotly(wimap))
#fig3
wimap
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />


```r
cd117_ga <- cd117 %>% 
            filter(STATENAME=="Georgia") %>%
            mutate(DISTRICT = as.character(DISTRICT))%>%
            select(DISTRICT)

# add data to plot - 2014 GOP party seat share
# reload election data - h from previous exercise
h <- president_data

# filter for 2014 election and state
R_ga_2020 <- h %>%
    filter(raceYear == 2020, State == "Georgia") %>%
    select(raceYear, State, district_num, X.3, X2020) %>%
  # summarize party vote share by district
    group_by(district_num) %>%
    summarise(Trump_margin = as.numeric(X.3)-as.numeric(X2020)) %>%
  # rename district variable name to match shapefile
    rename(DISTRICT = district_num)


# change class
cd117_ga$DISTRICT <- as.numeric(cd117_ga$DISTRICT)

# join election returns with shapefiles
cd117_ga <- cd117_ga %>% left_join(R_ga_2020, by="DISTRICT")


suppressPlotlyMessage <- function(p) {
  suppressMessages(plotly_build(p))
}
# time to map!
gamap <- ggplot() + 
  geom_sf(data=cd117_ga,aes(fill=Trump_margin),
          inherit.aes=FALSE,alpha=0.9) +
  scale_fill_gradient2(high="red",mid="white",low="blue") +
  theme_void() + ggtitle ("Donald Trump Vote Share in 2020 GA Presidential Election")
  
#fig4 <- suppressPlotlyMessage(ggplotly(gamap))
#fig4
gamap
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />



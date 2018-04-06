# FiveThirtyEight.com Express Riddler for 4-6-18

# Problem set: https://fivethirtyeight.com/features/when-will-the-arithmetic-anarchists-attack/?ex_cid=538twitter

# Puzzle text: 

# The year is 2000, and an arithmetical anarchist group has an idea. For the next 100 years, it will vandalize
# a famous landmark whenever the year (in two-digit form, for example this year is “18”) is the product of the 
# month and date (i.e. month × date = year, in the MM/DD/YY format).

# A few questions about the lawless ensuing century: How many attacks will happen between the beginning of 2001 
# and the end of 2099? What year will see the most vandalism? The least? What will be the longest gap between attacks?


# Puzzle Set Up
library(lubridate)

ts <- seq(ymd('2001-01-01'),ymd('2099-12-31'), by = '1 day') # all days in span

target <- NULL
for(i in 1:length(ts)){ # for loop to find dates that match criterion: d*m = y - 2000
        d <- day(ts[i])
        m <- month(ts[i])
        y <- year(ts[i])
        target[i] <- ifelse(y - 2000 == d*m, 1, 0)
}

# total number of attacks
sum(target) 

# largest number of attacks by year
df <- data.frame(cbind(target, ymd(ts), year(ts), month(ts), day(ts)))
colnames(df) <- c("target", "date", "year", "month", "day")

df %>% filter(target == 1) %>%
        mutate(year = factor(as.character(year))) %>%
        group_by(year) %>%
        summarize(attacks = sum(target)) %>%
        arrange(-attacks) %>%
        head(1)

# least attacks by year
df %>%
        mutate(year = factor(as.character(year))) %>%
        group_by(year) %>%
        summarize(attacks = sum(target)) %>%
        arrange(attacks) %>%
        filter(attacks == 0)

# largest gap between attacks
attack <- df %>% filter(target == 1)
attack$attack_gap <- NULL
for(i in 1:length(attack[,1])){
        attack$attack_gap[i] <- attack$date[i + 1] - attack$date[i]          
}
max(attack$attack_gap, na.rm = T)

# Plot attacks over time
df %>%
        mutate(year = factor(as.character(year))) %>%
        group_by(year) %>%
        summarize(attacks = sum(target)) %>%
        ggplot2::ggplot(aes(year, attacks, fill = attacks)) +
                geom_col() +
        theme(axis.title = element_text(size=14),
              axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position = "none") +
        xlab("\nYear") + 
        ylab("Number of Attacks\n")

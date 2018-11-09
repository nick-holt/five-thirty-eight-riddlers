# FiveThirtyEight.com Express Riddler for 11-9-18

# Problem set: https://fivethirtyeight.com/features/what-are-the-odds-youd-already-have-my-number/

# Puzzle text: 

# My daughter recently noticed that the 10 digits of our old landline phone number are the same digits as those in my wife’s 
# cell phone. The first three digits match exactly because we are in the same area code as before. And the last seven digits of 
# my wife’s cell phone are an exact scrambled version of the last seven digits of the old landline. By “exact scramble,” I mean 
# a string of numbers that is different than the original string of numbers but contains all of the same digits with the exact 
# same number of repetitions (so, for example, if “4” appears exactly three times in the landline number, it also appears 
# exactly three times in my wife’s cell number).

# My daughter asked, “What are the odds of that?”

# To make this concrete, assume that landlines and cell numbers in my area code are assigned randomly, such that a person is 
# equally likely to get any of the 10,000,000 numbers between and including 000-0000 to 999-9999. Given that assumption, what is 
# the probability that the last seven digits of the cell number are an exact scramble of the last seven digits of our landline?


# Libraries ---------------------------------------------------------------
library(magrittr)
library(dplyr)
library(stringr)
options(scipen = 999)

# The probability depends on how many unique digits there are in the phone number ------
# so we need to find the match probability for each of the possible values of unique
# digits in a 7 digit phone number.

set.seed(342)

# simulate one million phone numbers
numbers <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
phone_numbers <- NULL
for(i in 1:1000000){
        phone_numbers[[i]] <- sample(numbers, 7, replace = T) 
} 

# calculate the number of unique digits in each phone number
unique_digits <- NULL
for(i in seq_along(phone_numbers)){
        unique_digits[i] <- length(unique(phone_numbers[[i]]))
}

# convert each phone number to a single character value of the phone number
pn_string <- NULL
for(i in seq_along(phone_numbers)){
        pn_string[i] <- do.call(paste, c(as.list(phone_numbers[[i]]), sep = ""))
}

# create data frame of phone numbers and the number of unique digits for each
pn_data <- data.frame(unique_digits, pn_string)

# plot
hist(pn_data$unique_digits)

# keep data frame with one phone number with each number of unique digits ------

one <- pn_data %>%
        filter(unique_digits == 1) %>%
        head(1)

two <- pn_data %>%
        filter(unique_digits == 2) %>%
        head(1)

three <- pn_data %>%
        filter(unique_digits == 3) %>%
        head(1)

four <- pn_data %>%
        filter(unique_digits == 4) %>%
        head(1)

five <- pn_data %>%
        filter(unique_digits == 5) %>%
        head(1)

six <- pn_data %>%
        filter(unique_digits == 6) %>%
        head(1)

seven <- pn_data %>%
        filter(unique_digits == 7) %>%
        head(1)

example_numbers <- bind_rows(one, two, three, four, five, six, seven)
example_numbers$possible_matches <- NA

# Simulate matching phone numbers to determine probabilities --------

for(i in seq_along(example_numbers$unique_digits)){
        num <- as.numeric(str_split(example_numbers$pn_string[i], pattern = "")[[1]])
        num_list <- NULL
        for(j in 1:1000000){
                s <- sample(num, 7, replace = F)
                s_c <- do.call(paste, c(as.list(s), sep = ""))
                num_list[j] <- s_c
        } 
        example_numbers$possible_matches[i] <- length(unique(num_list)) - 1
}

# calculate probability of match based on number of unique digits in phone number
example_numbers %<>% 
        mutate(probability_match = possible_matches / 10000000)


# Simulate problem scenario (landline/cell match) -------------------------

numbers <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

probability <- NULL
for(i in 1:1000000){
        landline <- sample(numbers, 7, replace = T)
        probability[i] <- example_numbers$probability_match[example_numbers$unique_digits == length(unique(landline))]
}


# Solution: ---------------------------------------------------------------

scales::percent(mean(probability))



#------------------------------------------------------------------------
# Alternate Solution:
#------------------------------------------------------------------------


# Validate with brute force simulation ----------------------------------

numbers <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

match <- NULL
for(i in 1:1000000){
        landline <- as.numeric(do.call(paste, c(as.list(sort(sample(numbers, 7, replace = T) )), sep = ""))) 
        cell <- as.numeric(do.call(paste, c(as.list(sort(sample(numbers, 7, replace = T) )), sep = "")))
        match[i] <- ifelse(landline == cell, 1, 0)
}

# solution 

scales::percent(mean(match))

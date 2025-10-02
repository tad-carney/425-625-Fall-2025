## Submission for Tad Carney and Toby Salmon

fileurl <- '/Users/tobysalmon/Desktop/YALE/STATISTICS/S&DS 4250/17nflog.html'
x <- scan(fileurl, what = "", sep = "\n")

writeLines(x, "17nflog.html")
y <- readLines("17nflog.html")
#length(x)
#x[500]

#remove the '&nbsp;' and replace '</span>' with ',' in the html
x <- gsub('&nbsp;', '', x, fixed = TRUE) 
x <- gsub('</span>', ',', x, fixed = TRUE)

# Let's delete all the HTML.  An HTML tag starts with <, ends with >,
# and in between can have anything but not < or >.
x <- gsub("<[^<>]*>", "", x)       # fixed = FALSE is the default, okay here.

# Now let's break apart the fields of information:
y <- strsplit(x, ",")  # changes from character vector to list of char vectors
is.list(y) # checks if y is now a list

table(sapply(y, length)) 
## seems like the game data we want will be in the rows where the length is 6
y <- y[20:length(y)] # remove the first 19 rows which are not game results, just empty rows / information about html format

y <- y[sapply(y, length) > 0] # keep only the rows with at least one non-empty column



# Creating a function to parse the list into a data frame with the desired fields
# We used Claude.ai to help write out an original draft of this function, and then we modified it ourselves
# Claude.ai did a good job of getting the basic structure of the data frame, but it made some mistakes in the details, largely when dealing with populating the data frame
# added some redundant checks and if statements, and made some false assumptions
# of how to deal with some of the special cases

parse_football_data <- function(y) {
  # Create empty data frame with required columns
  result <- data.frame(
    date = character(),
    season = character(),
    team1 = character(),
    team2 = character(),
    pointspread = numeric(),
    score1 = numeric(),
    score2 = numeric(),
    location = character(),
    overtime = logical(),
    overunder = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Variables to track current team and position
  current_team <- NULL
  season <- "2017"  # All of the data comes from the 2017 season
  i <- 1
  
  while (i <= length(y)) {
    
    #SPECIAL CASES TO BE DEALT WITH FIRST 
    
    # Check for special two-element entries, these are either BYE WEEK or neutral location indicators
    if (length(y[[i]]) == 2) {
      # Check if it's NOT a BYE WEEK entry (which means it must be a neutral location indicator)
      if (!any(grepl("BYE WEEK", y[[i]]))) {
        # If we have at least one row in the result, update the last row's location to "N"
        if (nrow(result) > 0) {
          result$location[nrow(result)] <- "N"
        }
      }
      # Whether it's a neutral location or BYE WEEK, we want to skip it
      i <- i + 1
      next
    }
    
    # If length is 1, it either a team name or a record line
    if (length(y[[i]]) == 1) {
      # If it starts with a capital letter, it's a team name
      if (grepl("^[A-Z]", y[[i]])) {
        #storing the team1 name in a variable to be used until the next team1 name is found
        current_team <- gsub(" \\(AT\\)$", "", y[[i]])  # Remove " (AT)" if present
        i <- i + 2  # Skip the record line
        next
      } 
        # If it doesn't start with a capital letter, it's a record line, so skip it
        i <- i + 1
        next
    }
    
    # Skip the remaining BYE WEEK entries (of length 3)
    if (any(grepl("BYE WEEK", y[[i]]))) {
      i <- i + 1
      next
    }
    
    # Process game data row - make sure it is right length and starts with date
    # Date is of the form M.DD
    if (length(y[[i]]) >= 6 && grepl("^[A-Z]\\.[0-9]", y[[i]][1])) {
      
      #save the current row to a variable for easier reference
      game_data <- y[[i]]
      
        # Extract date - changing letter to month number
        date_parts <- strsplit(game_data[1], "\\.")[[1]]
        #strsplit returns a list, so we use [[1]] to get the first element which is a vector of c('M', 'DD')
        month <- switch(date_parts[1],
                        "S" = "09",
                        "O" = "10", 
                        "N" = "11",
                        "D" = "12",
                        "J" = "01",
                        "F" = "02") 
        
        # Remove any asterisks from the day before converting to numeric
        # We did not find anything interesting about games with asterisks
        day_cleaned <- gsub("\\*", "", date_parts[2])
        
        #formatting the day to be a two digit number
        day <- sprintf("%02d", as.numeric(day_cleaned))
        
        # Since the football season starts in September and rolls into the following year
        # We have to classify January and February (Super Bowl) in the following year
        year <- ifelse(month %in% c("01", "02"), "2018", "2017")
        
        # Combine to form date in YYYY/MM/DD format
        date <- paste(year, month, day, sep="/")
        
        # Extract opponent (team2)
        team2 <- gsub("-ot$", "", game_data[2])
        
        # Check for overtime - TRUE/FALSE due to logic of grepl
        overtime <- grepl("-ot", game_data[2])
        
        # Determine location based on capitalization of team2
        # We looked it up and found that capitalized team names indicate home games, not away games
        if (toupper(team2) == team2) {
          location <- "H"  # Home game
        } else {
          location <- "V"  # Away game (visitor)
        }
        
        
        # Extract pointspread and convert to numeric
        pointspread_str <- game_data[4]
        ps <- gsub("'", ".5", pointspread_str)
        # Remove any plus signs
        ps <- gsub("\\+", "", ps)
        # Replace "P" with "0"
        ps <- gsub("P", "0", ps)
        pointspread <- as.numeric(ps)
        
        # Extract scores
        score_parts <- strsplit(game_data[5], "-")[[1]]
        score1 <- as.numeric(score_parts[1])
        score2 <- as.numeric(score_parts[2])
        
        
        # Extract overunder and convert to numeric
        overunder_str <- game_data[6]
        ou <- gsub("'", ".5", overunder_str)
        # removing the 'o' or 'u' at the beginning of the betting lines
        # we found that we were getting NA values due to the presence of "n" in the overunder betting lines, so we removed this in addition to "o" and "u"
        ou <- gsub("[oun]", "", ou)
        overunder <- as.numeric(ou)
  
        # Add row to results
        result <- rbind(result, data.frame(
          date = date,
          season = season,
          team1 = current_team,
          team2 = team2,
          pointspread = pointspread,
          score1 = score1,
          score2 = score2,
          location = location,
          overtime = overtime,
          overunder = overunder,
          stringsAsFactors = FALSE
        ))
    }
    # next row
    i <- i + 1
  }
  
  return(result)
}

# load the data into the function
football_df <- parse_football_data(y)

# Quick sanity check, seeing if the number of rows matches the number of valid entries we parsed
nrow(football_df) == length(y[sapply(y, length) == 6])

# Write to CSV
write.csv(football_df, "football_2017.csv", row.names = FALSE)

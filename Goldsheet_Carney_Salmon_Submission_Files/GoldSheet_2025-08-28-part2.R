#' ---
#' title: "Case Study 1 - Due Tuesday 2025-09-02"
#' date: "`r Sys.Date()`"
#' author: "YourFirstName YourLastName"
#' output: pdf_document
#' # Or use output: html_document
#' # Or don't use either, in which case you will be prompted to choose
#' ---
#' 
#' ### CONTINUATION
#' 
#' This really follows from "part1" that we worked with in our first class.
#' 
#' 
#' ### To be submitted to Canvas before the next class
#' 
#' - This script with your answer/discussion of the
#' "efficient market hypothesis"
#' - A script for your NFL data scrape
#' - A CSV for your NFL data scrape
#' 
#'
#' # EXERCISES/HOMEWORK for Tuesday, September 2
#'
#' ## Basketball Goldsheet (continued from part1)
#'
#' What do you think about the economists' "efficient market hypothesis"?
#' 
#' Using the linear model, above, with the data set assembled in this script,
#' conduct a formal hypothesis test, with specific reference to a p-value
#' that you think is helpful.  Describe and defend (or criticise) it.  If
#' something concerns you, explain.
#'
#' We believe the efficient market hypothesis is reasonable to believe, that the 
#' pre-game point spreads from the betting market (i.e. the bookies) should be
#' unbiased predictors of the actual game spreads.
#' 
#' Using the linear model from part 1, we examined the relationship between the
#' point spread and the actual score difference. We tested the null hypothesis 
#' (H0) that there is no relationship between the point spread and the actual 
#' score difference (i.e., the slope of the regression line is 0) against there 
#' being a relationship (i.e., the slope is not 0) (H1). The p-value for this 
#' test was <2e-16, which is extremely small. This indicates that we can reject 
#' the null hypothesis.
#' 
#' To further investigate the efficient market hypothesis, we tested the null 
#' hypothesis that the slope of the regression line is 1 (i.e., the point spread
#' is an unbiased predictor of the actual score difference) against the
#' alternative that the slope is not 1. The p-value for this test was ~0.094,
#' so we fail to reject the null hypothesis at the 5% significance level, 
#' implying that it is reasonable to assume the efficient market hypothesis is
#' valid.
#' 
#' code for the second formal hypothesis test is below
#' 
#' co <- summary(lm.result)$coefficients
#' est <- co["pointspread","Estimate"]
#' se  <- co["pointspread","Std. Error"]
#' df  <- lm.result$df.residual
#' t_stat   <- (est - 1) / se
#' p_two    <- 2 * pt(abs(t_stat), df=df, lower.tail = FALSE)  # two-sided
#' p_right  <- pt(t_stat, df=df, lower.tail = FALSE)           # H1: β > 1
#' p_left   <- pt(t_stat, df=df, lower.tail = TRUE)            # H1: β < 1 
#' 
#' ## NFL Goldsheet (related but different)
#' 
#' Different sport, same data source, and so many similarities to what
#' we did for the college basketball example!  You'll use `17nflog.html`,
#' from the NFL 2017 season.  There are 32 teams.
#' 
#' Adapt this script (or else create your own script from scratch) to
#' assemble a complete CSV for the NFL season provided.  When done, please
#' upload your script to CANVAS along with the CSV file it produces.
#' 
#' The CSV file should include (variable names provided at the end of each
#' bullet):
#' 
#' - Date of game (let me recommend a format such as YYYY/MM/DD as a nice
#' choice, where you'll have to do some work and add the year carefully); `date`
#' - Season (like `2017`); `season`
#' - Team1 (the name of the primary team for a block of results); `team1`
#' - Team2 (the listed opponent); `team2`
#' - Pointspread; `pointspread`
#' - Score1; `score1`
#' - Score2; `score2`
#' - Location (please use simply `H`, `V`, or `N`; capital letters of
#' opponents may indicate games where the primary team is the visitor (?);
#' and maybe games in London, for example, are `N`?); `location`
#' - Overtime indicator (logical `TRUE/FALSE`) given by -ot; `overtime`
#' - OverUnder (the "over/under" betting line, which is the numeric
#' part of the very last column on the page -- the `o` or `u` are not needed);
#' `overunder`
#' - You can ignore/discard the `*` unless you think it is important or at
#' least interesting.
#' 
#' We are not giving complete instructions, above, and we know there are
#' a few places where you'll have to think about what is "reasonable" or
#' "helpful" or "desirable".  This isn't a problem set for a calculus class!
#' Explain any choices/decisions you made in comments of your script.
#' 
#' 
#'
#' ## Prepare for Tuesday: REVIEW/LEARNING but nothing to be turned in here.
#'
#' What are the connections, similarities, differences, strengths, and
#' weaknesses of vectors, matrices, lists, and data frames?
#'
#' What are the differences between sapply() and lapply() and apply()?
#'
#' Create a nice toy example of a regular expression where the fixed = TRUE/FALSE
#' option makes a critical difference.
#'
#' Explore what this script does if you delete any/all `byrow = TRUE` options to
#' the matrix() function.
#'
#' What are the differences between `[]` and `[[]]`?  If `z` is a list,
#' how are `z[8]` and `z[[8]]` similar or different?  What happens if 
#' `length(z)` is only 7?
#'
#' Using the data frame `x` created above in this script, explore why
#' a default (no option usage) `write.csv()` followed by a `read.csv()`
#' are a problem.  Disclaimer: It's always possible that different versions of
#' R could produce different results for something like this.
#'
#'
#'

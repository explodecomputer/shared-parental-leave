# how much nursery time is needed based on working time

# how many days (or hours) of nursery are needed
# what is the salary reduction 



nursery_time <- function(parent1, parent2) {
	# parent 1 days per week
	# parent 2 days per week
	# number of nursery days needed 

	nweek <- length(parent1)
	stopifnot(length(parent1) == length(parent2))
	nurs <- data_frame(week = 1:nweek)
	nurs$p1 <- parent1
	nurs$p2 <- parent2
	nurs$nursery <- nurs$p1 + nurs$p2 - 5

	nurs$on_leave <- "p1"
	nurs$on_leave[nurs$p2 == 0] <- "p2"
	nurs$on_leave[nurs$p1 != 0 & nurs$p2 != 0] <- "None"
	nurs$on_leave[nurs$week > 52] <- "None"
	return(nurs)
}


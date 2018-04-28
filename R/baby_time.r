# THIS IS ALL DEPRECATED

baby_awake_time <- function(waking_time, sleeping_time, start_date, duration)
{
	require(lubridate)
	start_day1 <- lubridate::ymd_hm(paste(start_date, waking_time))
	end_day1 <- lubridate::ymd_hm(paste(start_date, sleeping_time))

	waking <- start_day1 + lubridate::days(0:(duration*7))
	sleeping <- end_day1 + lubridate::days(0:(duration*7))

	awake <- lubridate::interval(waking, sleeping)
	return(awake)
}



work_time <- function(leave_time, home_time, start_date, work_days, duration)
{
	start_day1 <- lubridate::ymd_hm(paste(start_date, leave_time))
	end_day1 <- lubridate::ymd_hm(paste(start_date, home_time))

	leave <- start_day1 + lubridate::days(0:(duration*7))
	home <- end_day1 + lubridate::days(0:(duration*7))

	index <- ! wday(leave, label=TRUE) %in% work_days 
	home[index] <- leave[index]

	at_work <- lubridate::interval(leave, home)

	return(at_work)
}



#' Calculate time with baby for one parent
#' 
#' Some description
#' 
#' @param bwt output from \code{baby_awake_time}
#' @param wt output from \code{work_time}
#' 
#' @export
#' @return vector
time_with_baby <- function(bwt, wt)
{
	baby_time <- lubridate::hour(lubridate::as.period(bwt, "hours"))
	overlap <- baby_time - lubridate::hour(lubridate::as.period(intersect(bwt, wt), "hours"))
	return(overlap)
}


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



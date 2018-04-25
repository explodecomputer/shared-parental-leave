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



time_with_baby <- function(bwt, wt)
{
	baby_time <- lubridate::hour(lubridate::as.period(bwt, "hours"))
	overlap <- baby_time - lubridate::hour(lubridate::as.period(intersect(bwt, wt), "hours"))
	return(overlap)
}


bwt <- baby_awake_time("07:00", "19:00", "2018-08-25", 52)
wt <- work_time("08:45", "17:15", "2018-08-25", c("Tue", "Wed", "Thu", "Fri"), 52)
time_with_baby(bwt, wt)

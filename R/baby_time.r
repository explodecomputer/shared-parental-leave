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

parse_day_time <- function(x)
{
	words <- strsplit(x, split=" ")[[1]]
	stopifnot(length(words) == 2)

	# Time
	stopifnot(grepl("-", words[2]))
	times <- strsplit(words[2], split="-")[[1]]

	# Days
	dayseq <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
	days <- strsplit(words[1], ",")[[1]]

	index <- which(grepl("-", days))
	if(length(index) > 0)
	{
		l1 <- list()
		for(i in 1:index)
		{
			temp1 <- strsplit(days[index[i]], "-")[[1]]
			stopifnot(all(temp1 %in% dayseq))
			temp1 <- dayseq[which(dayseq == temp1[1]):which(dayseq == temp1[2])]
			l1[[i]] <- temp1
		}
		days <- c(days[-index], unlist(l1))
	}
	days <- dayseq[dayseq %in% days]
	out <- data_frame(days=days, start_time=times[1], end_time=times[2])
	return(out)
}

working <- function(weeks, ...)
{
	l <- list(...)
	if(length(l) == 0)
	{
		l[[1]] <- "Mon-Fri 09:00-17:00"
	}
	out <- lapply(l, parse_day_time) %>% bind_rows
	l2 <- list()
	for(i in 1:weeks)
	{
		l2[[i]] <- out
		l2[[i]]$section_week <- i
	}
	return(bind_rows(l2))
}

on_leave <- function(weeks)
{
	a <- expand.grid(days=c("Mon", "Tue", "Wed", "Thu", "Fri"), week=1:weeks) 
	a$start_time <- "09:00"
	a$end_time <- "09:00"
	a <- data_frame(days=as.character(a$days), start_time=a$start_time, end_time=a$end_time, section_week=as.numeric(a$week))
}

work_plan <- function(start_date, ...)
{
	all_days <- bind_rows(...)
	index <- diff(all_days$section_week) == 0
	week <- data_frame(section_week = all_days$section_week[!index])
	week$week <- 1:nrow(week)
	all_days <- inner_join(all_days, week, by=c("section_week"))

	return(all_days)
}


working("Mon-Thu 09:00-17:00", "Fri 09:00-12:00", weeks=26)

x <- "Mon-Fri 09:00-17:00"

parse_daytime("Mon-Fri 09:00-17:00")
parse_daytime("Mon,Wed,Fri 09:00-17:00")
parse_daytime("Mon-Wed,Fri 09:00-17:00")


work_plan("2018-08-25",
	working(26, "Mon-Fri 09:00-17:00"),
	on_leave(26),
	working(104, "Tue-Fri 09:00-17:00")
)




work_schedule("2018-08-25",
	working("09:00", "17:00", c("Mon", "Tue", "Wed", "Thu", "Fri"), 26),
	on_leave(26),
	working("09:00", "17:00", c("Tue", "Wed", "Thu", "Fri"), 104)
)



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


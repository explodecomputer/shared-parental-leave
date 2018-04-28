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
	days_work <- dayseq[dayseq %in% days]
	out <- dplyr::data_frame(days=days, start_time=times[1], end_time=times[2])
	days_notwork <- dayseq[!dayseq %in% days_work]
	if(length(days_notwork) != 0)
	{
		out2 <- dplyr::data_frame(days=days_notwork, start_time="09:00", end_time="09:00")
		out <- rbind(out, out2)
		stopifnot(nrow(out) == 7)
		out <- out[match(dayseq, out$days), ]
	}
	return(out)
}

working <- function(weeks, ...)
{
	l <- list(...)
	if(length(l) == 0)
	{
		l[[1]] <- "Mon-Fri 09:00-17:00"
	}
	out <- lapply(l, parse_day_time) %>% dplyr::bind_rows()
	l2 <- list()
	for(i in 1:weeks)
	{
		l2[[i]] <- out
		l2[[i]]$section_week <- i
	}
	return(dplyr::bind_rows(l2))
}

on_leave <- function(weeks)
{
	a <- expand.grid(days=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), week=1:weeks) 
	a$start_time <- "09:00"
	a$end_time <- "09:00"
	a <- dplyr::data_frame(days=as.character(a$days), start_time=a$start_time, end_time=a$end_time, section_week=as.numeric(a$week))
	return(a)
}

work_plan <- function(start_date, ...)
{
	l <- list(...)
	for(i in 1:length(l))
	{
		l[[i]]$section <- i
	}
	all_days <- dplyr::bind_rows(l)
	index <- diff(all_days$section_week) == 0
	week <- subset(all_days, !duplicated(paste(section, section_week)), select=c(section, section_week))
	week$week <- 1:nrow(week)
	all_days <- dplyr::inner_join(all_days, week, by=c("section", "section_week"))

	# closest_monday_to_start_date 
	# mon=2
	start_datel <- lubridate::ymd(start_date)
	d <- wday(start_datel)
	sub <- d - 2
	sub <- ifelse(sub < 0, 6, sub)
	start_datel2 <- lubridate::ymd(start_date) - days(sub)

	all_days$date <- as.character(start_datel2 + lubridate::days((1:nrow(all_days))-1))

	all_days$at_work <- lubridate::interval(
		ymd_hm(paste(all_days$date, all_days$start_time)),
		ymd_hm(paste(all_days$date, all_days$end_time))
	)

	all_days <- dplyr::select(all_days, week, section, section_week, at_work)

	return(all_days)
}


baby_time <- function(wp, waking_time="07:00", sleeping_time="19:00")
{
	start_date <- lubridate::int_start(wp$at_work[1]) %>% lubridate::date()
	duration <- nrow(wp)
	start_day1 <- lubridate::ymd_hm(paste(start_date, waking_time))
	end_day1 <- lubridate::ymd_hm(paste(start_date, sleeping_time))

	waking <- start_day1 + lubridate::days((0:(duration-1)))
	sleeping <- end_day1 + lubridate::days((0:(duration-1)))

	awake <- lubridate::interval(waking, sleeping)

	start_day1 <- lubridate::ymd_hm(paste(start_date, "09:00"))
	end_day1 <- lubridate::ymd_hm(paste(start_date, "17:00"))

	go_nursery <- start_day1 + lubridate::days((0:(duration-1)))
	return_nursery <- end_day1 + lubridate::days((0:(duration-1)))

	index <- wday(return_nursery, label=TRUE) %in% c("Sat", "Sun")
	return_nursery[index] <- go_nursery[index]

	nursery <- lubridate::interval(go_nursery, return_nursery)

	out <- dplyr::data_frame(week=wp$week, awake=awake, nursery=nursery)
	return(out)
}

wp_overlap <- function(wp_parent1, wp_parent2, wp_baby)
{
	stopifnot(nrow(wp_parent1) == nrow(wp_parent2))

	wp <- data_frame(week=wp_parent1$week, section=wp_parent1$section, section_week=wp_parent1$section_week, at_work1=wp_parent1$at_work, at_work2=wp_parent2$at_work)

	# For some reason I have to put this
	# wp_baby$awake[1] <- wp_baby$nursery[1]

	wp$both_at_work <- lubridate::intersect(wp$at_work1, wp$at_work2)
	wp$need_care <- lubridate::intersect(wp$both_at_work, wp_baby$nursery)
	wp$need_careh <- lubridate::hour(lubridate::as.period(wp$need_care, "hours"))
	
	baby_time <- lubridate::hour(lubridate::as.period(wp_baby$awake, "hours"))

	wp$with_baby1 <- baby_time - lubridate::hour(lubridate::as.period(lubridate::intersect(wp_baby$awake, wp$at_work1), "hours"))
	wp$with_baby2 <- baby_time - lubridate::hour(lubridate::as.period(lubridate::intersect(wp_baby$awake, wp$at_work2), "hours"))

	return(wp)
}


summarise_weeks <- function(wp_parent1, wp_parent2)
{
	wp_parent1$hr <- lubridate::hour(lubridate::as.period(wp_parent1$at_work, "hours"))
	wp_parent2$hr <- lubridate::hour(lubridate::as.period(wp_parent2$at_work, "hours"))
	week1 <- group_by(wp_parent1, week) %>%
		summarise(nday=sum(round(hr/8*2)/2))
	week2 <- group_by(wp_parent2, week) %>%
		summarise(nday=sum(round(hr/8*2)/2))

	stopifnot(nrow(week1) == nrow(week2))
	nweek <- nrow(week1)
	nurs <- data_frame(week = 1:nweek)
	nurs$p1 <- week1$nday
	nurs$p2 <- week2$nday
	nurs$nursery <- nurs$p1 + nurs$p2 - 5

	nurs$on_leave <- "p1"
	nurs$on_leave[nurs$p2 == 0] <- "p2"
	nurs$on_leave[nurs$p1 != 0 & nurs$p2 != 0] <- "None"
	nurs$on_leave[nurs$week > 52] <- "None"
	return(nurs)
}



parental_leave_salary_model <- function(x, maxweek=52)
{
	mod <- rbind(
		dplyr::data_frame(
			option = 1,
			weeks=c(1:maxweek),
			drop=c(rep(0, 8), rep(0.5, 16), rep(1, 15), rep(1, maxweek-39)),
			spp=c(rep(F, 8), rep(T, 16), rep(T, 15), rep(F, maxweek-39))
		),
		dplyr::data_frame(
			option = 2,
			weeks=c(1:maxweek),
			drop=c(rep(0, 16), rep(1, 23), rep(1, maxweek-39)),
			spp=c(rep(F, 16), rep(T, 23), rep(F, maxweek-39))
		)
	)
	return(subset(mod, option == x))
}


salary_calc <- function(base_salary, annual_increment, wp)
{
	weekly_salary <- base_salary / 52
	hourly_salary <- weekly_salary / 37.5

	wp$at_work_hr <- lubridate::hour(lubridate::as.period(wp$at_work, "hours"))
	wp$at_work_hr[wp$at_work_hr >= 8] <- wp$at_work_hr[wp$at_work_hr >= 8] - 0.5 
	
	# get expected full time salary during parental leave
	wp$at_work_hr[wp$week <= 52 & wp$at_work_hr == 0] <- 7.5 * 5 / 7

	# get weekly salary
	wps <- group_by(wp, week) %>%
		summarise(hr=pmin(37.5, sum(at_work_hr)))

	wps$income <- wps$hr * hourly_salary
	wps$year <- floor(as.period(weeks(wps$week)) / years(1))

	# Compound increase per year
	wps$income <- wps$income * (1+annual_increment)^wps$year
	return(wps)
}


statutory_parental_pay <- function(nt, option, rate=140.98)
{
	duration <- nrow(nt)
	# print(duration)
	mod <- parental_leave_salary_model(option, duration)
	# index <- as.numeric(mod$weeks %in% c(weeks_from_start:(weeks_from_start+time_on_leave)))
	index <- as.numeric(nt$on_leave != "None" & mod$spp)
	# print(index)
	mod$income <- as.numeric(mod$spp) * rate * index
	# print(mod)
	return(mod$income)
}


salary_loss <- function(salary1, salary2, nt, option)
{
	stopifnot(length(salary1) == length(salary2))
	stopifnot(length(salary1) == nrow(nt))
	mod <- parental_leave_salary_model(option, length(salary1))
	mod$income1 <- mod$drop * salary1 * as.numeric(nt$on_leave == "p1")
	mod$income2 <- mod$drop * salary2 * as.numeric(nt$on_leave == "p2")
	return(mod)
}


nursery_cost <- function(wp, hourly_rate)
{
	wp$nc <- wp$need_careh * hourly_rate
	wps <- group_by(wp, week) %>%
		summarise(nc=sum(nc))

	wps$year <- floor(as.period(weeks(wps$week)) / years(1))
	wps <- group_by(wps, year) %>%
		mutate(cum_nc = cumsum(nc))
	wps$nc_adj <- wps$nc
	wps$nc_adj[wps$cum_nc <= 10000] <- wps$nc_adj[wps$cum_nc <= 10000] * 0.8
	return(wps)
}




##############




salary_calc_deprecated <- function(base_salary, annual_increment, duration)
{
	nyears <- as.period(weeks(duration)) / years(1)
	sal <- dplyr::data_frame(
		week=1:duration,
		year=floor(as.period(weeks(week)) / years(1)),
		income=base_salary / (days(365)/weeks(1))
	)
	# Compound increase per year
	sal$income <- sal$income * (1+annual_increment)^sal$year
	return(sal$income)
}

salary_loss_deprecated <- function(salary, weeks_from_start, time_on_leave, duration, option)
{
	mod <- parental_leave_salary_model(option, length(salary))
	index <- as.numeric(1:duration %in% c(weeks_from_start:(weeks_from_start+time_on_leave)))
	mod$income <- mod$drop * salary * -1 * index
	return(mod$income)
}

statutory_parental_pay_deprecated <- function(rate=140.98, weeks_from_start, time_on_leave, option)
{
	duration <- time_on_leave + weeks_from_start
	# print(duration)
	mod <- parental_leave_salary_model(option)
	index <- as.numeric(mod$weeks %in% c(weeks_from_start:(weeks_from_start+time_on_leave)))
	# print(index)
	mod$income <- as.numeric(mod$spp) * rate * index
	# print(mod)
	return(mod$income)
}



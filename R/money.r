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


salary_calc <- function(base_salary, annual_increment, duration)
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


statutory_parental_pay <- function(rate=140.98, weeks_from_start, time_on_leave, option)
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


salary_loss <- function(salary, weeks_from_start, time_on_leave, duration, option)
{
	mod <- parental_leave_salary_model(option, length(salary))
	index <- as.numeric(1:duration %in% c(weeks_from_start:(weeks_from_start+time_on_leave)))
	mod$income <- mod$drop * salary * -1 * index
	return(mod$income)
}


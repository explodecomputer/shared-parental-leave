# Parental leave time and money planner

[Shared parental leave information](http://www.bristol.ac.uk/hr/policies/shared-parental-leave.html)

**Questions:**

1. What is the financial position for different working and nursery schedules?
2. How much time will be spent with the baby?
3. Can we use this to make decisions, or assign a 'value' to baby time?


**Scenarios:**

Parental leave possibilities:

- 0 months (No baby)
- 6 months 
    - one parent off 
- 9 months 
    - one parent off
    - 6/3 split
- 12 months 
    - one parent off
    - 6/6 split

Each followed by return to nursery:
- 5 days per week
- 4 days per week with either parent 80%
- 4 days per week with one parent compressed hours
- 3 days per week with both parents 80%
- 3 days per week with one parent compressed hours, other 80%

Calculate the timings for e.g. 3 years

## Code plan

Calculate the financial component (`money` module) and baby time component (`babytime` module) independently

# Money

Calculate a weekly balance by estimating all relevant assets and liabilities

## Assets

### `salary_calc`

Parameters 

- start rate
- increment percentage
- duration

return

- vector of weekly salary income

### `statutory_parental_pay`

Parameters

- rate
- duration

return vector_of_loss_of_salary


## liabilities

### salary_reduction
    - salary
    - spp_rate
    - from_start
    - duration

### nursery_cost
    daily_rate
Note that just put 80% discount for now


## real time module

- baby_awake_time
    - waking_time
    - sleeping_time
    - return vector of intervals

day 1 - 7am-7pm
day 2 - 7am-7pm
day 3 - 7am-7pm
day 4 - 7am-7pm


- work_time
    - start_time
    - end_time
    - start_date
    - days
    - duration

day 1 - 9am-5pm
day 2 - NULL
day 3 - NULL
day 4 - 9am-5pm


- time_with_baby
    - baby_awake_time
    - work_time

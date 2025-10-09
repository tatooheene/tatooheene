# Example, w

# Yearly discounting with annual 1 QALY per year

values = c(1, 1, 1, 1)            # 1 QALY per year
discount_rate = 0.03              # 3% annual discount rate
t_year <- seq_along(values)       # Time: 1, 2, 3, 4
discounted_values <- values * (1 + discount_rate)^(-t_year)
sum(discounted_values)            # ~3.7171


monthly_qaly <- 1 / 12            # 1 QALY/year = 1/12 per month
months <- 4 * 12                  # 48 months
t_month <- seq(1, 4, 1/12)        # 48 fractional years
values <- rep(monthly_qaly, months)
discounted_values_month <- values * (1 + discount_rate)^(-t_month)
sum(discounted_values_month)    # ~3.735


# Here we notice that we have
paste("disounted annually", round(sum(discounted_values),4), sep =" " )
paste("disounted montly", round(sum(discounted_values_month),4), sep =" " )

paste("difference ", (sum(discounted_values)   - sum(discounted_values_month)  ), sep =" " )


# Apprach
# sum


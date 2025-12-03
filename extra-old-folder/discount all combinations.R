

## Understanding time assumptions in discounting




Discounting depends on when outcomes are assumed to occur within each model cycle. The Dutch guideline recommends *not discounting the first cycle*, which implies a specific assumption about timing. The examples below illustrate how to set up the `times` vector based on different timing assumptions.

### Outcomes at the start of each cycle
When outcomes are rewarded at the **start** of each cycle, the first year is not discounted. Time points start at 0 (e.g., 0, 1, 2, 3, …).

```{r example-set-up-times-vector}
v_times <- seq(from = 0, to = length(v_c_SoC) - 1, by = 1)
```

### Outcomes at the end of each cycle
When outcomes are assumed to occur at the **end** of each cycle, discounting starts immediately. In this case, the `times` vector should start at 1 (e.g., 1, 2, 3 …).

```{r example-set-up-times-vector-discount-y1}
v_times_discount_y1 <- seq(from = 1, to = length(v_c_SoC), by = 1)
# or
v_times_discount_y1 <- seq_along(v_c_SoC)
```

### Outcomes half-way a cycle
In modelling studies, discrete time intervals are used to simulate events. However, the real processes occur in continuous time. In these simulation models, it is common to give the full cycle state reward at the beginning of the cycle, while transitions are assumed to occur at the end of each cycle. This could results in overestimation of the cycles rewards by about half of a cycle. On solution of this would be to make the cycle length shorter. Alternatively, cycle correction is often applied. This can be done by subtracting half a reward that is assigned in the first cycle and in half a reward in the last cycle. The `darthtools` package comes with a function called `gen_wcc` that can generate a vector of within-cycle corrections. For learning more about alternative methods of cycle correction we refer to:
  Elbasha EH, Chhatwal J. Myths and Misconceptions of Within-Cycle Correction: A Guide for Modelers and Decision Makers. Pharmacoeconomics. 2016 Jan;34(1):13-22. doi: 10.1007/s40273-015-0337-0. PMID: 26643402.


## Discounting in models with sub-annual cycle lengths

While the application of discounting is straightforward in models with annual cycles, it becomes more ambiguous in models that use sub-annual time steps (e.g., monthly or weekly cycles). The Dutch guideline does not specify how to handle the timing of discounting in such cases, leaving room for interpretation within the modeling community.

Different approaches have emerged: some modelers apply discounting starting after the first sub-annual cycle (e.g., excluding only the first month from discounting), as they link this to the fact that discounting relate to the fundamental principle that costs and benefits should be discounted beyond the present, and interpret every time point after the first cycle (present) as future. Others assume that no discounting occurs during the first full year, after which discounting is applied at intervals corresponding to the model’s cycle length. This lack of explicit guidance can lead to variation in results and underlines the importance of transparently reporting how discounting is implemented in sub-annual models.

### Why does this matter? Why does timing affects the present value?
Although yearly and monthly discounting approaches produce the same total undiscounted QALYs or costs, they differ in discounted values due to when outcomes are assumed to occur. Events that occur earlier are discounted less and therefore have a higher present value. So if we switch from annual to monthly cycles, we simulate that instead of receiving one full QALY at the end of each year, we receive 1/12 of a QALY each month. And because the QALYs in a sub-annual model are assigned slightly earlier in time, they lose a little less by discounting.

Therefore, under the assumption that rewards occur at the *end of each period* (meaning discounting from the start), the monthly model gives slightly higher present values. This can be seen in the example below:

  #### Example with discounting in the first year
  Below, demonstrate this concept using a hypothetical situation in which 1 QALY is awarded in a period of 4 years, the annual model. This gives a total of 4 undiscounted QALYs and 3.854385 discounted QALYs. Next, we demonstrate a model with monthly cycles, in which 1/12th QALY is awarded each cycle. This results in a total of 4 undiscounted QALYS, similar to the annual model. However, the discounted QALYs are 3.8808, which are 0.026427 QALYs more compared to the annual model.

```{r demo-difference}
# Annual model WITH discounting the first year
v_QALY <- rep(1, 4)       # 1 QALY per year
sum(v_QALY) # total undiscounted QALYs = 4
v_times_year <- seq_along(v_QALY) # Time: 1, 2, 3, 4
v_QALY_discounted <- apply_discounting(values = v_QALY, discount_rate = "effects", times = v_times_year)
pv_year_end <- sum(v_QALY_discounted)            # ~3.854385 WITH discounting first year
pv_year_end

# Monthly cycles WITH discounting the first year
v_QALYs_monthly <-  rep(1/12, 4/(1/12)) # 1 QALY per year
sum(v_QALYs_monthly) # total undiscounted QALYs = 4
v_times_month   <- seq_along(v_QALYs_monthly) * 1/12 # Time: 1/12, 2/12, 3/12, etc.until time is 4
v_QALY_month_discounted <- apply_discounting(values = v_QALYs_monthly, discount_rate = "effects", times = v_times_month)
pv_month_end    <- sum(v_QALY_month_discounted)        # ~3.8808 WITH discounting first year
pv_month_end
```

### A counter intuative change
More common is the assumption, to assume that reward occur *at the start* of each period. The first QALYs are assigned immediately at time = 0, and we begin discounting only form the second year. This results in a shirt from the time horizon. With the annual model, the time ranges from year 0 to year 3, meaning the last reward is assigned at time 3. While in a monthly model, the time ranges from 0 to last month of year 3, meaning that the last event occurs at almost year 4. So although in the monthly model, rewards occur earlier in time, they also extend one year further into the future. And this extra year of later events, offsets the early advantages, and results in a slight lower present value compared to the annual model. This is demonstrated in the example below:

  ```{r demo-difference-without-discounting-first-year}
# Annual model withOUT discounting the first year
v_times_year      <- seq(from = 0, to = length(v_QALY) - 1, by = 1 ) # -1 because we start at zero
v_QALY_discounted <- apply_discounting(values = v_QALY, discount_rate = "effects", times = v_times_year)
pv_year_start     <- sum(v_QALY_discounted)            # ~3.9122 withOUT discounting first year
pv_year_start

# Monthly cycles withOUT discounting the first year
v_times_month <- seq(from = 0, to = (length(v_QALYs_monthly)/12 - 1)  , by = 1/12) # Time: 1/12, 2/12, 3/12, etc.until time is 4

# -1 because we start at zero
v_times_month[1:12] <- 0

v_QALY_month_discounted <- apply_discounting(values = v_QALYs_monthly, discount_rate = "effects", times = v_times_month)
pv_month_start          <- sum(v_QALY_month_discounted)      # ~3.892422 withOUT discounting first year
pv_month_start
```



```{r}
# Collect results
df_compare <- data.frame(
  Model = rep(c("Annual", "Monthly"), times = 2),
  Timing_assumption   = rep(c("End-of-cycle (discount from start)", "Start-of-cycle (no discount in 1st yar"), each = 2),
  When_last_QALY_happends = c("Year 4", "Year 4", "Year 3", "Year 4") ,
  PV = round(c(pv_year_end, pv_month_end, pv_year_start, pv_month_start),3)
)

print(df_compare)

```



## What to remember from timing in discounting?
When discounting begins immediately, shorter cycles (monthly) give slightly higher PVs.
When the first period is undiscounted, shorter cycles extend the horizon and yield slightly lower PVs.


```{r image-to-examplain-timing, eval = TRUE, echo = FALSE}
# Improved timeline plot for vignette
# Paste this in an Rmd vignette or run in RStudio

# PARAMETERS
years <- 4
months_per_year <- 12
n_months <- years * months_per_year  # 48

# Create event times
annual_end   <- data.frame(facet = "Annual - End-of-cycle", t = 1:years)
monthly_end  <- data.frame(facet = "Monthly - End-of-cycle", t = seq(1/12, years, by = 1/12)) # length 48
annual_start <- data.frame(facet = "Annual - Start-of-cycle", t = 0:(years - 1))
monthly_start <- data.frame(facet = "Monthly - Start-of-cycle", t = seq(0, by = 1/12, length.out = n_months)) # 0, 1/12,...,47/12

# Combine
df_all <- rbind(annual_end, monthly_end, annual_start, monthly_start)

# Ensure factor order for neat vertical alignment
df_all$facet <- factor(df_all$facet, levels = c(
  "Annual - End-of-cycle",
  "Monthly - End-of-cycle",
  "Annual - Start-of-cycle",
  "Monthly - Start-of-cycle"
))

# Summary annotation for first and last events
ann <- df_all %>%
  group_by(facet) %>%
  summarise(t_first = min(t), t_last = max(t), n_points = n()) %>%
  ungroup()

# Plot
p <- ggplot(df_all, aes(x = t, y = facet)) +
  geom_segment(aes(x = 0, xend = years, y = facet, yend = facet),
               size = 0.35, linetype = "11", color = "grey60") +
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~ facet, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, years, 1), limits = c(0, years)) +
  labs(
    title = "Event timing: Annual vs Monthly (Start vs End-of-cycle)",
    x = "Time (years)",
    y = "",
    subtitle = "Dots = when each QALY (or event) is accrued within a 4-year horizon"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10)
  ) +
  geom_vline(xintercept = c(years - 1, years), linetype = "dashed", size = 0.3, color = "grey70") # lines at 3 and 4

# Add annotation labels: first, last and number of dots
p <- p +
  geom_text(
    data = ann,
    aes(x = t_first, y = facet, label = paste0("first: ", round(t_first, 2))),
    vjust = -1.2, size = 3
  ) +
  geom_text(
    data = ann,
    aes(x = t_last, y = facet, label = paste0("last: ", round(t_last, 2))),
    vjust = -1.2, size = 3
  ) +
  geom_text(
    data = ann,
    aes(x = 0.2, y = facet, label = paste0("n = ", n_points)),
    hjust = 0, vjust = 2.2, size = 3.2
  )

# Print plot
print(p)


```


## What to remember from timing in discounting for the Dutch guidelines?
Since the Dutch guidelines specifically state that the first year should not be discounted, the assumption should be made that rewards occur at the beginning of the cycle.

In order to NOT discount the first year, the `times` parameters should start with a zero in year 0 or needs to be zero of all months in the first year.
In general, the differences in results are small, and will not have a consequence for the final decision. However, reporting the assumption help to interpret the reported discounted value. And in most models, cycle-correction is applied any way, which gives a balanced approximation between both assumptions.




### Table with overview of the resuls
The overview below shows the undiscounted and discounted values of the different assumptions regarding time.
This is consistent with how we discussed the timing of event before.

```{r overview-discounted-values}
# Define the data
approaches          <- c("Annual-cycles and discounting", "Monthly cycles and discounting", "Monthly cycles, discounted annual")
undiscounted_values <- round(c(n_undiscounted_Q_SoC, n_undiscounted_Q_SoC_monthly, n_undiscounted_Q_SoC_monthly),3)
discounted_values   <- round(c(n_discounted_Q_SoC,   n_discounted_Q_SoC_monthly,   n_discounted_Q_SoC_monthly_annual_discount), 3)


# Create a data frame
results_table <- data.frame(
  Approach = approaches,
  Undiscounted_Value = undiscounted_values,
  Discounted_Value = discounted_values
)

# Print the table
print(results_table)

```

To end, we recommend users to carefully:
  - Decide when rewards occur (start, mid, end)
- Construct the corresponding times vector
- Report your assumptions
- Apply discounting consistently in line with the guidelines


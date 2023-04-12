# ### Data Dictionary
# 
# The data is from the London Fire Brigade. All variables are described in the table below. 
# 
# Variable | Description
# ------------- | -------------
# IncidentNumber| LFB Incident Number
# DateOfCall| Date of call
# CalYear| Year of call
# TimeOfCall| Time of call
# HourOfCall| Hour of call
# IncidentGroup| High level incident category
# StopCodeDescription| Detailed incident category
# SpecialServiceType| Further detail for special services incident categories
# PropertyCategory| High level property descriptor
# PropertyType| Detailed property descriptor
# AddressQualifier| Qualifies location of actual incident relevant to category above
# Postcode_full| Postcode
# Postcode_district| Postcode Districts
# UPRN| Unique Property Reference Number
# USRN| Unique Street Reference Number
# IncGeo_BoroughCode| Borough Code
# IncGeo_BoroughName| Borough Name
# ProperCase| Borough Name
# IncGeo_WardCode| Ward Code
# IncGeo_WardName| Ward Name
# IncGeo_WardNameNew| New Ward Name
# Easting_m| Easting
# Northing_m| Northing
# Easting_rounded| Easting rounded up to nearest 50
# Northing_rounded| Northing rounded up to nearest 50
# Latitude| Latitude
# Longitude| Longitude
# FRS| Fire Service ground
# IncidentStationGround| LFB Station ground
# FirstPumpArriving_AttendanceTime| First Pump attendance time in seconds
# FirstPumpArriving_DeployedFromStation| First Pump deployed from station
# SecondPumpArriving_AttendanceTime| Second Pump attendance time in seconds
# SecondPumpArriving_DeployedFromStation| Second Pump deployed from station
# NumStationsWithPumpsAttending| Number of stations with pumps in attendance
# NumPumpsAttending| Number of pumps in attendance
# PumpCount| Number of pumps available
# PumpHoursRoundUp| Time spent at incident by pumps, rounded up to nearest hour
# Notional Cost (£) | Cost of service
# NumCalls | Number of Calls
# 
# 
# 
# # Section 1
# 
# #### Source of the data: 
# London Fire Brigade
# 
# #### Purpose of the analysis: 
# A panel of Fire service managers and politicians want to gain insights on costs and response times associated with incidents during the time period in the data.
# 

# Adding the libraries we need
library(kableExtra)
library(emmeans)
library(gridExtra)
library(dplyr)
library(lubridate)
library(kableExtra)
library(tidyverse)
library(broom)
library(knitr)


# 
# 
# ### Reading and cleaning the data
# 
## ----data-cleaning-------------------------------------------------------------

#reading the data
fire_df <- read_csv("London_Fire_data.csv", guess_max = 10000)

#analysing the data by looking at the structure
str(fire_df)

#analysing the data by looking at the summary
summary(fire_df)

#renaming the column Notional Cost (£) to Cost
names(fire_df)[names(fire_df) == "Notional Cost (£)"] <- "Cost"

#changing DateOfCall from character to Date type
fire_df$DateOfCall <- dmy(fire_df$DateOfCall)

#changing IncidentGroup, StopCodeDescription, SpecialServiceType and PropertyCategory to factors
cols <- c("IncidentGroup", "StopCodeDescription", "SpecialServiceType", "PropertyCategory")
fire_df[cols] <- lapply(fire_df[cols], as.factor)



# 
# Now that our data has been cleaned, we can move on to answering the questions. 
# 
# NOTE: We are not removing any NA values from the original dataset, we will filter them out as and when required while performing calculations.
# 
# 
# ### The costs of responding to fires and false alarms
# 
# ##### The sum of all costs associated with responding to fires during the time period 
# 
## ----1a------------------------------------------------------------------------

#Filtering out NA values and "Fire" values and then adding Costs 
sumOfFireCosts <- fire_df %>% filter(!is.na(Cost)) %>% filter(IncidentGroup == "Fire") %>% summarise(sum_of_fire_costs = sum(Cost))

sumOfFireCosts


# 
# ##### The sum of all costs associated with responding to false alarms. 
# 
## ----1b------------------------------------------------------------------------

#Filtering out NA values and "False Alarm" values and then adding Costs 
sumOfFalseAlarmCosts <- fire_df %>% filter(IncidentGroup == "False Alarm") %>% filter(!is.na(Cost)) %>% summarise(sum_of_False_Alarm_costs = sum(Cost))

sumOfFalseAlarmCosts


# 
# ##### The average cost of responding to a fire, and of responding to a false alarm.
# 
## ----1c------------------------------------------------------------------------

#Finding avg. cost of "Fire" and "False Alarm"
avg_costs <- fire_df %>% group_by(IncidentGroup) %>% filter(IncidentGroup != "Special Service") %>% filter(!is.na(Cost)) %>% summarise(avg_cost = round(mean(Cost),2)) %>% as.data.frame()

avg_costs


# 
# ### The distribution of response times
# 
# ##### Response times to all incidents where there was indeed a response.
# 
## ----2a, fig.align='center'----------------------------------------------------

#Plotting a histogram to visualise distribution of response time
response_distribution <- fire_df %>% ggplot(aes(x = FirstPumpArriving_AttendanceTime)) + geom_histogram(color = "black", fill = "#0373fc", binwidth = 40, alpha = 0.7, na.rm = TRUE) + scale_x_continuous(limit = c(0,1200), 
                         breaks = c(0,200,400, 600, 800, 1000, 1200), 
                         labels = c("0s","200s","400s","600s", "800s","1000s", "1200s"))  + labs(title = "Distribution of Response time for incidents" , x ="Response time (seconds)", y = "Number of Responses")

response_distribution

# 
# ##### Compare the distribution of response times for the three different types of incidents.
# 
## ----2b,fig.align='center'-----------------------------------------------------

#Printing a summary table of min, max and mean values for all types of incidents
dist_details_incident_group <- fire_df %>% filter(!is.na(FirstPumpArriving_AttendanceTime)) %>% group_by(IncidentGroup) %>% summarise(min_val = min(FirstPumpArriving_AttendanceTime), max_val = max(FirstPumpArriving_AttendanceTime), mean_val = mean(FirstPumpArriving_AttendanceTime)) %>% as.data.frame()

dist_details_incident_group

#Plotting a histogram to visualise distribution of response time for Fire, Special Service, False Alarm

response_distribution_by_group <- fire_df %>% ggplot(aes(x = FirstPumpArriving_AttendanceTime)) + geom_histogram(color = "black", binwidth = 40, alpha = 0.7, na.rm = TRUE) + scale_x_continuous(limit = c(0,1200), breaks = c(0,200,400, 600, 800, 1000, 1200),labels = c("0s","200s","400s","600s", "800s","1000s", "1200s"))  + labs(title = "Distribution of Response time for incidents grouped by Incident Group" , x ="Response time (seconds)", y = "Number of Responses") + facet_grid(IncidentGroup~.) + aes(fill = IncidentGroup)

response_distribution_by_group 


# 
# ### Summary of special service response times
# 
# ##### Count of the number of each type of incident
# 
## ----3a, fig.align='center'----------------------------------------------------

#counting occurrence of each type of incident
count_of_incidents <- fire_df %>% filter(!is.na(SpecialServiceType)) %>% group_by(SpecialServiceType) %>% summarise(total_count =n())

count_of_incidents


#plotting a bar chart to display count categorised by incident type
(
  fire_df %>% filter(!is.na(SpecialServiceType)) %>% filter(!is.na(FirstPumpArriving_AttendanceTime)) %>% group_by(SpecialServiceType) %>%
    ggplot(aes(x = SpecialServiceType, fill = SpecialServiceType)) + geom_bar() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + labs(title = "Bar Chart of the different types of Special Services" , x = "Special Service Type", y = "Number of calls")
)


# 
# ##### The mean response time for each type of incident
# 
## ----3b------------------------------------------------------------------------

#calculating the avg response time for each incident type
mean_response_time_by_incident <- fire_df %>% filter(!is.na(SpecialServiceType)) %>% group_by(SpecialServiceType) %>% summarise_at(vars(FirstPumpArriving_AttendanceTime), list("mean_response_time_in_seconds" = mean), na.rm=TRUE) %>% as.data.frame()

mean_response_time_by_incident$mean_response_time_in_seconds <- round(mean_response_time_by_incident$mean_response_time_in_seconds, 2)

mean_response_time_by_incident

# 
# ##### The 10th percentile of response times for each type of incident
# 
## ----3c------------------------------------------------------------------------
#calculating 10th percentile of response times for each type of incident
quantiles_by_incident_10 <- fire_df %>% filter(!is.na(SpecialServiceType)) %>% group_by(SpecialServiceType) %>% summarise("10th_percentile" = quantile(FirstPumpArriving_AttendanceTime, probs = c(0.10), na.rm = TRUE)) %>% as.data.frame()

quantiles_by_incident_10


#             
# ##### The 90th percentile of response times for each type of incident
# 
## ----3d------------------------------------------------------------------------

#calculating 90th percentile of response times for each type of incident
quantiles_by_incident_90 <- fire_df %>% filter(!is.na(SpecialServiceType)) %>% group_by(SpecialServiceType) %>% 
  summarise("90th_percentile" = quantile(FirstPumpArriving_AttendanceTime, probs = c(0.90), na.rm = TRUE)) %>% as.data.frame()

quantiles_by_incident_90


# ### A t-test comparing Ealing and Greenwich
# 
# ##### T-test using Null Hypothesis Significance Testing
# 
## ----4a, fig.align='center'----------------------------------------------------

#T-test using Null Hypothesis Significance Testing

t.test(FirstPumpArriving_AttendanceTime~ProperCase, data = filter(fire_df, ProperCase %in% c("Ealing", "Greenwich")))

#Plotting the density curves for both Ealing and Greenwich
(
  mean_plot_ealing_greenwich <- fire_df %>% filter(ProperCase == "Ealing" | ProperCase == "Greenwich") %>% filter(!is.na(FirstPumpArriving_AttendanceTime)) %>% ggplot(aes(x = FirstPumpArriving_AttendanceTime, fill = ProperCase, alpha = 0.7)) + geom_density() + geom_vline(colour = "#000000", linetype = "dashed", aes(xintercept = mean(FirstPumpArriving_AttendanceTime,  na.rm = T))) + facet_wrap(~ProperCase) + theme(legend.position="none") + labs(x = "Response Time (seconds)", title = "Comparing respone time distribution for Ealing vs Greenwich", subtitle = "The dashed line running vertically for each facet is the mean repsonse time for that group") + scale_x_continuous(breaks = c(0,200,400, 600, 800, 1000, 1200),labels = c("0s","200s","400s","600s", "800s","1000s", "1200s")) 
)


# 
# ##### T-test using Estimation
# 
## ----4b, fig.align='center'----------------------------------------------------
emm_options(opt.digits = FALSE)

#Comparing means by using estimation approach 
response_time_estimation <- lm(FirstPumpArriving_AttendanceTime~ProperCase, data = filter(fire_df, ProperCase %in% c("Ealing", "Greenwich")))

response_time_emm <- emmeans(response_time_estimation, ~ProperCase)

response_time_emm


#using confint function to analyse confidence intervals
(ealing_greenwich_contrast <- confint(pairs(response_time_emm)))


#plotting the results of estimation approach
grid.arrange ( 
  estimation_plot <- ggplot(summary(response_time_emm), aes(x=ProperCase, y=emmean, ymin=lower.CL, ymax=upper.CL)) + 
		geom_point(aes(colour = ProperCase)) + geom_linerange(aes(colour = ProperCase)) + labs(y="Response Time (seconds)", x="Location", title="Average Response Time (sec)") + theme(legend.position = "None"), 
  contrast_plot <- ggplot(ealing_greenwich_contrast, aes(x = contrast, y = estimate , ymin = lower.CL, ymax = upper.CL )) + geom_point() + geom_linerange() + labs(y = "Difference in Response time (seconds)", x = "Contrast", title = "Difference in Response Time (sec)") + geom_hline(yintercept = 0, lty = 2), ncol = 2
)

# 
# ---
# 
# # Section 2
# 
# ### Costs of responding to fires and false alarms
# 
# ##### What is the sum of all costs associated with responding to fires? 
# 
# From the data available to us, we sum up all costs associated with responding to fires, excluding any empty Cost records. The sum of all fire related responses is £43,059,576. 
# 
## ----echo = FALSE--------------------------------------------------------------
colnames(sumOfFireCosts)[1] <- "Cost of responding to Fires in £"
(sumOfFireCosts %>% kbl(caption = "Table 1. Cost of Responding to Fires") %>% kable_styling(full_width  = F))

# 
# ##### What is the sum of all costs associated with responding to false alarms.
# 
## ----echo = FALSE--------------------------------------------------------------
colnames(sumOfFalseAlarmCosts)[1] <- "Cost of responding to False Alarms in £"
(sumOfFalseAlarmCosts %>% kbl(caption = "Table 2. Cost of Responding to Fires") %>% kable_styling(full_width = F))

# 
# Sometimes, calls made to the London Fire Brigade turn out to be false alarms. These could be calls from people who are genuinely concerned about something they thought was a fire, or from people with bad intentions, like playing a prank. The costs associated with responding to all types of false alarms is £61,249,812. This makes up 40.5% of the total costs. 
# 
# 
# 
# ##### What is the average cost of responding to a fire, and of responding to a false alarm?
# 
## ----echo = FALSE--------------------------------------------------------------

names(avg_costs)[names(avg_costs) == "IncidentGroup"] <- "Incident Group"
names(avg_costs)[names(avg_costs) == "avg_cost"] <- "Average Cost in £"
avg_costs$`Average Cost in £` <- round(avg_costs$`Average Cost in £`, 2)
(avg_costs %>% kbl(caption = "Table 3. Average Cost of Responding to Fire vs False Alarm") %>% kable_styling(full_width = F))



# The average cost of responding to a fire is £837.98 and the average cost of responding to a false alarm is £378.38.
# 
# ### Distribution of response times
# 
# ##### Visualisation of response time to all incidents where there was indeed a response.
# 
## ----echo = FALSE, fig.align='center'------------------------------------------

(
  fire_df %>% filter(!is.na(FirstPumpArriving_AttendanceTime)) %>% 
  summarise("Min. Time Taken to Respond (seconds)" = min(FirstPumpArriving_AttendanceTime),"Max. Time Taken to Respond (seconds)" = max(FirstPumpArriving_AttendanceTime), "Avg. Time Taken to Respond (seconds)" = mean(FirstPumpArriving_AttendanceTime)) %>%
  kbl(caption = "Table 4. Response Time Statistics") %>% kable_styling(full_width = F)
  )

response_distribution


# 
# On analysing the time taken to respond, we see that the least amount of time taken is 1 second and the maximum amount of time taken is 1199 seconds, which is approximately 20 minutes. However, on average it takes around 5 minutes to respond to a call. This data is in accordance with the graph, with a big chunk of responses taking between 200-400 seconds. 
# 
# ##### Comparing the distribution of response times for the different types of incidents
# 
# We have 3 high-level incident categories - Fire, False Alarms and Special Services. We are comparing the response times for the different types
# 
## ----echo = FALSE, fig.align='center'------------------------------------------
colnames(dist_details_incident_group)[1] <- "Incident Group"
colnames(dist_details_incident_group)[2] <- "Min. Response Time (seconds)"
colnames(dist_details_incident_group)[3] <- "Max. Response Time (seconds)"
colnames(dist_details_incident_group)[4] <- "Avg. Response Time (seconds)"
temp <- fire_df %>% group_by(IncidentGroup) %>% filter(!is.na(IncidentGroup)) %>% count()
dist_details_incident_group$`Total Count` <- temp$n

(dist_details_incident_group %>% kbl(caption = "Table 5. Response Time Statistics by Group ") %>% kable_styling(full_width = F))

response_distribution_by_group


# 
# We can see that all of them have the same minimum response time. The maximum response time, although different, is approximately 20 minutes for all groups. However, the average time taken to respond is different for the Incident Groups On average, the response time is fastest for false alarms, followed by special services and then fires. We can also see from the graph and table that there are more calls for false alarms than other groups In fact, fires have the least count of all the types. 
# 
# ### Summary of Special Service Type response times
# 
# Following is a summary of data based on the detailed incident categories. 
# 
# ##### Count of the number of each type of incident
# 
## ----echo = FALSE, fig.align='center'------------------------------------------
colnames(count_of_incidents)[1] <- "Special Service Type"
colnames(count_of_incidents)[2] <- "Count"      
(count_of_incidents %>% kbl(caption = "Table 6. Count of Special Service Types") %>% kable_styling(full_width = F))

(
  fire_df %>% filter(!is.na(SpecialServiceType)) %>% filter(!is.na(FirstPumpArriving_AttendanceTime)) %>% group_by(SpecialServiceType) %>%
    ggplot(aes(x = SpecialServiceType, fill = SpecialServiceType)) + geom_bar() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + labs(title = "Bar Chart of the different types of Special Services" , x = "Special Service Type", y = "Number of calls")
)


# 
# Here, we can see that Effecting Entry/Exit has the highest count of all Special Service types. The 4 most common Special Service types are Effecting Entry/Exit, Flooding, Lift Release, and RTC (which stands for Road Traffic Collision.) In contrast, there has only been one call for Water Provision.
# 
# ##### Mean response time for each type of incident
# 
## ----echo = FALSE--------------------------------------------------------------

colnames(mean_response_time_by_incident)[1] <- "Special Service Type"
colnames(mean_response_time_by_incident)[2] <- "Avg. Response Time (seconds)"
(mean_response_time_by_incident %>% kbl(caption = "Table 7. Mean Response Time for Special Service types") %>% kable_styling(full_width = F))



# On average, most services take 308.47 seconds, approximately 5 minutes, for responding to a Special Service. From the table above, we can see that Spills and Leaks take the most time for a response and Removal of Objects from people requires the least amount of time for a response. 
# 
# 
# ##### 10th percentile of response times for each type of incident
# 
## ----echo = FALSE--------------------------------------------------------------
colnames(quantiles_by_incident_10)[1] <- "Special Service Type"
colnames(quantiles_by_incident_10)[2] <- "10th Percentiles"
(quantiles_by_incident_10 %>% kbl(caption = "Table 8. 10th Percentile Response Times by Special Service Type") %>% kable_styling(full_width = F))


# When we say something is the nth percentile, it means that there are n% of values lesser than this cutoff point. In this scenario, we have calculated the 10th percentile of response time for each of the Special Service Types. This means that for each of these values in the table above, 10% of all response times are lesser and 90% of response time values are greater. 
# 
# ##### 90th percentile of response times for each type of incident
# 
## ----echo = FALSE--------------------------------------------------------------

colnames(quantiles_by_incident_90)[1] <- "Special Service Type"
colnames(quantiles_by_incident_90)[2] <- "90th Percentiles"
(quantiles_by_incident_90 %>% kbl(caption = "Table 9. 90th Percentile Response Times by Special Service Type") %>% kable_styling(full_width = F))


# 
# Building on what was discussed previously, these values represent the 90th percentile of response time. This means that there are only 10% of values that are greater and 90% of all values lower than each of these values in the table. 
# 
# ### A t-test comparing Ealing and Greenwich
# 
# A t-test is a statistical test used to check if there is a significant difference between the means of two variables. 
# 
# ##### Null Hypothesis Significance Testing
# 
## ----echo = FALSE, fig.align='center'------------------------------------------

t.test(FirstPumpArriving_AttendanceTime~ProperCase, data = filter(fire_df, ProperCase %in% c("Ealing", "Greenwich")))

mean_plot_ealing_greenwich


# 
# We obtain the mean response time in Ealing as 316.93 seconds and the mean response time in Greenwich as 311.38 seconds. On analysing the confidence interval, we can conclude that 95% of the time, the difference in the average response times in Ealing and Greenwich will fall between [1.74, 9.37]. Most importantly, we can see the low probability (p-value) that implies that the difference in their average response time being zero is highly unlikely. Hence, we can reject the null hypothesis.
# 
# 
# ##### Estimation approach
# 
# The estimation approach is a more modern approach in estimating values of a large sample based on a smaller sample of the data. This method does not depend on the sample size and is hence preferred over the Null Hypothesis Significance Testing. 
# 

emm_options(opt.digits = FALSE)

response_time_emm %>% kbl(caption = "Table 10. Results of Estimation" ) %>% kable_styling(full_width = F)

ealing_greenwich_contrast %>% kbl(caption = "Table 11. Confidence Interval Results" ) %>% kable_styling(full_width = F)

grid.arrange(estimation_plot, contrast_plot, ncol = 2)



# 
# By analysing the results of the estimation approach, we can see the mean value for Ealing is 316.93 seconds and Greenwich is 311.38 seconds. Additionally, the chart on the left displays the range of the mean response time that can be expected for each location, and the chart on the right displays the range of difference in response time for the two locations. 
# 
# We can estimate that the difference in mean response time between Ealing and Greenwich is 5.55 and be 95% confident that the true population mean lies between [1.73, 9.37].


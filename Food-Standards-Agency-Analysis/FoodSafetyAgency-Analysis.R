## ----libraries, message = F, fig.align = 'center'-----------------------------------------------------------------------

library(tidyverse)
library(sjPlot)
library(patchwork)
library(car)
library(ggplot2)
library(cowplot)
library(dplyr)
library(scales)
library(readr)
library(emmeans) 
library(grid)
library(Hmisc)
library(gridExtra)
library(ggpubr)
library(broom)
library(kableExtra)


# 
# # Question 1: Food Standards Agency
# 
# ## Section 1
# 
# This section of the report addresses the following for politicians and the management team of the Food Standards Agency:
# 
# 1. Distribution of the percentage of enforcement actions successfully achieved across the Local Authorities.
# 2. Does employing more professional enforcement officers increase the likelihood of establishments successfully responding to enforcement actions?
# 
# ### Data Dictionary
# 
# | **Original Name** | **Description** |
# |---|---|
# | Country | Country Name |
# | LAType | Local Authority Type |
# | LAName | Local Authority Name |
# | Totalestablishments.includingnotyetrated.outside. | Total Number of Establishments |
# | Establishmentsnotyetratedforintervention | Number of Unrated Establishments |
# | Establishmentsoutsidetheprogramme | Number of Establishments outside the Programme |
# | Total.ofBroadlyCompliantestablishmentsratedA.E | Percentage of Establishments that are "Broadly Compliant" and do not require any intervention action |
# | Total.ofBroadlyCompliantestablishments.includingnotyetrated. | Percentage of Unrated Establishments that are "Broadly Compliant" and do not require any intervention action |
# | Aratedestablishments | Number of Establishments with Rating A |
# | Total.ofBroadlyCompliantestablishments.A | Percentage of A Rated Establishments that are "Broadly Compliant" |
# | Bratedestablishments | Number of Establishments with Rating B |
# | Total.ofBroadlyCompliantestablishments.B | Percentage of B Rated Establishments that are "Broadly Compliant" |
# | Cratedestablishments | Number of Establishments with Rating C |
# | Total.ofBroadlyCompliantestablishments.C | Percentage of C Rated Establishments that are "Broadly Compliant" |
# | Dratedestablishments | Number of Establishments with Rating D |
# | Total.ofBroadlyCompliantestablishments.D | Percentage of D Rated Establishments that are "Broadly Compliant" |
# | Eratedestablishments | Number of Establishments with Rating E |
# | Total.ofBroadlyCompliantestablishments.E | Percentage of E Rated Establishments that are "Broadly Compliant" |
# | Total.ofInterventionsachieved.premisesratedA.E. | Percentage of Interventions achieved for all Establishments rated A to E |
# | Total.ofInterventionsachieved.premisesratedA | Percentage of Interventions achieved for Establishments rated A |
# | Total.ofInterventionsachieved.premisesratedB | Percentage of Interventions achieved for Establishments rated B |
# | Total.ofInterventionsachieved.premisesratedC | Percentage of Interventions achieved for Establishments rated C |
# | Total.ofInterventionsachieved.premisesratedD | Percentage of Interventions achieved for Establishments rated D |
# | Total.ofInterventionsachieved.premisesratedE | Percentage of Interventions achieved for Establishments rated E |
# | Total.ofInterventionsachieved.premisesnotyetrated | Percentage of Interventions achieved for unrated establishments |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Voluntaryclosure | Number of Establishments that are subject to Formal Action - Voluntary Closure |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Seizure.detention.surrenderoffood | Number of Establishments that are subject to Formal Action - Seizure, Detention or Surrender of Food |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Suspension.revocationofapprovalorlicence | Number of Establishments that are subject to Formal Action - Suspension, Revocation of Approval or License |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Hygieneemergencyprohibitionnotice | Number of Establishments that are subject to Formal Action - Hygiene Emergency Prohibition Notice |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Prohibitionorder | Number of Establishments that are subject to Formal Action - Prohibition Order |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Simplecaution | Number of Establishments that are subject to Formal Action - Simple Caution |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Hygieneimprovementnotices | Number of Establishments that are subject to Formal Action - Hygiene Notice |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Remedialaction.detentionnotices | Number of Establishments that are subject to Formal Action - Detention Notice |
# | TotalnumberofestablishmentssubjecttoWrittenwarnings | Number of Establishments that are subject to Formal Action - Written Warning |
# | Totalnumberofestablishmentssubjecttoformalenforcementactions.Prosecutionsconcluded | Number of Establishments where Prosecution has been completed |
# | ProfessionalFullTimeEquivalentPosts.occupied.. | Percentage of Profession Full Time Equivalent Posts occupied |
# 
## ----q1-read-data, message = F, fig.align = 'center'--------------------------------------------------------------------

# Reading the data
fda_data <- read.csv("2019-20-enforcement-data-food-hygiene.csv", stringsAsFactors = T)


# 
# ### Data Cleaning
# 
## ----q1-renaming-columns, message = F, fig.align = 'center'-------------------------------------------------------------

fda_data <- fda_data %>% rename("Total_Establishments" = "Totalestablishments.includingnotyetrated.outside.",
                    "Establishments_outside_programme" = "Establishmentsoutsidetheprogramme" ,
                     "Establishments_unrated_for_Intervention" = "Establishmentsnotyetratedforintervention",
                    "A_Rated_Establishments" = "Aratedestablishments",
                    "B_Rated_Establishments" = "Bratedestablishments" ,
                    "C_Rated_Establishments" = "Cratedestablishments",
                    "D_Rated_Establishments" = "Dratedestablishments",
                    "E_Rated_Establishments" = "Eratedestablishments",
                     "Percentage_Unrated_Broadly_Compliant_establishments" = "Total.ofBroadlyCompliantestablishments.includingnotyetrated.",
                    "Percentage_Rated_Broadly_Compliant_establishments" = "Total.ofBroadlyCompliantestablishmentsratedA.E",
                    "Percentage_A_Rated_Compliant_Establishments" = "Total.ofBroadlyCompliantestablishments.A",
                    "Percentage_B_Rated_Compliant_Establishments" = "Total.ofBroadlyCompliantestablishments.B",
                    "Percentage_C_Rated_Compliant_Establishments" = "Total.ofBroadlyCompliantestablishments.C",
                    "Percentage_D_Rated_Compliant_Establishments" = "Total.ofBroadlyCompliantestablishments.D",
                    "Percentage_E_Rated_Compliant_Establishments" = "Total.ofBroadlyCompliantestablishments.E",
                    "Percentage_Interventions_A_to_E_Rated" = "Total.ofInterventionsachieved.premisesratedA.E.",                    
                    "Percentage_Interventions_A_premises" = "Total.ofInterventionsachieved.premisesratedA",
                    "Percentage_Interventions_B_premises" = "Total.ofInterventionsachieved.premisesratedB",
                    "Percentage_Interventions_C_premises" = "Total.ofInterventionsachieved.premisesratedC",
                    "Percentage_Interventions_D_premises" = "Total.ofInterventionsachieved.premisesratedD",
                    "Percentage_Interventions_E_premises" = "Total.ofInterventionsachieved.premisesratedE",
                    "Percentage_Interventions_Unrated_premises" = "Total.ofInterventionsachieved.premisesnotyetrated",
                    "Establishments_subject_to_Written_Warning" = "TotalnumberofestablishmentssubjecttoWrittenwarnings",
                    "Establishments_subject_to_Simple_Caution" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Simplecaution",
                    "Establishments_subject_to_Prohibition_Order" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Prohibitionorder",
                    "Establishments_subject_to_Hygiene_Notice" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Hygieneimprovementnotices",
                    "Establishments_subject_to_Hygiene_Emergency_Prohibition" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Hygieneemergencyprohibitionnotice",
                    "Establishments_subject_to_Voluntary_Closure" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Voluntaryclosure",
                    "Establishments_subject_to_Seizure_Detention_Surrender" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Seizure.detention.surrenderoffood",
                    "Establishments_subject_to_Suspension_License_Revocation" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Suspension.revocationofapprovalorlicence",
                    "Establishments_subject_to_Detention_Notice" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Remedialaction.detentionnotices",
                    "Establishments_Prosecution_concluded" = "Totalnumberofestablishmentssubjecttoformalenforcementactions.Prosecutionsconcluded",
                    "FTE_Posts_Occupied" = "ProfessionalFullTimeEquivalentPosts.occupied.."
                    )

# Removing duplicate elements
fda_data <- distinct(fda_data)

# Structure and summary of the data
# str(fda_data)

# Removing commas and changing class to integer
fda_data$Establishments_subject_to_Written_Warning <- as.integer(gsub(",","",fda_data$Establishments_subject_to_Written_Warning))

# Removing any characters that are not alphabets and changing class to character
fda_data$LAName <- as.character(fda_data$LAName) %>%
  {gsub("[[:punct:]]","",.)} %>%
  {gsub("[[:digit:]]","",.)} %>%
  {gsub("  "," ",.)}

# NR values in Percentage_Interventions_A_premises means No Interventions Due or Reported, so we are changing them and other empty values to NA
fda_data$Percentage_Interventions_A_premises[as.character(fda_data$Percentage_Interventions_A_premises) == "NR" | as.character(fda_data$Percentage_Interventions_A_premises) == ""] <- NA

# Changing class to numeric
fda_data$Percentage_Interventions_A_premises <- as.numeric(as.character(fda_data$Percentage_Interventions_A_premises))

# NP means 'No Premises given at this risk rating', so we are changing any NP values to 0
fda_data$Percentage_A_Rated_Compliant_Establishments[as.character(fda_data$Percentage_A_Rated_Compliant_Establishments) == "NP"] <- 0
#Changing empty values to NA
fda_data$Percentage_A_Rated_Compliant_Establishments[as.character(fda_data$Percentage_A_Rated_Compliant_Establishments) == ""] <- NA

# Changing class to numeric
fda_data$Percentage_A_Rated_Compliant_Establishments <- as.numeric(as.character(fda_data$Percentage_A_Rated_Compliant_Establishments))

# Summary of the data
# summary(fda_data)


# 
# ### Summary and visualisation of Categorical Variables
# 
## ----q1-categorical-variables, message = F, fig.align = 'center'--------------------------------------------------------

ggarrange(
  ggplot(fda_data) + 
  geom_bar(aes(x=reorder(LAType,LAType, function(x)+length(x)), fill = LAType), colour = "black") + 
  coord_flip() + 
  labs(y = "Frequency", x = "Local Authority Type", title = "Frequency of Local Authority Types") + 
  theme(legend.position ="NA"),

ggplot(fda_data) + 
  geom_bar(aes(x=reorder(Country,Country, function(x)+length(x)), fill = Country), colour = "black") + 
  coord_flip() + 
  labs(y = "Frequency", x = "Country", title = "Frequency of Country") + 
  theme(legend.position ="NA"),
ncol = 1,
align = "hv"
) %>% annotate_figure(bottom = textGrob("\nFigure 1.1: Visualising Categorical Variables", gp=gpar(fontsize=10,font=3)))
  


# 
# ### Summary and visualisation of Continuous Variables
# 
## ----q1-continuous-variables, message = F, fig.align = 'center', fig.height = 8, fig.width = 8--------------------------

total.establishments <- ggplot(na.omit(fda_data)) + 
  geom_histogram(bins = 70, aes(x = Total_Establishments), fill = "#5ed7ff", colour = "black") +
  geom_vline(xintercept = mean(fda_data$Total_Establishments, na.rm = T), linetype = "longdash", colour = "red") +
  labs(x = "Number of Establishments", y = "Frequency", title = "Distribution of Number of Establishments")
  
unrated.establishments <- ggplot(na.omit(fda_data)) + 
  geom_histogram(bins = 70, aes(x = Establishments_unrated_for_Intervention), fill = "#5ed7ff", colour = "black") +
  geom_vline(xintercept = mean(fda_data$Establishments_unrated_for_Intervention, na.rm = T), linetype = "longdash", colour = "red") +
  labs(x = "Number of Unrated Establishments", y = "Frequency", title = "Distribution of Number of Unrated Establishments")

outside.prog.establishments <- ggplot(na.omit(fda_data)) + 
  geom_histogram(bins = 70, aes(x = Establishments_outside_programme), fill = "#5ed7ff", colour = "black", na.rm = T) +
  geom_vline(xintercept = mean(fda_data$Establishments_outside_programme, na.rm = T), linetype = "longdash", colour = "red") +
  labs(x = "Number of Establishments outside the Programme", y = "Frequency", title = "Distribution of Number Establishments outside the Programme")

ggarrange(total.establishments,
          unrated.establishments,
          outside.prog.establishments,
          ncol = 1,
          align = "hv",
          heights = c(2,1.8,2)) %>% annotate_figure(bottom = textGrob("\nThe red lines represent the mean value\nFigure 1.2: Distribution of Establishments", gp=gpar(fontsize=10,font=3)))


# 
## ----q1-continuous-variables2, message = F, fig.align = 'center'--------------------------------------------------------

grid.arrange(
ggplot(na.omit(fda_data)) + 
  geom_histogram(bins = 70, aes(x = Percentage_Rated_Broadly_Compliant_establishments), fill = "#5ed7ff", colour = "black", na.rm = T) +
  geom_vline(xintercept = mean(fda_data$Percentage_Rated_Broadly_Compliant_establishments, na.rm = T), linetype = "longdash", colour = "red") +
  labs(x = "Percentage of Broadly Compliant Rated Establishments", y = "Frequency", title = "Distribution of Broadly Compliant Rated Establishments"),

ggplot(na.omit(fda_data)) + 
  geom_histogram(bins = 70, aes(x = Percentage_Unrated_Broadly_Compliant_establishments), fill = "#5ed7ff", colour = "black", na.rm = T) +
  geom_vline(xintercept = mean(fda_data$Percentage_Unrated_Broadly_Compliant_establishments, na.rm = T), linetype = "longdash", colour = "red") +
  labs(x = "Percentage of Broadly Compliant Unrated Establishments", y = "Frequency", title = "Distribution of Broadly Compliant Unrated Establishments"),
bottom = textGrob("\nThe red lines represent the mean value\nFigure 1.3: Distribution of Broadly Compliant Establishments", gp=gpar(fontsize=10,font=3))
)



# 
# ### Distribution of the percentage of enforcement actions successfully achieved across the Local Authorities
# 
## ----q1-distribution-of-enforcement-actions, message = F, fig.align = 'center', fig.height = 9, fig.width = 9-----------

interventions.histogram <- ggarrange(ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_A_to_E_Rated)) +
  geom_histogram(fill = "lightgreen", bins = 70, colour = "black") +
  geom_vline(xintercept = mean(na.omit(fda_data)$Percentage_Interventions_A_to_E_Rated), linetype = "longdash", colour = "red") +
  labs(x ="% of Enforcement Actions Achieved", y = "Frequency", title = "A-E rated Establishments"))

interventions.a.rated.histogram <- ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_A_premises)) + 
  geom_histogram(fill = "lightgreen", bins = 70, colour = "black") +
  geom_vline(xintercept = mean(na.omit(fda_data)$Percentage_Interventions_A_premises), linetype = "longdash", colour = "red") +
  labs(x ="% of Enforcement Actions Achieved", y = "Frequency", title = "A rated Establishments")

interventions.b.rated.histogram <- ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_B_premises)) + 
  geom_histogram(fill = "lightgreen", bins = 70, colour = "black") +
  geom_vline(xintercept = mean(na.omit(fda_data)$Percentage_Interventions_B_premises), linetype = "longdash", colour = "red") +
  labs(x ="% of Enforcement Actions Achieved", y = "Frequency", title = "B rated Establishments ")

interventions.c.rated.histogram <- ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_C_premises)) + 
  geom_histogram(fill = "lightgreen", bins = 70, colour = "black") +
  geom_vline(xintercept = mean(na.omit(fda_data)$Percentage_Interventions_C_premises), linetype = "longdash", colour = "red") +
  labs(x ="% of Enforcement Actions Achieved", y = "Frequency", title = "C rated Establishments")

interventions.d.rated.histogram <- ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_D_premises)) + 
  geom_histogram(fill = "lightgreen", bins = 70, colour = "black") +
  geom_vline(xintercept = mean(na.omit(fda_data)$Percentage_Interventions_D_premises), linetype = "longdash", colour = "red") +
  labs(x ="% of Enforcement Actions Achieved", y = "Frequency", title = "D rated Establishments ")

interventions.e.rated.histogram <- ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_E_premises)) + 
  geom_histogram(fill = "lightgreen", bins = 70, colour = "black") +
  geom_vline(xintercept = mean(na.omit(fda_data)$Percentage_Interventions_E_premises), linetype = "longdash", colour = "red") +
  labs(x ="% of Enforcement Actions Achieved", y = "Frequency", title = "E rated Establishments")

grid.arrange(interventions.histogram,
          interventions.a.rated.histogram,
          interventions.b.rated.histogram,
          interventions.c.rated.histogram, 
          interventions.d.rated.histogram,
          interventions.e.rated.histogram,
          top = textGrob("Distribution of Percentage of Enforcement Actions Successfully Achieved in Rated Establishments"),
          bottom = textGrob("\nThe red lines represent the mean values\nFigure 1.4: Distribution of Percentage of Enforcement Actions Successfully Achieved", gp=gpar(fontsize=10,font=3)),
          layout_matrix = rbind(1,c(2,3),c(4,5),6)
          )


  

# 
# ###  Relationship between proportion of successful responses and the number of Professional FTE Officers
# 
## ----q1-professional-FTE, message = F, fig.align = 'center', fig.align = 'center'---------------------------------------

interventions.by.fte <- lm(Percentage_Interventions_A_to_E_Rated~FTE_Posts_Occupied, na.omit(fda_data))
summary(interventions.by.fte)
emmeans(interventions.by.fte, ~FTE_Posts_Occupied)
cbind(coefficients = coef(interventions.by.fte), confint(interventions.by.fte))

# We can see that there is no significant effect of FTE posts occupied on the Percentage of Interventions

ggarrange(
  ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_A_to_E_Rated, y = FTE_Posts_Occupied)) + 
  geom_point(colour = "maroon", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Proportion of Successful Responses for A-E Rated Establishments", y = "Number of Professional FTE Officers", title = "Relationship between Proportion of Successful Responses\nand the Number of Professional FTE Officers") 
  ) %>% annotate_figure(bottom = textGrob("\nFigure 1.5: Relationship between Proportion of Successful Responses and Number of Professional FTE Officers", gp=gpar(fontsize=10,font=3)))


# 
# ###  Relationship between proportion of successful responses and the number of Professional FTE Officers as a proportion of the number of establishments
# 
## ----q1-professional-FTE-proportion, message = F, fig.align = 'center'--------------------------------------------------

# As we are only considering the rated establishments, we will fist calculate the number of rated establishments

fda_data$Total_Rated_Establishments <- fda_data$Total_Establishments-(fda_data$Establishments_outside_programme + fda_data$Establishments_unrated_for_Intervention) 

# The proportion was calculated by dividing FTE employee count by Total Rated Establishments

fda_data$Proportion_of_FTE_by_Establishments <- (fda_data$FTE_Posts_Occupied/fda_data$Total_Rated_Establishments)*100

interventions.by.fte.establishments <- lm(Percentage_Interventions_A_to_E_Rated~Proportion_of_FTE_by_Establishments, na.omit(fda_data))

summary(interventions.by.fte.establishments)
emmeans(interventions.by.fte.establishments, ~Proportion_of_FTE_by_Establishments)
cbind(coefficients = coef(interventions.by.fte.establishments), confint(interventions.by.fte.establishments))

anova(interventions.by.fte.establishments)

ggarrange(
  ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_A_to_E_Rated, y = Proportion_of_FTE_by_Establishments)) + 
  geom_point(colour = "red", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Proportion of Successful Responses for A-E Rated Establishments", y = "Proportion of Professional FTE Officers", title = "Relationship between Proportion of Successful Responses\nand the Proportion of Professional FTE Officers per Establishment")) %>% 
  annotate_figure(bottom = textGrob("\nFigure 1.6: Relationship between Proportion of Successful Responses and\nProportion of Professional FTE Officers per Establishment", gp=gpar(fontsize=10,font=3)))


# 
# ## Section 2
# 
# This report presents the results of the analyses that addresses the questions raised by politicians and the management team of the Food Standards Agency. The data had a small number of null values. Furthermore, as the names of the columns were too long, we have changed it to a more understandable form. There were several "NR" values, which represents 'No Interventions Due or Reported", were changed to null values. "NP" values, which represents "No Premises given at the time of Risk Rating", were changed to 0. It is to be noted that almost all columns exhibit skewed data.
# 
## ----q1-distribution-section2, message = F, fig.align = 'center', fig.height = 9, fig.width = 9, echo = F---------------


grid.arrange(interventions.histogram,
          interventions.a.rated.histogram,
          interventions.b.rated.histogram,
          interventions.c.rated.histogram, 
          interventions.d.rated.histogram,
          interventions.e.rated.histogram,
          top = textGrob("Distribution of Percentage of Enforcement Actions Successfully Achieved in Rated Establishments"),
          bottom = textGrob("\nThe red lines represent the mean values\nFigure 1.7: Distribution of Percentage of Enforcement Actions Successfully Achieved", gp=gpar(fontsize=10,font=3)),
          layout_matrix = rbind(1,c(2,3),c(4,5),6)
          )


# 
# From the visualisation, we can see that on average, 86.62% of enforcement actions have been successfully achieved while considering all premises rated A-E together. When we drill down further into individual premises, we can see that A-rated premises have an average percentage of successful enforcement actions of 98.08%, which is the highest average of all rated premises. This is followed by B-rated premises with an average of 95.25%, C-rated premises with an average of 91.84%, D-rated premises with an average of 86.32% and E-rated premises with an average of 77.37%. It is to be noted that all the distributions above are left-skewed. We also see some outliers with very low rates of successful enforcement actions, though these values are just due to the natural variability in the data. 
# 
## ----q1-part2-section2, message = F, fig.align = 'center', echo = F, fig.width = 8, fig.height = 8----------------------

ggarrange(
  ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_A_to_E_Rated, y = FTE_Posts_Occupied)) + 
  geom_point(colour = "maroon", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Proportion of Successful Responses for A-E Rated Establishments", y = "Number of Professional FTE Officers", title = "Relationship between Proportion of Successful Responses and\nthe Number of Professional FTE Officers"),
  
  ggplot(na.omit(fda_data), aes(x = Percentage_Interventions_A_to_E_Rated, y = Proportion_of_FTE_by_Establishments)) + 
  geom_point(colour = "red", alpha = 0.5) + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(x = "Proportion of Successful Responses for A-E Rated Establishments", y = "Proportion of Professional FTE Officers", title = "Relationship between Proportion of Successful Responses and\nthe Proportion of Professional FTE Officers per Establishment"),
  ncol = 1
) %>% annotate_figure(bottom = textGrob("\nFigure 1.8: Relationship between Proportion of Successful Responses and Professional FTE Officers", gp=gpar(fontsize=10,font=3)))


# 
# From the results of the tests, we can say that there is no significant effect of the number of employees on the proportion of successful responses, $t(321) = -0.093, p = 0.926$ and $F(1,321) = 0.008, p = 0.9262$. The chart above confirms this as we can see a relatively flat line for this relationship. However, when we consider the proportion of employees per establishment, we see there is a positive relationship with the proportion of successful responses. This is statistically significant, $t(321) = 3.869, p = 0.0001$ and $F(1,321) = 14.966, p = 0.0001$. We can infer that as we increase the number of professional FTE officers per establishment by 1%, we can expect the proportion of successful responses to increase by 24.4 units, 95% CI [12-36.81].
# 
# It is to be noted that though we have obtained statistically significant results while trying to fulfill the requests, more analysis is required before implementing any decisions arrived at after interpreting these results. There are some variables that were not explored that could be worthwhile looking into. Finally, the data engineering process in general could be improved by providing better names for columns and obtaining more relevant data.  


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


#' 
#' # Question 2 : Book Sales
#' 
#' ## Section 1
#' 
#' This section of the report addresses the following questions raised by the management team of the publishing company:
#' 
#' 1. Do books from different genres have different daily sales on average?
#' 2. Do books have more/fewer sales depending upon their average review scores and total number of reviews?
#' 3. What is the effect of sale price upon the number of sales, and is this different across genres?
#' 
#' ### Data Dictionary
#' 
#' Variable | Description
#' ------------- | -------------
#' sold.by | Publishing house that sold the book
#' publisher.type | Type of publisher
#' genre | Genre of the book - either "Children's", "Fiction" or "Non Fiction"
#' avg.review | Average rating of the book over the entire time period. It is a rating between 0 and 5
#' daily.sales | Average number of sales across the entire time period excluding refunds
#' total.reviews | Total number of reviews for the book
#' sale.price | Average price in pounds (£) for which the book was sold across the entire time period
#' 
#' ### Reading the data
#' 
## ----q2-read-data, message = F, fig.align = 'center'--------------------------------------------------------------------

# Reading the data
books <- read.csv("publisher_sales.csv", stringsAsFactors = T)

# Removing duplicate elements
books <- distinct(books)

# Structure and summary of the data
# str(books)
# summary(books)

# Checking if there are any NULL values
# colSums(is.na(books))


#' 
#' ### Summary and visualisations of the continous variables
#' 
## ----q2-data-exploration, message = F, fig.align = 'center', fig.align = 'center'---------------------------------------

# Making the levels for publisher.type and genre more readable
levels(books$publisher.type) <- list(Amazon = "amazon", "Big Five" = "big five", Indie = "indie", "Single Author" = "single author", "Small/Medium" = "small/medium")

levels(books$genre) <- list(Childrens = "childrens", Fiction = "fiction", "Non Fiction" = "non_fiction")

# HarperCollins Publishers and HarperCollins Publishing are the same
# Though HarperCollins is the parent company of HarperCollins Christian Publishing, we will treat them as a separate entities

books$sold.by <- as.character(books$sold.by)
books$sold.by[books$sold.by == "HarperCollins Publishing"] <- "HarperCollins Publishers"

# Renaming some of the sellers to make it easier to read
books$sold.by[books$sold.by == "Amazon Digital Services,  Inc."] <- "Amazon Digital Services"
books$sold.by[books$sold.by == "Penguin Group (USA) LLC"] <- "Penguin Group"
books$sold.by[books$sold.by == "Random House LLC"] <- "Random House"
books$sold.by[books$sold.by == "Simon and Schuster Digital Sales Inc"] <- "Simon and Schuster Digital Sales"
books$sold.by[books$sold.by == "RCS MediaGroup S.p.A."] <- "RCS MediaGroup"

# Converting sold.by to factor
books$sold.by <- as.factor(books$sold.by)

# Plotting all numeric data

daily.sales.plot <- ggplot(books) +
  geom_histogram(aes(x = daily.sales), bins = 35, colour = "black", fill = "#7abdff", alpha = 0.6) +
  geom_vline(xintercept = mean(books$daily.sales), linetype = "longdash", colour = "red") +
  labs(x ="Average Daily Sales", y = "Frequency", title = "Daily Sales")

avg.review.plot <- ggplot(books) +
  geom_histogram(aes(x = avg.review), bins = 35, colour = "black", fill = "#7abdff", alpha = 0.6) +
  geom_vline(xintercept = mean(books$avg.review), linetype = "longdash", colour = "red") +
  labs(x = "Average Review Ratings (0-5)", y = "Frequency", title = "Average Review Rating")

total.reviews.plot <- ggplot(books) +
  geom_histogram(aes(x = total.reviews), bins = 35, colour = "black", fill = "#7abdff", alpha = 0.6) +
  geom_vline(xintercept = mean(books$total.reviews), linetype = "longdash", colour = "red") +
  labs(x = "Number of Reviews", y = "Frequency", title = "Total Reviews")

sale.price.plot <- ggplot(books) +
  geom_histogram(aes(x = sale.price), bins = 35, colour = "black", fill = "#7abdff", alpha = 0.6) +
  geom_vline(xintercept = mean(books$sale.price), linetype = "longdash", colour = "red") +
  labs(x = "Sale Price of the Books (in £)", y = "Frequency", title = "Sale Price")

ggarrange(daily.sales.plot,
             avg.review.plot,
             total.reviews.plot,
             sale.price.plot) %>%
  annotate_figure(bottom = textGrob("\nThe red lines represent the mean value\nFigure 2.1: Visualising the Continuous Variables", gp=gpar(fontsize=10,font=3)))



#' 
#' We can see from the visualisation that the only column with a distribution closest to a normal distribution is Sale Price. The other columns exhibit skewed distributions. 
#' 
#' ### Visualising the Categorical Variables
#' 
## ----q2-data-exploration-categorical, message = F, fig.align = 'center'-------------------------------------------------

publisher.summary <- books %>% group_by(Publisher = publisher.type) %>% summarise(Frequency = n())
genre.summary <- books %>% group_by(Genre = genre) %>% summarise(Frequency = n())
seller.summary <- books %>% group_by(Seller = sold.by) %>% summarise(Frequency = n())


publisher.plot <- ggplot(publisher.summary, aes(x = reorder(Publisher, Frequency), y = Frequency)) +
  geom_col(colour = "black", aes(fill = Publisher)) +
  labs(x = "Publisher", y = "Frequency", title = "Frequency of Publishers") +
  theme(legend.position = "None") + scale_x_discrete(labels=wrap_format(6))

genre.plot <- ggplot(genre.summary, aes(x = Genre, y = Frequency)) +
  geom_col(colour = "black", aes(fill = Genre)) +
  labs(x = "Genre of Book", y = "Frequency", title = "Frequency by Genre") +
  theme(legend.position = "None") + ylim(0,2500)

seller.plot <- ggplot(seller.summary, aes(x = reorder(Seller, Frequency), y = Frequency)) +
  geom_col(aes(fill = Seller), colour = "black") +
  coord_flip() +
  labs(x = "Seller", y = "Frequency", title = "Frequency by Seller") +
  theme(legend.position = "None")

grid.arrange(seller.plot,
             publisher.plot,
             genre.plot,
             ncol = 2,
             bottom = textGrob("\nFigure 2.2: Visualising categorical variables",
                               gp = gpar(fontsize=10,font=3)),
             heights = c(2,1.7),
             layout_matrix = rbind(1,c(2,3))
             )


#' 
#' ### Data Cleaning
#' 
## ----q2-data-cleaning, message = F, fig.align = 'center'----------------------------------------------------------------

# Removing record where daily.sales < 0 as sales cannot be negative
books <- books %>% filter(daily.sales >= 0)


#' 
#' ### Do books from different genres have different daily sales on average?
#' 
#' This section addresses the relationship between daily sales and the genre of the book.
#' 
## ----q2-1, message = F, fig.align = 'center'----------------------------------------------------------------------------

daily.sales.by.genre <- lm(daily.sales~genre, books)
(daily.sales.by.genre.emm <- emmeans(daily.sales.by.genre, ~genre))
summary(daily.sales.by.genre)
cbind(coefficient = coef(daily.sales.by.genre), confint(daily.sales.by.genre))

ggarrange(
  ggplot(books, aes(x = daily.sales, y = genre)) +
  geom_boxplot(aes(fill = genre), outlier.shape = NA) +
    coord_flip() +
  labs(x = "Daily Sales", y = "Genre", title = "Daily Sales by Genre", subtitle = "Error bars are 95% Confidence Intervals") +
  theme(legend.position = "None")
  )  %>% annotate_figure(bottom = textGrob("Figure 2.3: Visualing Daily Sales by Genre", gp = gpar(fontsize=10,font=3)))



#' 
#' ### Do books have more/fewer sales depending upon their average review scores and total number of reviews?
#' 
#' This section addresses the relationship between daily sales and their average review rating and total number of reviews. 
#' 
## ----q2-2, message = F, fig.align='center'------------------------------------------------------------------------------

# model without interaction term
daily.sales.by.reviews.or.total <- lm(daily.sales~avg.review+total.reviews, books)
summary(daily.sales.by.reviews.or.total)

# model with interaction term
daily.sales.by.reviews.and.total <- lm(daily.sales~avg.review*total.reviews, books)
summary(daily.sales.by.reviews.and.total)
cbind(coefficient = coef(daily.sales.by.reviews.and.total), confint(daily.sales.by.reviews.and.total))

# comparing the two models using anova
anova(daily.sales.by.reviews.or.total, daily.sales.by.reviews.and.total)

# Visualising the model

ggarrange(ggplot(books, aes(x = daily.sales, y = avg.review)) + geom_point(alpha = 0.2, colour = "turquoise") + geom_smooth(method = "lm") + labs(x = "Average Daily Sales", y = "Average Review Rating (0-5)", title = "Daily Sales by Average Review"),
          ggplot(books, aes(x = daily.sales, y = total.reviews)) + geom_point(alpha = 0.2, colour = "turquoise") + geom_smooth(method = "lm") + labs(x = "Average Daily Sales", y = "Average Total Reviews", title = "Daily Sales by Total Review")) %>%
  annotate_figure(bottom = textGrob("Figure 2.4: Regression fit for Daily Sales by Average Reviews and Total Reviews", gp = gpar(fontsize=10,font=3)))

# Visualising Interaction model

plot_model(daily.sales.by.reviews.and.total, 
           type = "pred", 
           terms = c("avg.review", "total.reviews"),
           legend.title = "Total Reviews",
           axis.title = c("Average Review Rating (0-5)","Average Daily Sales"),
           title = "Effect of Total Reviews and Average Review Rating on Daily Sales") %>% annotate_figure(bottom = textGrob("Figure 2.5: Effect of Total Reviews and Average Review Rating on Daily Sales", gp = gpar(fontsize=10,font=3)))




#' 
#' ### What is the effect of sale price upon the number of sales, and is this different across genres?
#' 
#' This section addresses the genre-wise relationship between daily sales and the sale price.
#' 
## ----q2-3, message = F, fig.align = 'center', fig.align = 'center'------------------------------------------------------

# Using sale.price and genre as an interaction term to check the effect it has on daily sales
daily.sales.by.sale.price.genre <- lm(daily.sales~sale.price*genre, books)
summary(daily.sales.by.sale.price.genre)
cbind(coefficients = coef(daily.sales.by.sale.price.genre), confint(daily.sales.by.sale.price.genre))

#checks if sale price and genre have a significant effect on daily sales
anova(daily.sales.by.sale.price.genre)

# we can see from the ANOVA results that the interaction term between sale price and genre has a significant impact on the daily sales

# Plotting fit for daily sales and price
ggarrange(ggplot(books, aes(x = daily.sales, y = sale.price)) +
  geom_point(aes(colour = genre, alpha = 0.5)) +
  geom_smooth(method = "lm", colour = "black") +
  facet_grid(~genre, scales = "free") +
  labs(x = "Average Daily Sales", y = "Sale Price (in £)", title = "Effect of Sales Price on Daily Sales across different Genres") + 
  theme(legend.position = "None")
)  %>% annotate_figure(bottom = textGrob("Figure 2.6: Effect of Sales Price on Daily Sales by Genre", gp = gpar(fontsize=10,font=3)))

# Plotting the interaction model
plot_model(daily.sales.by.sale.price.genre, 
           type = "pred", 
           terms = c("sale.price", "genre"),
           legend.title = "Genre",
           axis.title = c("Average Sale Price (in £)","Average Daily Sales"),
           title = "Effect of Genre and Sale Price on Daily Sales") %>% annotate_figure(bottom = textGrob("Figure 2.7: Effect of Genre and Sale Price on Daily Sales", gp = gpar(fontsize=10,font=3)))


#' 
#' ---
#' 
#' ## Section 2
#' 
#' This report presents the results of the analyses that addresses the questions raised by the management team of the publishing company. The data was relatively clean and the only value that was removed was a record of a negative daily sales. Although there are outliers in the data, they have values that are realistic, which can be attributed to natural variability in the dataset. 
#' 
## ----q2-section2-part1, fig.align = 'center', message = F, echo = F-----------------------------------------------------

ggarrange(
  ggplot(books, aes(x = daily.sales, y = genre)) +
  geom_boxplot(aes(fill = genre), outlier.shape = NA) +
    coord_flip() +
  labs(x = "Daily Sales", y = "Genre", title = "Daily Sales by Genre", subtitle = "Error bars are 95% Confidence Intervals") +
  theme(legend.position = "None")
  )  %>% annotate_figure(bottom = textGrob("Figure 2.8: Visualing Daily Sales by Genre", gp = gpar(fontsize=10,font=3)))


#' 
#' 
#' From the box plot, we can see that Fiction books have the highest mean daily sales. The results of the regression shows that there is a significant effect between the daily sales and the genre of the book. On average, there are 55.58 Children's books sold daily, 95% CI [54.60-56.55] and is statistically significant, $t(5996) = 111.86, p < 0.0001$. For Fiction books, the daily sales is greater than Children's books sales by 50.31 books, 95% CI [48.93-51.69], and is statistically significant, $t(5996) = 71.60, p < 0.0001$. Finally, when we move to Non Fiction books, the daily sales is greater than Children's books sales by 20.33 books, 95% CI [18.95-21.7], and this increase is statistically significant, $t(5996) = 28.93, p < 0.0001$.
#' 
## ----q2-section2-part2, message = F, fig.align = 'center', echo = F-----------------------------------------------------

plot_model(daily.sales.by.reviews.and.total, 
           type = "pred", 
           terms = c("avg.review", "total.reviews"),
           legend.title = "Total Reviews",
           axis.title = c("Average Review Rating (0-5)","Average Daily Sales"),
           title = "Effect of Total Reviews and Average Review Rating on Daily Sales") %>% 
  annotate_figure(bottom = textGrob("Figure 2.9: Effect of Total Reviews and Average Review Rating on Daily Sales", gp = gpar(fontsize=10,font=3)))


#' 
#' From the visualisation above, we can see that daily sales is higher when the average rating of the book and the number of reviews is high. The interaction term between the average review rating and the total number of reviews is a significant predictor, $t(5995) = 11.388, p < 0.0001$. After running a model comparison test, the results shows that the overall model fit has improved significantly by adding the interaction term between average review rating and total number of reviews, $F(1,5995) = 129.68, p < 0.0001$.
#' 
#' From the analysis, we can say that the average review rating has a significant effect on the daily sales, $t(5995) = -13.817, p < 0.0001$, with daily sales decreasing by 13.7 books, 95% CI [(-15.66)-(-11.76)] for an increase in average review rating by 1. The total number of reviews has a significant effect on daily sales, $t(5995) = 4.873, p < 0.0001$. The daily sales increases by 0.17 books, 95% CI [0.1-0.23] for every additional review. 
#'  
#' 
## ----q2-section2-part3, message = F, echo = F, fig.align = 'center'-----------------------------------------------------

# Plotting the interaction model
plot_model(daily.sales.by.sale.price.genre, 
           type = "pred", 
           legend.title = "Genre",
           terms = c("sale.price", "genre"),
           axis.title = c("Average Sale Price (in £)","Average Daily Sales"),
           title = "Effect of Genre and Sale Price on Daily Sales") %>% annotate_figure(bottom = textGrob("Figure 2.10: Effect of Genre and Sale Price on Daily Sales", gp = gpar(fontsize=10,font=3)))


#' 
#' We can see from the visualisation that in general, the average daily sales reduces as the average sale price goes up. This behaviour is exhibited across all genres of books. The results from the regression model shows that there is a significant effect of the sale price on the daily sales with a decrease of 1.73 books, 95% CI [(-2.21)-(-1.25)] for an increase of £1 in sale price, $t(5993) = -7.059, p < 0.0001$. There is a significant effect of genre on the daily sales, with an increase in daily sales by 35.2 books, 95% CI [28.79-41.61] when we move to Fiction books, $t(5993) = 10.761, p < 0.0001$, and an increase of 6.4 books, 95% CI [0.12-12.67] when we move to Non Fiction books. 
#' 
#' Finally, there is also a significant effect of sale price on the sales of Fiction books, with sales increasing by 1.46 books, 95% CI [0.76-2.15] when we move to Fiction books, and increasing by 1.3 books, 95% CI [0.6-2.00] when we consider Non Fiction books. We can also say that the interaction term between sale price and the genre has a significant effect on daily sales, $F(2,5993) = 10.5, p < 0.0001$.
#' 
#' It is to be noted that though we have obtained statistically significant results while trying to fulfill the requests by the management team of the publishing company, more analysis is required before implementing any decisions arrived at after interpreting these results. For example, we noticed a decrease in daily sales for an increase in average review ratings, which does not make much sense. For this reason, it is imperative that better data is collected and a more thorough analysis performed. 

## Week 11

rm(list=ls())
setwd("/Users/se.kim/Library/CloudStorage/OneDrive-NortheasternUniversity/Northeastern/MKTG6234-S25")

# Today we use the heterogeneous MNL from week 9 to optimize prices.

library(tidyverse)
library(mlogit)
library(gridExtra) # <-- for showing mutiple ggplots at the same time


# load data, get segments, fit mnl model

# these are all things we did last week

# load main dataset
cust_dat <- read_csv("data/smartphone_customer_data.csv", show_col_types=F)

# load "enhanced" dataset for fitting mnl model
load("data/mnl_datasets.RData")

# fit mnl data
out <- mlogit(choice ~ apple:segment + 
                samsung:segment + 
                price:segment +
                screen_size:segment + 
                price:total_minutes:segment | 0, data=mdat1)


# Demand curve for a phone

# Let's construct the estimated demand curve for the small Samsung phone (S1).
# That means we will vary the price of S1 while holding everything else constant,
# and track how its market share changes as we change its price.

# Recall that last year S1 had a price of $799 and a market share of 25.3%.

# get a vector of price changes to use
pvec <- seq(from=-200, to=200, by=10)

# and construct empty matrix to store shares at each price
smat <- matrix(NA, nrow=length(pvec), ncol=6)
colnames(smat) <- c("A1", "A2", "S1", "S2", "H1", "H2")

# loop over the price change values
for(i in 1:length(pvec)) {
  
  # print progress
  cat("Working on", i, "of", length(pvec), "\n")
  
  # get the price change amount
  p <- pvec[i]
  
  # change prices for S1 phones
  tempdat <- as_tibble(mdat1) %>% 
    mutate(price = ifelse(idx$phone_id == "S1", price + p, price))
  
  # make market share predictions with the temporarily-changed S1 prices
  preds <- predict(out, newdata=tempdat)
  preds <- preds[, c("A1", "A2", "S1", "S2", "H1", "H2")]
  
  # calculate and store market shares
  smat[i,] <- colMeans(preds)
}

# gather our prices and estimated shares into a dataframe
relcol <- which(colnames(smat) == "S1")
s1dat <- tibble(scenario=1:length(pvec), price=pvec+799, share=smat[,relcol])

# plot S1's inverse demand curve
ggplot(s1dat, aes(x=share, y=price)) +
  geom_point() + 
  geom_line() + 
  labs(x="Share", y="Price") +
  theme_bw()

# Congratulations! You just estimated and plotted your first (residual) demand curve!
# Question now we have, What would be the optimal price?? 

# Note : we call it "residual" demand when it's product-specific and takes other 
# factors as given. Doing so acknowledges that when other factors change, like say 
# prices of competing products A1 or H1, then S1 residual demand curve would change also.

# Notice that the model-predicted market share is not exactly the observed market share,
# because we have brand-specific coefficients but not product-specific coefficients.
# This isn't a "problem" but rather just something to be aware of.

# actual market shares
cust_dat %>% filter(years_ago == 1) %>% count(phone_id) %>% mutate(shr = n / sum(n))

# predicted market shares at 0 price change
smat[21,] %>% round(3)


# Convert shares to number of phones

# Suppose the market size is such that 150 million smartphones are sold in the US each year
# and further suppose that the college-age demographic that we've measured with our
# dataset comprises 1 out of every 15 smartphone sales, or 10 million phones.

M <- 10

# Let's scale our demand curve to be in the price-quantity space instead of the price-share space

s1dat <- s1dat %>% mutate(quantity = share*M)

ggplot(s1dat, aes(x=quantity, y=price)) + 
  geom_point() + 
  geom_line() + 
  labs(x="Quantity", y="Price") +
  theme_bw()



# Calculate S1 price to maximize S1 profits based on own-price elasticity

# Marginal cost

# We need to know our marginal cost function. Suppose that a manager 
# at Samsung informs us that it costs $470 to manufacture, transport, 
# and advertise one S1 phone, regardless of how many S1 phones are produced. 

mc1 <- 470

# Calculate own-price elasticity at +/- $10 from actual price of $799

p1 <- s1dat %>% filter(price==799-10) %>% pull(price)
q1 <- s1dat %>% slice(20) %>% pull(quantity)

p2 <- s1dat %>% slice(22) %>% pull(price)
q2 <- s1dat %>% slice(22) %>% pull(quantity)

# Percentage change in Quantity / Percentage change in Price
elasticity <- ((q2-q1)/q1) / ((p2-p1)/p1)
elasticity

# -3.62: 1% change in price should lead to a 3.62% change in quantity
# Approximate optimal price using the elasticity rule 
# if we have constant elasticity of demand curve)

mc1 * 1 / (1 - 1/abs(elasticity))

# this approach suggests that S1 price should be set much lower ($649) than its 
# current value ($799). However, it is based on an assumption of constant demand elasticity,
# whereas our estimated demand model does not restrict price elasticity to be constant

# As an aside: check elasticity over full range of prices considered

# total calc
p1 <- s1dat %>% slice(1) %>% pull(price)
q1 <- s1dat %>% slice(1) %>% pull(quantity)

p2 <- s1dat %>% slice(41) %>% pull(price)
q2 <- s1dat %>% slice(41) %>% pull(quantity)

elasticity <- ((q2-q1)/q1) / ((p2-p1)/p1)
elasticity

# one-window at a time calc
res_e <- vector(length=39)
for(i in 2:40) {
  p1 <- s1dat %>% slice(i-1) %>% pull(price)
  q1 <- s1dat %>% slice(i-1) %>% pull(quantity)
  
  p2 <- s1dat %>% slice(i+1) %>% pull(price)
  q2 <- s1dat %>% slice(i+1) %>% pull(quantity)
  
  res_e[i-1] <- ((q2-q1)/q1) / ((p2-p1)/p1)
}
summary(res_e)


# Instead of assuming constant elasticity, 
# For each one of 41 points, we want to calculate the price at that point
# And find which one will give profit maximization solution!

# Calculate S1 price to maximize S1 profit based on full set of MNL model estimates
# (We'll do this with grid search)

# revenue

s1dat <- s1dat %>% mutate(revenue = price * quantity)

p1 <- ggplot(s1dat) + geom_point(aes(x=quantity, y=price)) + theme_bw()
p2 <- ggplot(s1dat) + geom_point(aes(x=quantity, y=revenue)) + theme_bw()

grid.arrange(p2, p1, ncol=1)

# notice that revenue increases, but starts to flatten out, as quantity increases

# margin

s1dat <- s1dat %>% mutate(cost = mc1*quantity)

p3 <- ggplot(s1dat) + geom_point(aes(x=quantity, y=cost)) + theme_bw()

grid.arrange(p3, p2, p1, ncol=1)

# note how cost is linear, so unlike revenue, it does not start to flatten out
# at higher quantities

# profit, at a particular price, is the distance between the revenue and cost
# curves. Let's look at that distance graphically

ggplot(s1dat) + 
  geom_line(aes(x=quantity, y=revenue), color="blue") +
  geom_point(aes(x=quantity, y=revenue), color="blue") +
  geom_line(aes(x=quantity, y=cost), color="red") + 
  geom_point(aes(x=quantity, y=cost), color="red") + 
  labs(
    x = "Quantity",
    y = "Dollars", 
    title = "Revenue (blue) and Cost (red)"
  ) +
  theme_bw()

# the distance/gap is largest somewhere around a quantity of 3 million phones,
# which roughly corresponds to price in the $700-750.  Let's now calculate
# profit and the profit maximizing price more exactly.

# profit

s1dat <- s1dat %>% mutate(profit = revenue - cost)

p4 <- ggplot(s1dat) + geom_point(aes(x=quantity, y=profit)) + theme_bw()

grid.arrange(p4, p3, p2, p1, ncol=1)


# find S1-profit-maximizing quantity and price, and compare profit to $799 price

s1dat %>% filter(price == 799)
s1dat %>% filter(profit == max(profit))

# We see that the profit-maximizing price of $709 results $834 million of profits.
# While the price of $799 results in only 765 million of profits

# This demonstration has focused on finding the profit-maximizing price
# for phone S1, where we have only considered the profits of phone S1.
# Samsung, however, cares about the profit from its smartphone product line
# in total.  And so far we have ignored phone S2.  Let's now take S2 into account.


# Calculate S1 price to maximize total Samsung smartphone profit       

# The manager at Samsung reports that S2 (big phone) marginal costs are $490

mc2 <- 490

# Let's calculate quantity, revenue, cost, and profit for the S2 phone

s2dat <- tibble(scenario=1:length(pvec), price=899, share=smat[,4])

s2dat <- s2dat %>% mutate(quantity = share*M,
                         revenue = price * quantity,
                         cost = mc2*quantity,
                         profit = revenue - cost)

# now we will aggregate across phones to get total Samsung smartphone profit

s2dat <- s2dat %>% mutate(price=0)

sdat <- rbind(s1dat, s2dat)

sdat <- sdat %>% group_by(scenario) %>% 
  summarize_all(sum)

# find Samsung profit-maximizing quantity and price 

sdat %>% filter(price == 799)
sdat %>% filter(profit == max(profit))

# we get a profit maximizing price of $749 leading to $1,271 mil in profits, 
# which is $18 million more than the profit of $1,253 when s1 is priced at $799

# why do we get different answer?

# As you decrease the price of S1, S1 is estimated to garner a larger 
# market share. The increase to S1's share results from decreases to
# other phones' market shares.  These other phone include competitors' 
# phones like A1, A2, H1, and H2, but also Samsung's other phone S2.

share_dat <- as_tibble(cbind(S1_price=pvec+799, smat))

share_dat <- pivot_longer(share_dat, cols=A1:H2, names_to="phone", values_to="share")

ggplot(share_dat, aes(x=S1_price, y=share, color=phone)) +
  geom_line() + 
  geom_point() + 
  labs(
    x = "Price of S1 Phone",
    y = "Market Share",
    title = "Estimated Market Share Responses to S1 Price Change by Phone"
  ) +
  scale_x_continuous(limits = c(700,900)) +
  scale_y_continuous(limits = c(0,0.5)) +
  theme_bw()

# So the total-samsung profit-maximizing price for S1 needs to trade-off
# the increased revenue and profit from pricing S1 low, with the loss of
# revenue and profit from S2-to-S1 switchers that would occur if S1 was 
# priced very low.



# Summary of R commands introduced

# colMeans()    - for taking the average of each column of a matrix or data.frame





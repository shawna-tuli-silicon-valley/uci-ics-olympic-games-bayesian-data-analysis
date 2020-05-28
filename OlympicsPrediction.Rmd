```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(rstanarm)
library(bayesplot)
library(patchwork)
```

Load the raw data:
```{r message=FALSE}
olympics <- read_csv("data/olympics.csv")

glimpse(olympics)
```

Filter and format the data for this particular analysis:
```{r}
USA <- olympics %>%
	filter(NOC == "USA") %>%
	mutate(Won_Medal = !is.na(Medal))

glimpse(USA)
```

Exploratory data analysis:
```{r message=FALSE, warning=FALSE}
USA %>%
	mutate(Won_Medal = factor(Won_Medal, levels = c(T, F), labels = c("Medal", "No Medal"))) %>%
	ggplot(aes(x = Age)) +
	geom_histogram(aes(y = ..density..), fill = "#1a9988", alpha = 0.65) + 
	facet_grid( ~ Won_Medal)

plot_feature <- function(feature, label)
{
	feature = enquo(feature)
	
	USA %>%
		mutate(Won_Medal = factor(Won_Medal, levels = c(T, F), labels = c("Medal", "No Medal"))) %>%
		ggplot(aes(y = !!feature, x = Won_Medal, group = Won_Medal)) +
		geom_boxplot(fill = "#1a9988", alpha = 0.65) +
		ylab(label) +
		xlab("") +
		theme(axis.text = element_text(size = 14))
}

plot_feature(Age, "Age (yrs)")
plot_feature(Height, "Height (cm)")
plot_feature(Weight, "Weight (kg)")

USA %>%
	group_by(Sex, Won_Medal) %>%
	tally() %>%
	ungroup() %>%
	pivot_wider(names_from = "Won_Medal", values_from = "n") %>%
	mutate(`No Medal` = `FALSE` / sum(`FALSE`),
		   `Medal` = `TRUE` / sum(`TRUE`)) %>%
	pivot_longer(cols = c("No Medal", "Medal"), names_to = "Won_Medal", values_to = "prop") %>%
	ggplot(aes(x = "", y = prop, fill = Sex)) +
	geom_bar(stat = "identity") +
	coord_polar("y") +
	facet_grid( ~ Won_Medal) +
	ylab("Proportion") +
	xlab("") +
	scale_fill_manual(values = c("#1a9988", "gray80")) +
	theme(axis.text = element_blank(),
		  axis.ticks = element_blank(),
		  panel.grid = element_blank())
```

What is the outliers in `age`? Are 90-year-olds running track?
```{r}
USA %>%
	filter(Age > 90) %>%
	select(Name, Age, Year, Sport)
```
Answer - Art competitions were part of the Olympics in the 1930s!

Fit the logistic regression model:
```{r}
normalize <- function(vec)
	(vec - mean(vec, na.rm = T)) / sd(vec, na.rm = T)

USA_normalized <- USA %>%
	mutate(Age = normalize(Age),
		   Height = normalize(Height),
		   Weight = normalize(Weight))
```

```{r, results = "hide"}
model <- stan_glm(
	Won_Medal ~ Age + Sex + Height + Weight,
	data = USA,
	family = binomial(link = "logit")
)

model_normalized <- stan_glm(
	Won_Medal ~ Age + Sex + Height + Weight,
	data = USA_normalized,
	family = binomial(link = "logit")
)
```

See the coefficient estimates:
```{r}
coef(model)
```

Verify the models created good approximations:
```{r}
mcmc_trace(model)
mcmc_dens_overlay(model)
```

```{r message=FALSE}
posterior_interval(model, prob = 0.95) %>%
	round(digits = 2)

posterior_vs_prior(model_normalized, prob = 0.95, pars = c("Age", "Height", "SexM", "Weight")) +
	ggtitle("Posterior Medians for Coefficients (Normalized Data)")
```

Could we win a medal too?
```{r}
pred <- function(Age, Sex, Height, Weight)
{
	y_dist <- posterior_predict(model, newdata = data.frame(Age, Sex, Height, Weight))
	
	mean(y_dist) >= 0.5
}
```

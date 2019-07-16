## Data Visualisation

### Soil Type

A good starting point is to see which type of cover grows in which type of soil. Different soil porosity and acidity are known to play a large role in determining what plants can grow in a particular area, and so `Soil_Type` is sure to be a good predictor variable. For the purpose of exploration, we aren't interested in specific values, but rather the variation of `Cover_Type` by Soil_Type.

```{r}
ggplot(cov_Tidy, aes(x = Cover_Type, fill = Cover_Type)) +
  geom_bar() +
  facet_wrap(~reorder(Soil_Type, sort(as.integer(Soil_Type))), scales = 'free') +
  labs(fill = 'Cover Type', title = 'Cover Type by Soil Type') +
  scale_fill_brewer(palette = 'Set2') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank())
```

`Soil_Type`s 21, 25, 36 and 37 contain only a single cover type while others, such as 11 or 24, contain multiple. One thing to note is that there are several soil types where the only cover type is Krummholz. This is a type of cover that grows in harsh conditions, such as sub-alpine regions, where other plants are rare and soils may be poor in organic nutrients. Another point of interest is that Krummholz and Cottonwood/Willow covers do not share any soil types. You may also have noticed that soil types 7, 8 and 15 are missing; we simply did not have any instances of these types in our sample. Let's look at the role of `Elevation` and see if this explains any of our findings.

### Elevation

Using violin plots (a combination of box plots and density plots), we can get a decent idea of how each cover type is distributed across our predictor variables.

```{r}
ggplot(cov_Tidy, aes(x = Cover_Type, y = Elevation, fill = Cover_Type)) +
  geom_violin() + 
  stat_summary(fun.y = 'median', geom = 'point') +
  labs(x = 'Cover Type') +
  scale_fill_brewer(palette = 'Set2') +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))
```
As expected, Krummholz cover is found at very high elevations where other plants are rare. Not only do Krummholz and Cottonwood/Willow covers not share any of the same soil types, but they aren't found within nearly 500 meters of elevation to each other.

The [UCI dataset page](https://archive.ics.uci.edu/ml/datasets/Covertype) shows that the Neota wilderness area has the highest mean elevation, followed closely by the Rawah and Comanche Peak areas. Looking at a map of the Roosevelt National Forest tells us that the Cache la Poudre area is the area surrounding the lower-lying Cache la Poudre river. With our previous figure showing that `Elevation` plays a role in determining cover type, we should expect to find a different composition of `Cover_Type` in this area.

```{r}
ggplot(cov_Tidy, aes(x = Wilderness_Area, y = Elevation, fill = Cover_Type)) +
  geom_violin() + 
  labs(x = 'Wilderness Area') +
  scale_fill_brewer(name = 'Cover Type',
                    palette = 'Set2') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))
```

Keeping in mind that we are only working with a sample of the overall data, we can see that Cottonwood/Willow trees are only found in the Cache la Poudre area and Krummholz trees are only found in the upper elevations of the other three areas. A bar plot gives a better look at the distribution of `Cover_Type` by `Wilderness_Area`.

```{r}
ggplot(cov_Tidy, aes(x = Cover_Type, fill = Cover_Type)) +
  geom_bar() +
  facet_wrap(~Wilderness_Area) +
  labs(y = 'Count') +
  scale_fill_brewer(name = 'Cover Type',
                    palette = 'Set2') +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
```

### Slope

`Slope` is another geographic variable that might play a role in determining the type of cover in an area. Steeper slopes likely feature higher water run-off and increased erosion, leading to the need for cover types with stronger root systems that might be more tolerant to drier conditions.

```{r}
ggplot(cov_Tidy, aes(x = Cover_Type, y = Slope, fill = Cover_Type)) +
  geom_violin() + 
  stat_summary(fun.y = 'median', geom = 'point') +
  labs(x = 'Cover Type') +
  scale_fill_brewer(palette = 'Set2') +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))
```

Within our sample, there doesn't seem to be much variation in terms of `Slope`` when measured across `Cover_Type`. At most, there appears to be an 8 or 9 degree difference between the medians of the Ponderosa Pine and the Spruce/Fir types, but the ranges are highly similar for all covers. We can expect that slope may play a role, but it will not likely be as important as `Soil_Type` or `Elevation`.

### Aspect and Hillshade

The dataset also contains some temporal data in the form of hillshade index values measured throughout the day. These values can be plotted against the aspect values to give us an idea of how sunlight, direction and time influence `Cover_Type`.

```{r fig.height = 10}
cov_Tidy %>% 
  gather(Hillshade, Hillshade_Index, Hillshade_9am:Hillshade_3pm) %>% 
  mutate(Hillshade = factor(Hillshade, levels = c('Hillshade_9am', 
                                                  'Hillshade_Noon',
                                                  'Hillshade_3pm'),
                            labels = c('Hillshade 09:00',
                                       'Hillshade 12:00',
                                       'Hillshade 15:00'))) %>% 
  ggplot(aes(x = Hillshade_Index, y = Aspect, colour = Cover_Type)) +
  geom_point(alpha = 0.1) +
  facet_grid(Cover_Type ~ Hillshade) +
  labs(x = 'Hillshade Index') +
  scale_colour_brewer(name = 'Cover Type',
                      palette = 'Set2') +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  theme_minimal() +
  theme(legend.position = 'bottom',
        strip.text.y = element_blank())
```

Shade Index ~ Aspect figures can be difficult to read because of the shear number of points on the plot; each facet in the above graph contains 1000 points, many of which are overlapping. When we reduce the alpha levels so that the points are nearly transparent (changing the shape to something hollow could also be useful here) and view the figure on a large enough screen, some interesting patterns start to emerge. For example, Cottonwood/Willow cover mostly has aspects between 100 and 150 degrees azimuth, roughly facing east-southeast, creating a large amount of shade during the morning and much less during the afternoon.

### Distance to Landmarks

The dataset contains four measures of distance to three type of landmarks: water sources, roadways and fire points.

* Distance to water sources could show the extent of a cover's dependence on water. Lands closer to a water source are likely to have soil which is more saturated than those far away.

* Distance to roadways might indicate how well a type of cover deals with human interference; conversely, it could just show that roadways are built in a certain area, i.e. in lower elevations.

* Distance to fire points can be seen as a metric of a cover type's ability to regrow following a forest fire; types of cover which can regrow quickly will take hold of a new area long before a slow-growing cover. 

```{r fig.height = 5}
cov_Tidy %>% 
  gather(Measure, Distance, 
         Vertical_Distance_To_Hydrology:Horizontal_Distance_To_Fire_Points) %>% 
  mutate(Measure = factor(Measure, 
                          levels = c('Vertical_Distance_To_Hydrology',
                                     'Horizontal_Distance_To_Hydrology',
                                     'Horizontal_Distance_To_Roadways',
                                     'Horizontal_Distance_To_Fire_Points'),
                          labels = c('Vertical Distance to Hydrology',
                                     'Horizontal Distance to Hydrology',
                                     'Horizontal Distance to Roadways',
                                     'Horizontal Distance to Fire Points'))) %>% 
  ggplot(aes(x = Cover_Type, y = Distance, fill = Cover_Type)) +
  geom_violin() +
  stat_summary(fun.y = 'median', geom = 'point',
               show.legend = FALSE) +
  facet_wrap(~Measure, scales = 'free_y') +
  labs(x = 'Cover Type') +
  scale_fill_brewer(palette = 'Set2') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        axis.text.x = element_blank())
```

With the exception of `Vertical_Distance_To_Hydrology`, there seems to be a good amount of variation between `Cover_Type`s for the distance measurements. `Vertical_Distance_To_Hydrology` is on the smallest scale by about 800 meters, and all of the medians are within about 50 meters of each other. There is some variation in the ranges for this measurement, but the other three metrics might be better predictors of `Cover_Type`.

# Modelling

We should always consider correlations in the data. The `corrplot` function provides a succinct graph to visualise correlation between variables. Correlations are not necessarily bad, but we should be aware of them because they can impact certain machine learning models.

```{r fig.height = 7}
col <- colorRampPalette(c('#6D9EC1', '#FFFFFF', '#E46726'))
corrplot(cor(cov_Tidy[4:13]), method = 'color', type = 'upper', 
         diag = FALSE, addgrid.col = 'white', col = col(200),
         tl.cex = 0.5, tl.srt = 45, tl.col = 'black',
         addCoef.col = 'black', number.cex = 0.5)
```

For the most part, the data in our sample is uncorrelated with a few exceptions:
  
  * `Hillshade_9am` and `Hillshade_3pm` are negatively correlated and `Hillshade_Noon` and `Hillshade_3pm` are positively correlated. Interestingly, `Hillshade_9am` and `Hillshade_Noon` show virtually no correlation (-0.01).

* `Slope` and `Hillshade_Noon` are positively correlated.

* `Aspect` is also correlated with `Hillshade_9am` (negatively) and `Hillshade_3pm` (positively), which we would expect.

* Vertical and horizontal distances to hydrology are positively correlated.

Due to the nature of random variable selection and single variable splits with random forest models, correlated values aren't much of a concern. There is always a chance that the correlated values won't be chosen together for each decision, provided the mtry value is not too high and, even if the correlated values are chosen together, the splits are based only on one predictor variable.

Correlated values can pose a problem for kNN models, but we can deal with this by performing feature selection prior to building a kNN model. There are two ways we can do this: 
  
  1. by running a PCA 

2. by building a random forest model with all variables and using the built-in importance measures.

## Variable Selection

For variable selection using a random forest model, we will use the `caret` package and the 'rf' method from the `randomForest` package. We will perform a 10-fold cross-validation to test out-of-sample performance. We set the `importance` argument to `TRUE` and then use the `varImp` function on the output to provide us with a data frame listing the relative importance of each variable on predicting the `Cover_Type`. 

We use our tidied data as input into our random forest model because the model will automatically one-hot encode all of the categorical variables.

```{r eval = FALSE}
vars <- train(Cover_Type ~ ., data = cov_Tidy,
              method = 'rf',
              importance = TRUE,
              trControl = trainControl(method = 'cv',
                                       number = 3,
                                       verboseIter = TRUE))
varImp(vars)
```

Based on the importance results, we are going to go ahead and use all the variables to build our models. We could reduce the number used by removing some individual soil types or wilderness areas, but we wouldn't have any real justification for doing so in this case. 

## Model Building

Let's start building some models by splitting our data into training and test sets. We will use an 80/20 split.

```{r message = FALSE}
set.seed(1808)
cov_Tidy_Train <- sample_frac(cov_Tidy, 0.8)
cov_Tidy_Test <- anti_join(cov_Tidy, cov_Tidy_Train)
```

We can also build our trainControl now to save some work later. We are going to use a simple 10-fold cross-validation to test for out-of-sample performance.

```{r eval = FALSE}
control <- trainControl(method = 'cv', number = 3)
```

### K Nearest Neighbours

For our k-Nearest Neighbors model, we need to remember to pre-process the data by centring and scaling it. We can also select a range of values of k for our model to test. Here, we will use the square root of the number of rows of our training data +/- 10.

```{r eval = FALSE}
set.seed(1808)
mod_knn <- train(Cover_Type ~ ., data = cov_Tidy_Train,
                 method = 'knn',
                 tuneGrid = expand.grid(k = (round(sqrt(nrow(cov_Tidy) * 0.8)) - 10):
                                          (round(sqrt(nrow(cov_Tidy) * 0.8)) + 10)),
                 preProc = c('center', 'scale'),
                 trControl = control)
mod_knn
preds_knn <- predict(mod_knn, cov_Tidy_Test)
mean(preds_knn == cov_Tidy_Test$Cover_Type)
```

### Random Forest with rf

```{r eval = FALSE}
set.seed(1808)
mod_rf <- train(Cover_Type ~ ., data = cov_Tidy_Train,
                method = 'rf',
                trControl = control)
mod_rf
preds_rf <- predict(mod_rf, cov_Tidy_Test)
mean(preds_rf == cov_Tidy_Test$Cover_Type)
```

### Random Forest with ranger

```{r eval = FALSE}
set.seed(1808)
mod_ranger <- train(Cover_Type ~ ., data = cov_Tidy_Train,
                    method = 'ranger',
                    trControl = control)
mod_ranger
preds <- predict(mod_ranger, cov_Tidy_Test)
mean(preds == cov_Tidy_Test$Cover_Type)
```
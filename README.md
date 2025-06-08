As our group began our brainstorm on ideas to investigate using the tools we've learned in STA323, we agreed that whatever topic we would explore would need to have publicly available and substantially sized data sets with many variables. This was to ensure that our visualizations would have multiple angles and components to explore from. Ultimately, we chose to investigate the relationship between the time of year of travel and travel volume. We hypothesized that busy times of year, like the summer months (depending on the hemisphere) and major holidays would coincide with spikes in travel volume.

Using the Global Holidays and Travel data set from TidyTuesday,(https://github.com/rfordatascience/tidytuesday/blob/main/data/2024/2024-12-24/readme.md) our plan for this project was to create an interactive Shiny dashboard that explores the relationship between time of year (especially around national holidays) and travel patterns. The data set has the data broken down by country, allowing us to examine travel trends by time and by country. We originally wanted our Shiny app dashboard to output the following once the user selected a country:

1.  An annotated time series graph showing monthly travel data with holiday events marked
2.  A color-coded heat map displaying seasonal patterns in holiday occurrences by type
3.  A detailed table listing specific holidays with their corresponding travel statistics.

This would've emphasized the effect that holidays have on travel behavior and allowed users to choose a holiday and country of interest to investigate visually. However, we ran into issues with this approach, in part because the data set we used only shows specific month-year data, but not individual days. We attempted to work around it by having a user select the month and then have both holiday data for the month and travel data be displayed, but we ultimately decided that the information was non-informative and not relevant to our original goal. The bare bones version of that app is saved as app2.r in the Shiny folder.

We created another trial of the app in app2.r and saved it as app3.r. Since the initial app only showed statistics, we thought it would be interesting to add a visual representation of monthly travel trends and top destination countries by month. We updated the app to include a world map that highlights and labels specific countries based on travel volume for the selected month. We also added a line plot with points to show the average travel volume for the selected month compared to other months. This way, users can visually identify which countries and months may not be optimal for travel. This set us up for what we wanted for our third and final design update for what we wanted for the app.

For our final version, we settled on the following user experience goals:

Our target demographic for our Shiny App is a prospective traveler, looking to avoid the higher prices and long delays that typically come with travelling during peak times. Therefore, when deciding on travel dates for a specific country, they would be interested in viewing the travel volume trends over time for that country to avoid its peak season. Furthermore, in order to avoid busy holiday travel seasons, the user would also want to know what times of the year coincide with the most holidays in that country (when more people would be free to travel). Finally, once the user has settled on a specific month and country to travel in, as one final check, they would want to the exact dates and names of the holidays that occur during that month (a user would be concerned about traveling on a country's Independence Day, for example, but unconcerned about traveling on National Talk Like a Pirate Day).

Therefore, our Shiny Dashboard will output the following:

1.  A line plot, displaying the monthly travel volume for the years in our data set for the country or countries selected. An interactive and movable world map will be included, which will prompt the user to click on their country or countries of interest. The line plot will update based on the selected countries.

2.  A heat map, displaying the number of public holidays per month and year in our data set for the country selected. Year will be displayed on the X axis and Month on the Y axis, and a color scale will be used to visualize the number of public holidays in each Month-Year combination. The user will again be prompted with the interactive world map and the option to select a country to investigate.

3.  A table listing specific holidays for the selected country observed in that month across all available years for your selected country or countries. Clicking on a holiday name in that first table populates the bottom table with the exact dates on which that holiday fell, again covering every year in the dataset. 

Order to Run Code:

Keep in mind that our project is designed for a user to run our project.qmd code all the way through first and then run our final app, which is saved as app.R in the Shiny folder in our project.

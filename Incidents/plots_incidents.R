# Function to create trend line plot of number of accidents
trend_plot <- function(dataset){
  dataset %>% 
    count(JAAR_VKL) %>%
    ggplot() +
    geom_line(aes(x = as.factor(JAAR_VKL), y = n, group = 1), color = "Red") +
    labs(title = "Aantal ongevallen per jaar",
       x = "Jaar",
       y = "Aantal") +
    theme_classic() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 14))
}

# Histogram plot of distribution of incidents among comparable areas 
incidents_histogram <- function(incidents_all_inclu_niveau,
                                count_incidents_niveau,
                                incidents_mean
                                ){
  ggplot(incidents_all_inclu_niveau, aes(x = n)) +
  geom_histogram(binwidth = 4, fill = "dark gray") +
  geom_vline(xintercept = count_incidents_niveau, color = "blue", size = 1.5) +
  geom_vline(xintercept = incidents_mean, color = "green4", size = 1.5) +
  annotate(x = count_incidents_niveau, y = +Inf, 
           geom = "label", label = "Geselecteerd gebied", vjust = 2, color = "blue") +
  annotate(x = count_incidents_niveau, y = +Inf, 
             geom = "label", label = as.character(count_incidents_niveau), vjust = 3, color = "blue") +
  annotate(x = incidents_mean, y = +Inf, 
           geom = "label", label = "Gemiddelde", vjust = 4, color = "green4") +
  annotate(x = incidents_mean, y = +Inf, 
           label = as.character(incidents_mean), vjust = 5, 
           geom = "label", color = "green4") +
  labs(title = "Verdeling van aantal ongelukken in vergelijkbare gebieden",
       x = "Aantal ongelukken",
       y = "Aantal gebieden") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  theme_classic()
}
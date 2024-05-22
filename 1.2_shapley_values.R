library(ggforce)
library(janitor)
# a function to standardize feature values into same range
std1 <- function(x){
  return ((x - min(x, na.rm = T))/(max(x, na.rm = T) - min(x, na.rm = T)))
}

# Define the pairs you want to keep
pairs_to_keep <- tibble(
  shap_var = c("abar", "bbar","cbar","dbar"),
  var = c("Pop","GDPperCapita","EnergyIntensity","CarbonIntensity"))


plot_df <- ShapleyValue %>% 
  cbind(ShapleyWorld %>% select(GDPperCapita,EnergyIntensity,CarbonIntensity,Pop,Period)) %>% 
  as_tibble() %>%
  pivot_longer(cols = contains("bar"),names_to = "shap_var",values_to = "shap_values") %>%
  group_by(shap_var) %>% 
  mutate(median_shap = median(abs(shap_values),na.rm=T)) %>% 
  ungroup() %>% 
 # mutate_at(1:4,std1) %>% 
  pivot_longer(cols = 1:4, names_to = "var",values_to = "values") %>% 
  filter(
    ( shap_var == pairs_to_keep$shap_var[1] & var == pairs_to_keep$var[1]) |
    ( shap_var == pairs_to_keep$shap_var[2] & var == pairs_to_keep$var[2])|
    ( shap_var == pairs_to_keep$shap_var[3] & var == pairs_to_keep$var[3]) |
    ( shap_var == pairs_to_keep$shap_var[4] & var == pairs_to_keep$var[4])
  ) %>% 
  mutate(zvalues = scale(values)) %>%
  filter(between(zvalues, -2.5, 2.5)) 



# SHAP


plot_df %>% 
  mutate(shap_var = case_when(shap_var == "abar" ~ "Pop",
            shap_var == "bbar" ~ "GDP per Capita",
            shap_var == "cbar" ~ "Energy Intensity",
            T ~ "Carbon Intensity")) %>% 
  mutate(shap_var = fct_reorder(shap_var,median_shap)) %>% 
  ggplot(aes(shap_values,shap_var)) +
    # sina plot: 
    geom_sina(aes(color = values),alpha=0.3) +
    # print the mean absolute value: 
    geom_text(aes(x = -Inf, y=shap_var, label = sprintf("%.3f", median_shap)),
              size = 3, alpha = 0.7,
              hjust = -0.2, 
              fontface = "bold") + # bold
    # # add a "SHAP" bar notation
    # annotate("text", x = -Inf, y = -Inf, vjust = -0.2, hjust = 0, size = 3,
    #          label = expression(group("|", bar(SHAP), "|"))) + 
    scale_color_gradient(low="#FFCC33", high="#6600CC", 
                         breaks=c(0,1), labels=c("Low","High")) +
    theme_bw() + 
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), # remove axis line
          legend.position="bottom") + 
    geom_vline(xintercept = 0) + # the vertical line
    labs(x = "SHAP value (impact on Co2 Emissions)", y = "", color = "Feature value") 

  
 
 
 # Over time



plot_df %>% 
  mutate(shap_var = case_when(shap_var == "abar" ~ "Pop",
                              shap_var == "bbar" ~ "GDP per Capita",
                              shap_var == "cbar" ~ "Energy Intensity",
                              T ~ "Carbon Intensity")) %>% 
  ggplot(aes(values, shap_values,col=Period)) +
  geom_point(alpha=0.2) +
  facet_wrap(~ shap_var,scales = "free") +
  geom_smooth(method = "loess", col="red") +
  theme_minimal() +
  labs(y="Shapley Values")

 
 
 

# Work a bit with the scaling: ---------







plot_df
 
 
 
 
 
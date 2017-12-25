library(cloudy)
data("demo_cloudy")
posee <- demo_cloudy %>%
  condition(
    "tu fumes combien ?" = list(`tu fumes ?` == 'oui' & age > 18)
    , "date des derniere regles" = list(sexe == 'F')
  )
demo_cloudy %>% quali_desc(`tu fumes combien ?`,posee = posee)

demo_cloudy %>% quali_desc(`tu fumes combien ?`)

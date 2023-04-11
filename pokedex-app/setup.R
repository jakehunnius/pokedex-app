library(tidyverse)
library(ggplot2)
url = "https://raw.githubusercontent.com/lgreski/pokemonData/master/Pokemon.csv"
pokedex = read_csv(url)

pokedex = pokedex |> 
  mutate(Form = replace_na(Form, "Base")) |> 
  mutate(Type2 = replace_na(Type2, "None"))



write_csv(x = pokedex, file = "Pokedex/data/pokedex.csv")




pokedex |> 
  filter(Name == "Charizard" | Name == "Nacli") |> 
  filter(Form == "Base" | Form == "Base") |> 
  pivot_longer(HP:Speed, names_to = "Stat", values_to = "Count") |> 
  group_by(Name, Stat) |> 
  summarise(Count = sum(Count)) |> 
  ggplot() +
  ifelse("Charizard" == "Charizard",
         aes(x = Stat, y = Count, fill = Form),
         aes(x = Stat, y = Count, fill = Name)) |> 
  geom_bar(stat = "identity", position = "Dodge") +
  theme_bw()
  
pokedex |> 
  filter()

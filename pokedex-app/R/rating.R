calc_rating = function(data) {
  data |> 
    mutate(mean.total = mean(Total),
           mean.attack = mean(Attack),
           mean.spattack = mean(`Sp. Atk`)) |> 
    mutate(Rating = round(100*((Total/mean.total)+(Attack/mean.attack)+(`Sp. Atk`/mean.spattack) - 1), digits = 3)) |> 
    select(-mean.total, -mean.attack, -mean.spattack)
}




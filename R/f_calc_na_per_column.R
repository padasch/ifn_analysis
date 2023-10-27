calc_na_per_column <- function(df_in) {
  
  vec_missing_tot  <- colSums(is.na(df_in))
  vec_missing_perc <- colMeans(is.na(df_in)) * 100
  
  tibble(
    variable = names(vec_missing_perc),
    na_perc  = vec_missing_perc,
    na_count = vec_missing_tot
  ) |> 
  arrange(desc(na_count)) 
}

# calc_statistics_per_column <- function(df_in) {
#   
#   
#   cible_na |> 
#     select(where(is.numeric)) |> 
#     summary() |> 
#     as.data.frame() |> 
#     group_by(Var2) |> 
#     filter(str_detect(Freq, "Mean")) |> 
#     ungroup() |> 
#     mutate(var_mean = str_remove(pattern = "Mean   :", Freq), 
#            var_mean = as.double(var_mean),
#            var = as.factor(str_trim(Var2))) |> 
#     select(var, var_mean) |> 
#     print(n = 60)
# }


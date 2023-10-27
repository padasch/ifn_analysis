get_coords_from_df <- function(
  df_in,
  x = "lon",
  y = "lat"
){
  
  df_in |> 
    ungroup() |>
    select(
      !!x,
      !!y
      ) |> 
    distinct() |>
    rename(
      y = !!y, 
      x = !!x
      )
  }

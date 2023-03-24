# printing works

    Code
      print(fit)
    Output
      <trending_fit_tbl> 2 x 4
        model_name result       warnings     errors      
        <chr>      <named list> <named list> <named list>
      1 l          <lm>         <NULL>       <NULL>      
      2 nb         <negbin>     <NULL>       <NULL>      

---

    Code
      print(pred)
    Output
      <trending_predict_tbl> 2 x 4
        model_name result              warnings     errors      
        <chr>      <named list>        <named list> <named list>
      1 l          <trndng_p [32 x 7]> <NULL>       <NULL>      
      2 nb         <trndng_p [32 x 7]> <NULL>       <NULL>      

---

    Code
      print(pred$result[[1]])
    Output
      <trending_prediction> 32 x 7
            hp   cyl estimate lower_ci upper_ci lower_pi upper_pi
         <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
       1   110     6    141.     127.     155.     60.6      221.
       2   110     6    141.     127.     155.     60.6      221.
       3    93     4     76.8     54.5     99.0    -5.17     159.
       4   110     6    141.     127.     155.     60.6      221.
       5   175     8    205.     185.     225.    123.       286.
       6   105     6    141.     127.     155.     60.6      221.
       7   245     8    205.     185.     225.    123.       286.
       8    62     4     76.8     54.5     99.0    -5.17     159.
       9    95     4     76.8     54.5     99.0    -5.17     159.
      10   123     6    141.     127.     155.     60.6      221.
      # i 22 more rows


# Final Sorting Arrangement

    Code
      print(pkdf, n = Inf, width = Inf)
    Output
      # A tibble: 30 × 84
         C     NSTUDY SUBJID    ID   ATFD  ATLD  NTFD  NTLC  NTLD  NDAY   TPT  EVID
         <chr>  <int>  <dbl> <int>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1 <NA>       1     NA     1     0     0      1     1     0     1     1     1
       2 <NA>       1     NA     1  2082. 2081.     1     1     0     1     1     0
       3 <NA>       1     NA     1  9524.   NA     NA    NA    NA    NA    NA     2
       4 <NA>       2     NA     2     0     0      3     3     0     2     2     1
       5 <NA>       2     NA     2  2393. 2389.     3     3     0     2     2     0
       6 <NA>       2     NA     2  9567.   NA     NA    NA    NA    NA    NA     2
       7 <NA>       3     NA     3     0     0      5     5     0     3     3     1
       8 <NA>       3     NA     3  2205  2196      5     5     0     3     3     0
       9 <NA>       3     NA     3  9661.   NA     NA    NA    NA    NA    NA     2
      10 <NA>       4     NA     4     0     0      7     7     0     4     4     1
      11 <NA>       4     NA     4  2764. 2748.     7     7     0     4     4     0
      12 <NA>       4     NA     4 10097.   NA     NA    NA    NA    NA    NA     2
      13 <NA>       5     NA     5     0     0      9     9     0     5     5     1
      14 <NA>       5     NA     5  2701. 2676.     9     9     0     5     5     0
      15 <NA>       5     NA     5  9913.   NA     NA    NA    NA    NA    NA     2
      16 <NA>       6     NA     6     0     0     11    11     0     6     6     1
      17 <NA>       6     NA     6  3012. 2976.    11    11     0     6     6     0
      18 <NA>       6     NA     6 10410.   NA     NA    NA    NA    NA    NA     2
      19 <NA>       7     NA     7     0     0     13    13     0     7     7     1
      20 <NA>       7     NA     7  3288. 3239.    13    13     0     7     7     0
      21 <NA>       7     NA     7 10410.   NA     NA    NA    NA    NA    NA     2
      22 <NA>       8     NA     8     0     0     15    15     0     8     8     1
      23 <NA>       8     NA     8  3290. 3226.    15    15     0     8     8     0
      24 <NA>       8     NA     8 10627.   NA     NA    NA    NA    NA    NA     2
      25 <NA>       9     NA     9     0     0     17    17     0     9     9     1
      26 <NA>       9     NA     9  3470. 3389.    17    17     0     9     9     0
      27 <NA>       9     NA     9 10765.   NA     NA    NA    NA    NA    NA     2
      28 <NA>      10     NA    10     0     0     19    19     0    10    10     1
      29 <NA>      10     NA    10  3444. 3344.    19    19     0    10    10     0
      30 <NA>      10     NA    10 10786.   NA     NA    NA    NA    NA    NA     2
           MDV   CMT  DVID   AMT  ADDL    II   ODV    LDV   BLQ  LLOQ DOSENUM DOSEA
         <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>   <dbl> <dbl>
       1     1     0    NA     1     1     1    NA NA        NA    NA       1     1
       2     0     1     1    NA    NA    NA     1  0         0     1       1     1
       3    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
       4     1     0    NA     2     2     2    NA NA        NA    NA       1     2
       5     0     2     2    NA    NA    NA     2  0.693     0     2       1     2
       6    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
       7     1     0    NA     3     3     3    NA NA        NA    NA       1     3
       8     0     3     3    NA    NA    NA     3  1.10      0     3       1     3
       9    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      10     1     0    NA     4     4     4    NA NA        NA    NA       1     4
      11     0     4     4    NA    NA    NA     4  1.39      0     4       1     4
      12    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      13     1     0    NA     5     5     5    NA NA        NA    NA       1     5
      14     0     5     5    NA    NA    NA     5  1.61      0     5       1     5
      15    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      16     1     0    NA     6     6     6    NA NA        NA    NA       1     6
      17     0     6     6    NA    NA    NA     6  1.79      0     6       1     6
      18    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      19     1     0    NA     7     7     7    NA NA        NA    NA       1     7
      20     0     7     7    NA    NA    NA     7  1.95      0     7       1     7
      21    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      22     1     0    NA     8     8     8    NA NA        NA    NA       1     8
      23     0     8     8    NA    NA    NA     8  2.08      0     8       1     8
      24    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      25     1     0    NA     9     9     9    NA NA        NA    NA       1     9
      26     0     9     9    NA    NA    NA     9  2.20      0     9       1     9
      27    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      28     1     0    NA    10    10    10    NA NA        NA    NA       1    10
      29     0    10    10    NA    NA    NA    10  2.30      0    10       1    10
      30    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
         NROUTE  NFRQ  NSEX NRACE NETHNIC  BAGE  BALT  BAST BBILI BCREAT BHEIGHT
          <int> <int> <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl>
       1      1     1     1     3       0     5    21    22     5     60     180
       2      1     1     1     3       0     5    21    22     5     60     180
       3     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
       4      2     2     1     1       0    20    25    18     7     70     150
       5      2     2     1     1       0    20    25    18     7     70     150
       6     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
       7      3     3     1     4       0    41    30    13    10     61     170
       8      3     3     1     4       0    41    30    13    10     61     170
       9     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
      10      4     4     1     4       1    90    26    19    15     73     161
      11      4     4     1     4       1    90    26    19    15     73     161
      12     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
      13      5     5     1     1       0    71    27    22    17     80     181
      14      5     5     1     1       0    71    27    22    17     80     181
      15     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
      16      6     6     1     3       0    52    21    20     9     81     167
      17      6     6     1     3       0    52    21    20     9     81     167
      18     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
      19      7     7     1     3       0    34    23    25    10     69     187
      20      7     7     1     3       0    34    23    25    10     69     187
      21     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
      22      8     8     1     1       0    15    22    21     8     68     150
      23      8     8     1     1       0    15    22    21     8     68     150
      24     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
      25      9     9     1     4       0     4    22    15    14     71     164
      26      9     9     1     4       0     4    22    15    14     71     164
      27     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
      28     10    10     1     1       1    20    27    16    11     83     190
      29     10    10     1     1       1    20    27    16    11     83     190
      30     NA    NA    NA    NA      NA    NA    NA    NA    NA     NA      NA
         BWEIGHT  TAST  TALT PDOSEF TIMEF  AMTF  DUPF NOEXF NODV1F NODV2F NODV3F
           <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
       1    80.2    19    60      0     0     0     0     0      0      1      1
       2    80.2    19    60      0     0     0     0     0      0      1      1
       3    NA      19    60      0     0     0     0     0      0      1      1
       4    70.1    13    32      0     0     0     0     0      1      0      1
       5    70.1    13    32      0     0     0     0     0      1      0      1
       6    NA      13    32      0     0     0     0     0      1      0      1
       7    63.7    16    20      0     0     0     0     0      1      1      0
       8    63.7    16    20      0     0     0     0     0      1      1      0
       9    NA      16    20      0     0     0     0     0      1      1      0
      10    45.7    20    41      0     0     0     0     0      1      1      1
      11    45.7    20    41      0     0     0     0     0      1      1      1
      12    NA      20    41      0     0     0     0     0      1      1      1
      13    90.6    21    30      0     0     0     0     0      1      1      1
      14    90.6    21    30      0     0     0     0     0      1      1      1
      15    NA      21    30      0     0     0     0     0      1      1      1
      16    70.2    25    62      0     0     0     0     0      1      1      1
      17    70.2    25    62      0     0     0     0     0      1      1      1
      18    NA      25    62      0     0     0     0     0      1      1      1
      19    80.4    21    50      0     0     0     0     0      1      1      1
      20    80.4    21    50      0     0     0     0     0      1      1      1
      21    NA      21    50      0     0     0     0     0      1      1      1
      22    90      18    28      0     0     0     0     0      1      1      1
      23    90      18    28      0     0     0     0     0      1      1      1
      24    NA      18    28      0     0     0     0     0      1      1      1
      25    99.2    22    33      0     0     0     0     0      1      1      1
      26    99.2    22    33      0     0     0     0     0      1      1      1
      27    NA      22    33      0     0     0     0     0      1      1      1
      28   100      20    46      0     0     0     0     0      1      1      1
      29   100      20    46      0     0     0     0     0      1      1      1
      30    NA      20    46      0     0     0     0     0      1      1      1
         NODV4F NODV5F NODV6F NODV7F NODV8F NODV9F NODV10F   SDF PLBOF SPARSEF TREXF
          <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>   <dbl> <dbl>
       1      1      1      1      1      1      1       1     0     0       1     0
       2      1      1      1      1      1      1       1     0     0       1     0
       3      1      1      1      1      1      1       1     0    NA       1     0
       4      1      1      1      1      1      1       1     0     0       1     0
       5      1      1      1      1      1      1       1     0     0       1     0
       6      1      1      1      1      1      1       1     0    NA       1     0
       7      1      1      1      1      1      1       1     0     0       1     0
       8      1      1      1      1      1      1       1     0     0       1     0
       9      1      1      1      1      1      1       1     0    NA       1     0
      10      0      1      1      1      1      1       1     0     0       1     0
      11      0      1      1      1      1      1       1     0     0       1     0
      12      0      1      1      1      1      1       1     0    NA       1     0
      13      1      0      1      1      1      1       1     0     0       1     0
      14      1      0      1      1      1      1       1     0     0       1     0
      15      1      0      1      1      1      1       1     0    NA       1     0
      16      1      1      0      1      1      1       1     0     0       1     0
      17      1      1      0      1      1      1       1     0     0       1     0
      18      1      1      0      1      1      1       1     0    NA       1     0
      19      1      1      1      0      1      1       1     0     0       1     0
      20      1      1      1      0      1      1       1     0     0       1     0
      21      1      1      1      0      1      1       1     0    NA       1     0
      22      1      1      1      1      0      1       1     0     0       1     0
      23      1      1      1      1      0      1       1     0     0       1     0
      24      1      1      1      1      0      1       1     0    NA       1     0
      25      1      1      1      1      1      0       1     0     0       1     0
      26      1      1      1      1      1      0       1     0     0       1     0
      27      1      1      1      1      1      0       1     0    NA       1     0
      28      1      1      1      1      1      1       0     0     0       1     0
      29      1      1      1      1      1      1       0     0     0       1     0
      30      1      1      1      1      1      1       0     0    NA       1     0
         IMPEX IMPDV  LINE USUBJID NSTUDYC VISIT TPTC  DOMAIN DVIDC DVIDU TIMEU
         <dbl> <dbl> <int> <chr>   <chr>   <chr> <chr> <chr>  <chr> <chr> <chr>
       1     0    NA     1 a       a       a     a     EX     a     a     days 
       2     0     0     2 a       a       a     a     PD     a     a     days 
       3    NA    NA     3 a       a       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
       4     0    NA     4 b       b       b     b     EX     b     b     days 
       5     0     0     5 b       b       b     b     PD     b     b     days 
       6    NA    NA     6 b       b       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
       7     0    NA     7 c       c       c     c     EX     c     c     days 
       8     0     0     8 c       c       c     c     PD     c     c     days 
       9    NA    NA     9 c       c       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
      10     0    NA    10 d       d       d     d     EX     d     d     days 
      11     0     0    11 d       d       d     d     PD     d     d     days 
      12    NA    NA    12 d       d       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
      13     0    NA    13 e       e       e     e     EX     e     e     days 
      14     0     0    14 e       e       e     e     PD     e     e     days 
      15    NA    NA    15 e       e       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
      16     0    NA    16 f       f       f     f     EX     f     f     days 
      17     0     0    17 f       f       f     f     PD     f     f     days 
      18    NA    NA    18 f       f       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
      19     0    NA    19 g       g       g     g     EX     g     g     days 
      20     0     0    20 g       g       g     g     PD     g     g     days 
      21    NA    NA    21 g       g       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
      22     0    NA    22 h       h       h     h     EX     h     h     days 
      23     0     0    23 h       h       h     h     PD     h     h     days 
      24    NA    NA    24 h       h       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
      25     0    NA    25 i       i       i     i     EX     i     i     days 
      26     0     0    26 i       i       i     i     PD     i     i     days 
      27    NA    NA    27 i       i       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
      28     0    NA    28 j       j       j     j     EX     j     j     days 
      29     0     0    29 j       j       j     j     PD     j     j     days 
      30    NA    NA    30 j       j       <NA>  <NA>  TVCOV  <NA>  <NA>  days 
         NROUTEC NFRQC NSEXC NRACEC                           NETHNICC              
         <chr>   <chr> <chr> <chr>                            <chr>                 
       1 a       a     F     ASIAN                            NOT HISPANIC OR LATINO
       2 a       a     F     ASIAN                            NOT HISPANIC OR LATINO
       3 <NA>    <NA>  <NA>  <NA>                             <NA>                  
       4 b       b     F     WHITE                            NOT HISPANIC OR LATINO
       5 b       b     F     WHITE                            NOT HISPANIC OR LATINO
       6 <NA>    <NA>  <NA>  <NA>                             <NA>                  
       7 c       c     F     AMERICAN INDIAN OR ALASKA NATIVE NOT HISPANIC OR LATINO
       8 c       c     F     AMERICAN INDIAN OR ALASKA NATIVE NOT HISPANIC OR LATINO
       9 <NA>    <NA>  <NA>  <NA>                             <NA>                  
      10 d       d     F     AMERICAN INDIAN OR ALASKA NATIVE HISPANIC OR LATINO    
      11 d       d     F     AMERICAN INDIAN OR ALASKA NATIVE HISPANIC OR LATINO    
      12 <NA>    <NA>  <NA>  <NA>                             <NA>                  
      13 e       e     F     WHITE                            NOT HISPANIC OR LATINO
      14 e       e     F     WHITE                            NOT HISPANIC OR LATINO
      15 <NA>    <NA>  <NA>  <NA>                             <NA>                  
      16 f       f     F     ASIAN                            NOT HISPANIC OR LATINO
      17 f       f     F     ASIAN                            NOT HISPANIC OR LATINO
      18 <NA>    <NA>  <NA>  <NA>                             <NA>                  
      19 g       g     F     ASIAN                            NOT HISPANIC OR LATINO
      20 g       g     F     ASIAN                            NOT HISPANIC OR LATINO
      21 <NA>    <NA>  <NA>  <NA>                             <NA>                  
      22 h       h     F     WHITE                            NOT HISPANIC OR LATINO
      23 h       h     F     WHITE                            NOT HISPANIC OR LATINO
      24 <NA>    <NA>  <NA>  <NA>                             <NA>                  
      25 i       i     F     AMERICAN INDIAN OR ALASKA NATIVE NOT HISPANIC OR LATINO
      26 i       i     F     AMERICAN INDIAN OR ALASKA NATIVE NOT HISPANIC OR LATINO
      27 <NA>    <NA>  <NA>  <NA>                             <NA>                  
      28 j       j     F     WHITE                            HISPANIC OR LATINO    
      29 j       j     F     WHITE                            HISPANIC OR LATINO    
      30 <NA>    <NA>  <NA>  <NA>                             <NA>                  
         BAGEU BALTU BASTU BBILIU BCREATU BHEIGHTU BWEIGHTU TASTU TALTU
         <chr> <chr> <chr> <chr>  <chr>   <chr>    <chr>    <chr> <chr>
       1 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
       2 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
       3 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
       4 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
       5 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
       6 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
       7 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
       8 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
       9 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
      10 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      11 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      12 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
      13 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      14 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      15 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
      16 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      17 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      18 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
      19 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      20 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      21 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
      22 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      23 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      24 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
      25 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      26 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      27 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
      28 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      29 Years IU/L  IU/L  mg/dL  mg/dL   cm       kg       IU/L  IU/L 
      30 <NA>  <NA>  <NA>  <NA>   <NA>    <NA>     <NA>     IU/L  IU/L 
         DTIM                FDOSE              
         <chr>               <chr>              
       1 2020-07-04 18:00:00 2020-07-04 18:00:00
       2 2026-03-17 09:30:00 2020-07-04 18:00:00
       3 2046-08-01 08:45:00 2020-07-04 18:00:00
       4 2021-01-01 00:00:01 2021-01-01 00:00:01
       5 2027-07-21 14:00:00 2021-01-01 00:00:01
       6 2047-03-12 18:05:00 2021-01-01 00:00:01
       7 2021-12-31 23:59:59 2021-12-31 23:59:59
       8 2028-01-15 00:00:02 2021-12-31 23:59:59
       9 2048-06-14 02:15:03 2021-12-31 23:59:59
      10 2022-02-14 20:00:00 2022-02-14 20:00:00
      11 2029-09-09 12:12:12 2022-02-14 20:00:00
      12 2049-10-08 04:44:13 2022-02-14 20:00:00
      13 2022-11-11 11:11:11 2022-11-11 11:11:11
      14 2030-04-04 15:30:00 2022-11-11 11:11:11
      15 2050-01-01 10:15:00 2022-11-11 11:11:11
      16 2023-05-17 08:30:00 2023-05-17 08:30:00
      17 2031-08-15 19:00:00 2023-05-17 08:30:00
      18 2051-11-16 13:05:00 2023-05-17 08:30:00
      19 2023-10-31 23:59:59 2023-10-31 23:59:59
      20 2032-11-01 00:59:59 2023-10-31 23:59:59
      21 2052-05-02 04:19:58 2023-10-31 23:59:59
      22 2024-02-29 14:29:00 2024-02-29 14:29:00
      23 2033-03-03 21:00:00 2024-02-29 14:29:00
      24 2053-04-04 16:25:00 2024-02-29 14:29:00
      25 2025-06-30 13:00:00 2025-06-30 13:00:00
      26 2034-12-30 23:59:59 2025-06-30 13:00:00
      27 2054-12-20 05:59:58 2025-06-30 13:00:00
      28 2025-12-25 12:00:00 2025-12-25 12:00:00
      29 2035-05-31 14:00:00 2025-12-25 12:00:00
      30 2055-07-07 09:05:00 2025-12-25 12:00:00


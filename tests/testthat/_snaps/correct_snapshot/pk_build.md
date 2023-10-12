# Snapshots

    Code
      print(pkdf, n = Inf, width = Inf)
    Output
      # A tibble: 30 × 67
         C     NSTUDY SUBJID    ID   ATFD  ATLD  NTFD  NTLC  NTLD  NDAY   TPT  EVID
         <chr>  <int> <chr>  <int>  <dbl> <dbl> <dbl> <dbl> <dbl> <int> <dbl> <dbl>
       1 <NA>       1 ""         1     0     0      1     1     0     1     1     1
       2 <NA>       1 ""         1  5764. 5763.     1     1     0     1     1     0
       3 <NA>       1 ""         1  9524.   NA     NA    NA    NA    NA    NA     2
       4 <NA>       2 ""         2     0     0      3     3     0     2     2     1
       5 <NA>       2 ""         2  6075. 6071.     3     3     0     2     2     0
       6 <NA>       2 ""         2  9567.   NA     NA    NA    NA    NA    NA     2
       7 <NA>       3 ""         3     0     0      5     5     0     3     3     1
       8 <NA>       3 ""         3  5891. 5882.     5     5     0     3     3     0
       9 <NA>       3 ""         3  9661.   NA     NA    NA    NA    NA    NA     2
      10 <NA>       4 ""         4     0     0      7     7     0     4     4     1
      11 <NA>       4 ""         4  6445. 6429.     7     7     0     4     4     0
      12 <NA>       4 ""         4 10097.   NA     NA    NA    NA    NA    NA     2
      13 <NA>       5 ""         5     0     0      9     9     0     5     5     1
      14 <NA>       5 ""         5  6385. 6360.     9     9     0     5     5     0
      15 <NA>       5 ""         5  9913.   NA     NA    NA    NA    NA    NA     2
      16 <NA>       6 ""         6     0     0     11    11     0     6     6     1
      17 <NA>       6 ""         6  6697. 6661.    11    11     0     6     6     0
      18 <NA>       6 ""         6 10410.   NA     NA    NA    NA    NA    NA     2
      19 <NA>       7 ""         7     0     0     13    13     0     7     7     1
      20 <NA>       7 ""         7  6941. 6892.    13    13     0     7     7     0
      21 <NA>       7 ""         7 10410.   NA     NA    NA    NA    NA    NA     2
      22 <NA>       8 ""         8     0     0     15    15     0     8     8     1
      23 <NA>       8 ""         8  6974. 6910.    15    15     0     8     8     0
      24 <NA>       8 ""         8 10627.   NA     NA    NA    NA    NA    NA     2
      25 <NA>       9 ""         9     0     0     17    17     0     9     9     1
      26 <NA>       9 ""         9  7122. 7040.    17    17     0     9     9     0
      27 <NA>       9 ""         9 10765.   NA     NA    NA    NA    NA    NA     2
      28 <NA>      10 ""        10     0     0     19    19     0    10    10     1
      29 <NA>      10 ""        10  7128. 7028.    19    19     0    10    10     0
      30 <NA>      10 ""        10 10786.   NA     NA    NA    NA    NA    NA     2
           MDV   CMT  DVID   AMT  ADDL    II   ODV    LDV   BLQ  LLOQ DOSENUM DOSEA
         <dbl> <dbl> <int> <int> <int> <int> <int>  <dbl> <dbl> <int>   <dbl> <int>
       1     1     0    NA     1     1     1    NA NA        NA    NA       1     1
       2     0     1     1    NA    NA    NA     2  0.693     0     1       1     1
       3    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
       4     1     0    NA     2     2     2    NA NA        NA    NA       1     2
       5     0     2     2    NA    NA    NA     3  1.10      0     2       1     2
       6    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
       7     1     0    NA     3     3     3    NA NA        NA    NA       1     3
       8     0     3     3    NA    NA    NA     4  1.39      0     3       1     3
       9    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      10     1     0    NA     4     4     4    NA NA        NA    NA       1     4
      11     0     4     4    NA    NA    NA     5  1.61      0     4       1     4
      12    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      13     1     0    NA     5     5     5    NA NA        NA    NA       1     5
      14     0     5     5    NA    NA    NA     6  1.79      0     5       1     5
      15    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      16     1     0    NA     6     6     6    NA NA        NA    NA       1     6
      17     0     6     6    NA    NA    NA     7  1.95      0     6       1     6
      18    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      19     1     0    NA     7     7     7    NA NA        NA    NA       1     7
      20     0     7     7    NA    NA    NA     8  2.08      0     7       1     7
      21    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      22     1     0    NA     8     8     8    NA NA        NA    NA       1     8
      23     0     8     8    NA    NA    NA     9  2.20      0     8       1     8
      24    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      25     1     0    NA     9     9     9    NA NA        NA    NA       1     9
      26     0     9     9    NA    NA    NA    10  2.30      0     9       1     9
      27    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
      28     1     0    NA    10    10    10    NA NA        NA    NA       1    10
      29     0    10    10    NA    NA    NA    11  2.40      0    10       1    10
      30    NA    NA    NA    NA    NA    NA    NA NA        NA    NA      NA    NA
         NROUTE  NFRQ STUDY  TAST  TALT PDOSEF TIMEF  AMTF  DUPF NOEXF NODV1F NODV2F
          <int> <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>
       1      1     1  -999    19    60      0     0     0     0     0      0      1
       2      1     1     1    19    60      0     0     0     0     0      0      1
       3     NA    NA    NA    19    60      0     0     0     0     0      0      1
       4      2     2  -999    13    32      0     0     0     0     0      1      0
       5      2     2     2    13    32      0     0     0     0     0      1      0
       6     NA    NA    NA    13    32      0     0     0     0     0      1      0
       7      3     3  -999    16    20      0     0     0     0     0      1      1
       8      3     3     3    16    20      0     0     0     0     0      1      1
       9     NA    NA    NA    16    20      0     0     0     0     0      1      1
      10      4     4  -999    20    41      0     0     0     0     0      1      1
      11      4     4     4    20    41      0     0     0     0     0      1      1
      12     NA    NA    NA    20    41      0     0     0     0     0      1      1
      13      5     5  -999    21    30      0     0     0     0     0      1      1
      14      5     5     5    21    30      0     0     0     0     0      1      1
      15     NA    NA    NA    21    30      0     0     0     0     0      1      1
      16      6     6  -999    25    62      0     0     0     0     0      1      1
      17      6     6     6    25    62      0     0     0     0     0      1      1
      18     NA    NA    NA    25    62      0     0     0     0     0      1      1
      19      7     7  -999    21    50      0     0     0     0     0      1      1
      20      7     7     7    21    50      0     0     0     0     0      1      1
      21     NA    NA    NA    21    50      0     0     0     0     0      1      1
      22      8     8  -999    18    28      0     0     0     0     0      1      1
      23      8     8     8    18    28      0     0     0     0     0      1      1
      24     NA    NA    NA    18    28      0     0     0     0     0      1      1
      25      9     9  -999    22    33      0     0     0     0     0      1      1
      26      9     9     9    22    33      0     0     0     0     0      1      1
      27     NA    NA    NA    22    33      0     0     0     0     0      1      1
      28     10    10  -999    20    46      0     0     0     0     0      1      1
      29     10    10    10    20    46      0     0     0     0     0      1      1
      30     NA    NA    NA    20    46      0     0     0     0     0      1      1
         NODV3F NODV4F NODV5F NODV6F NODV7F NODV8F NODV9F NODV10F   SDF PLBOF SPARSEF
          <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl>   <dbl>
       1      1      1      1      1      1      1      1       1     0     0       1
       2      1      1      1      1      1      1      1       1     0     0       1
       3      1      1      1      1      1      1      1       1     0    NA       1
       4      1      1      1      1      1      1      1       1     0     0       1
       5      1      1      1      1      1      1      1       1     0     0       1
       6      1      1      1      1      1      1      1       1     0    NA       1
       7      0      1      1      1      1      1      1       1     0     0       1
       8      0      1      1      1      1      1      1       1     0     0       1
       9      0      1      1      1      1      1      1       1     0    NA       1
      10      1      0      1      1      1      1      1       1     0     0       1
      11      1      0      1      1      1      1      1       1     0     0       1
      12      1      0      1      1      1      1      1       1     0    NA       1
      13      1      1      0      1      1      1      1       1     0     0       1
      14      1      1      0      1      1      1      1       1     0     0       1
      15      1      1      0      1      1      1      1       1     0    NA       1
      16      1      1      1      0      1      1      1       1     0     0       1
      17      1      1      1      0      1      1      1       1     0     0       1
      18      1      1      1      0      1      1      1       1     0    NA       1
      19      1      1      1      1      0      1      1       1     0     0       1
      20      1      1      1      1      0      1      1       1     0     0       1
      21      1      1      1      1      0      1      1       1     0    NA       1
      22      1      1      1      1      1      0      1       1     0     0       1
      23      1      1      1      1      1      0      1       1     0     0       1
      24      1      1      1      1      1      0      1       1     0    NA       1
      25      1      1      1      1      1      1      0       1     0     0       1
      26      1      1      1      1      1      1      0       1     0     0       1
      27      1      1      1      1      1      1      0       1     0    NA       1
      28      1      1      1      1      1      1      1       0     0     0       1
      29      1      1      1      1      1      1      1       0     0     0       1
      30      1      1      1      1      1      1      1       0     0    NA       1
         TREXF IMPEX IMPFEX IMPDV  LINE USUBJID NSTUDYC VISIT TPTC  DOMAIN DVIDC DVIDU
         <dbl> <dbl>  <dbl> <dbl> <int> <chr>   <chr>   <chr> <chr> <chr>  <chr> <chr>
       1     0     0      0    NA     1 a       A       a     a     EX     a     a    
       2     0     0      0     0     2 a       A       a     a     PC     a     a    
       3     0    NA     NA    NA     3 a       A       <NA>  <NA>  TVCOV  <NA>  <NA> 
       4     0     0      0    NA     4 b       B       b     b     EX     b     b    
       5     0     0      0     0     5 b       B       b     b     PC     b     b    
       6     0    NA     NA    NA     6 b       B       <NA>  <NA>  TVCOV  <NA>  <NA> 
       7     0     0      0    NA     7 c       C       c     c     EX     c     c    
       8     0     0      0     0     8 c       C       c     c     PC     c     c    
       9     0    NA     NA    NA     9 c       C       <NA>  <NA>  TVCOV  <NA>  <NA> 
      10     0     0      0    NA    10 d       D       d     d     EX     d     d    
      11     0     0      0     0    11 d       D       d     d     PC     d     d    
      12     0    NA     NA    NA    12 d       D       <NA>  <NA>  TVCOV  <NA>  <NA> 
      13     0     0      0    NA    13 e       E       e     e     EX     e     e    
      14     0     0      0     0    14 e       E       e     e     PC     e     e    
      15     0    NA     NA    NA    15 e       E       <NA>  <NA>  TVCOV  <NA>  <NA> 
      16     0     0      0    NA    16 f       F       f     f     EX     f     f    
      17     0     0      0     0    17 f       F       f     f     PC     f     f    
      18     0    NA     NA    NA    18 f       F       <NA>  <NA>  TVCOV  <NA>  <NA> 
      19     0     0      0    NA    19 g       G       g     g     EX     g     g    
      20     0     0      0     0    20 g       G       g     g     PC     g     g    
      21     0    NA     NA    NA    21 g       G       <NA>  <NA>  TVCOV  <NA>  <NA> 
      22     0     0      0    NA    22 h       H       h     h     EX     h     h    
      23     0     0      0     0    23 h       H       h     h     PC     h     h    
      24     0    NA     NA    NA    24 h       H       <NA>  <NA>  TVCOV  <NA>  <NA> 
      25     0     0      0    NA    25 i       I       i     i     EX     i     i    
      26     0     0      0     0    26 i       I       i     i     PC     i     i    
      27     0    NA     NA    NA    27 i       I       <NA>  <NA>  TVCOV  <NA>  <NA> 
      28     0     0      0    NA    28 j       J       j     j     EX     j     j    
      29     0     0      0     0    29 j       J       j     j     PC     j     j    
      30     0    NA     NA    NA    30 j       J       <NA>  <NA>  TVCOV  <NA>  <NA> 
         TIMEU NROUTEC NFRQC TASTU TALTU DTIM                FDOSE               VERSN
         <chr> <chr>   <chr> <chr> <chr> <chr>               <chr>               <chr>
       1 days  A       A     IU/L  IU/L  2020-07-04 18:00:00 2020-07-04 18:00:00 0.3.4
       2 days  A       A     IU/L  IU/L  2036-04-15 10:33:00 2020-07-04 18:00:00 0.3.4
       3 days  <NA>    <NA>  IU/L  IU/L  2046-08-01 08:45:00 2020-07-04 18:00:00 0.3.4
       4 days  B       B     IU/L  IU/L  2021-01-01 00:00:01 2021-01-01 00:00:01 0.3.4
       5 days  B       B     IU/L  IU/L  2037-08-19 15:07:00 2021-01-01 00:00:01 0.3.4
       6 days  <NA>    <NA>  IU/L  IU/L  2047-03-12 18:05:00 2021-01-01 00:00:01 0.3.4
       7 days  C       C     IU/L  IU/L  2021-12-31 23:59:59 2021-12-31 23:59:59 0.3.4
       8 days  C       C     IU/L  IU/L  2038-02-17 01:01:03 2021-12-31 23:59:59 0.3.4
       9 days  <NA>    <NA>  IU/L  IU/L  2048-06-14 02:15:03 2021-12-31 23:59:59 0.3.4
      10 days  D       D     IU/L  IU/L  2022-02-14 20:00:00 2022-02-14 20:00:00 0.3.4
      11 days  D       D     IU/L  IU/L  2039-10-08 13:14:13 2022-02-14 20:00:00 0.3.4
      12 days  <NA>    <NA>  IU/L  IU/L  2049-10-08 04:44:13 2022-02-14 20:00:00 0.3.4
      13 days  E       E     IU/L  IU/L  2022-11-11 11:11:11 2022-11-11 11:11:11 0.3.4
      14 days  E       E     IU/L  IU/L  2040-05-05 16:35:00 2022-11-11 11:11:11 0.3.4
      15 days  <NA>    <NA>  IU/L  IU/L  2050-01-01 10:15:00 2022-11-11 11:11:11 0.3.4
      16 days  F       F     IU/L  IU/L  2023-05-17 08:30:00 2023-05-17 08:30:00 0.3.4
      17 days  F       F     IU/L  IU/L  2041-09-16 20:07:00 2023-05-17 08:30:00 0.3.4
      18 days  <NA>    <NA>  IU/L  IU/L  2051-11-16 13:05:00 2023-05-17 08:30:00 0.3.4
      19 days  G       G     IU/L  IU/L  2023-10-31 23:59:59 2023-10-31 23:59:59 0.3.4
      20 days  G       G     IU/L  IU/L  2042-11-02 01:59:58 2023-10-31 23:59:59 0.3.4
      21 days  <NA>    <NA>  IU/L  IU/L  2052-05-02 04:19:58 2023-10-31 23:59:59 0.3.4
      22 days  H       H     IU/L  IU/L  2024-02-29 14:29:00 2024-02-29 14:29:00 0.3.4
      23 days  H       H     IU/L  IU/L  2043-04-04 22:07:00 2024-02-29 14:29:00 0.3.4
      24 days  <NA>    <NA>  IU/L  IU/L  2053-04-04 16:25:00 2024-02-29 14:29:00 0.3.4
      25 days  I       I     IU/L  IU/L  2025-06-30 13:00:00 2025-06-30 13:00:00 0.3.4
      26 days  I       I     IU/L  IU/L  2044-12-29 00:59:58 2025-06-30 13:00:00 0.3.4
      27 days  <NA>    <NA>  IU/L  IU/L  2054-12-20 05:59:58 2025-06-30 13:00:00 0.3.4
      28 days  J       J     IU/L  IU/L  2025-12-25 12:00:00 2025-12-25 12:00:00 0.3.4
      29 days  J       J     IU/L  IU/L  2045-07-01 15:07:00 2025-12-25 12:00:00 0.3.4
      30 days  <NA>    <NA>  IU/L  IU/L  2055-07-07 09:05:00 2025-12-25 12:00:00 0.3.4


# Testing the residual-based shading scheme

Ran the first test in `quick-test.R`


## Error: Use of `%>%` rather than native pipe `|>`

```
Warning message:
  Computation failed in `stat_mosaic()`.
Caused by error in `data %>% dplyr::select(dplyr::all_of(c(vars, ".n"))) %>% dplyr::distinct()`:
  ! could not find function "%>%"
```
* I corrected this. Anything in this package should use the native pipe `|>` rather than magrittr `%>%`

## `stat_mosaic()` error

```
Warning message:
Computation failed in `stat_mosaic()`.
Caused by error in `UseMethod()`:
! no applicable method for 'distinct' applied to an object of class "NULL" 
```

This is triggered from line 91 in R/loglinear.R

## Loglinear model fitting failed

```
Warning message:
Loglinear model fitting failed: replacement has 8 rows, data has 10
Proceeding without residual shading. 
```

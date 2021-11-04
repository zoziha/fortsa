# `pacf`

## Description

Partial Auto Covariance Function.

## Class

Pure subroutine.

## Synatx

```fortran
call fortsa_stats:pacf(vec, n, par, m)
```

## Arguments

`vec`: Shall be a `real(c_double)` and `rank-1` array.
This argument is `intent(in)`.

`par`: Shall be a `real(c_double)` and `rank-1` array.
This argument is `intent(out)`.

`n`, `m`: Shall be a `integer(c_int)` scalar.
This argument is `value` and `intent(in)`.
`n = size(vec)`, `m = size(par)`.

# `acvf_opt`

## Description

Method 0 : Yule-Walker<br>
Method 1 : Burg<br>
Method 2 : Box-Jenkins Conditional MLE

## Class

Impure subroutine.

## Synatx

```fortran
call fortsa_stats:pacf_opt(vec, n, method, par, m)
```

## Arguments

`vec`: Shall be a `real(c_double)` and `rank-1` array.
This argument is `intent(in)`.

`par`: Shall be a `real(c_double)` and `rank-1` array.
This argument is `intent(out)`.

`method`: Shall be a `integer(c_int)` scalar.
This argument is `value` and `intent(in)`.

`n`, `m`: Shall be a `integer(c_int)` scalar.
This argument is `value` and `intent(in)`.
`n = size(vec)`, `m = size(par)`.

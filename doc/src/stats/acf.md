# `acvf`

## Description

Auto Covariance Function.

## Class

Impure subroutine.

## Synatx

```fortran
call fortsa_stats:acvf(vec, n, par, m)
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

Method 0 : Regular Method. Slow for large input length.<br>
Method 1 : FFT based Method. Use it if data length is large.

## Class

Impure subroutine.

## Synatx

```fortran
call fortsa_stats:acvf_opt(vec, n, method, par, m)
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

# `acvf2acf`

## Description

Converts Autocovariance to autocorrelation function.

## Class

Pure subroutine.

## Synatx

```fortran
call fortsa_stats:acvf2acf(acf, m)
```

## Arguments

`acf`: Shall be a `real(c_double)` and `rank-1` array.
This argument is `intent(inout)`.

`m`: Shall be a `integer(c_int)` scalar.
This argument is `value` and `intent(in)`.
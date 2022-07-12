# fortsa: A ctsa/Fortran binding

> [中文](./README-CN.md) | English

## A Univariate Time Series Analysis and ARIMA Modeling Package in Fortran

`fortsa` is a Fortran binding package for univariate time series analysis, which is based on [rafat/ctsa](https://github.com/rafat/ctsa).

`ctsa` is a C software package for univariate time series analysis.

## Get Started

```sh
git clone https://github.com/zoziha/fortsa.git
cd fortsa
```
### Dependencies

- [Fortran-lang/fpm][1] >= 0.6.0: for building `fortsa`.
- [GNU/GCC][2] >= 9.4.0: for compiling `fortsa`.

[1]: https://github.com/fortran-lang/fpm
[2]: https://gcc.gnu.org/

### Build with [Fortran-lang/fpm][1]
Fortran Package Manager (fpm) is a package manager and build system for Fortran.<br>
You can build using provided `fpm.toml`:

```sh
fpm build --profile release
```

To use `fortsa` within your fpm project, add the following to `fpm.toml` file:

```toml
[dependencies]
fortsa = { git = "https://github.com/zoziha/fortsa" }
```
## `ctsa` Docs

|**[Auto ARIMA](https://github.com/rafat/ctsa/wiki/AUTO-ARIMA)**| Auto ARIMA Class + Examples        |
|:-----------------------------------------------------|:----------------------------------|
|**[SARIMAX](https://github.com/rafat/ctsa/wiki/SARIMAX/)**| SARIMAX Class + Examples             |
|**[ARIMA](https://github.com/rafat/ctsa/wiki/ARIMA)**| ARIMA Class + Example             |
|**[Seasonal ARIMA](https://github.com/rafat/ctsa/wiki/SARIMA)**| Seasonal ARIMA Class + Example    |
|**[AR](https://github.com/rafat/ctsa/wiki/AR)**      | AR Class + Example                |
|**[ACF](https://github.com/rafat/ctsa/wiki/ACF)**    | Autocovariance, Autocorrelation and Partial Autocorrelation + Examples|
|**[References](https://github.com/rafat/ctsa/wiki/References)**| References (List Being Updated)   |

Wiki is available at 

[ctsa/wiki](https://github.com/rafat/ctsa/wiki)

License : BSD 3- Clause Check LICENSE file

For `C` routines, contact rafat.hsn@gmail.com.  
For `Fortran` routines, contact zuo.zhihua@qq.com.

## Links

1. [rafat/ctsa](https://github.com/rafat/ctsa)
2. [fortran-lang](https://fortran-lang.org/learn/)
3. [fortran-lang/fpm](https://github.com/fortran-lang/fpm)

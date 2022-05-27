# ForTSA: A CTSA/Fortran binding

## A Univariate Time Series Analysis and ARIMA Modeling Package in Fortran

`ForTSA` is a Fortran software package for univariate time series analysis, which is base on [rafat/CTSA](https://github.com/rafat/ctsa).  
`CTSA` is a C software package for univariate time series analysis. 

<div align="center">
<img src="doc/src/media/logo.png" alt="ForTSA" width="220">
</div>

| Item | Info |  
|:-:|:-:|
| **Version:** | 0.1.0 |
| **Author:** | `ForTSA` Contributors |
| **Web site:** | https://github.com/zoziha/fortsa |
| **API-Doc Web site:** | https://zoziha.github.io/fortsa/ |
| **License:** | _`ForTSA`_ is released under BSD-3. |

## Get Started

```sh
git clone https://github.com/zoziha/fortsa.git
cd fortsa
```
## Dependencies

- Git
- [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
- [rafat/ctsa](https://github.com/rafat/ctsa)

### Supported Compilers
The following combinations are tested on the default branch of `ForTSA`:  

|Name|Vesrion|Platform|Architecture|  
|:-:|:-:|:-:|:-:|  
|GCC Fortran(MSYS2)|10|Windows 10|x86_64|
|GCC Fortran|10|Ubuntu|x86_64|

### Build with [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
Fortran Package Manager (fpm) is a package manager and build system for Fortran.<br>
You can build using provided `fpm.toml`:

```sh
fpm test
```

To use `ForTSA` within your fpm project, add the following to `fpm.toml` file:

```toml
[dependencies]
fortsa = { git="https://github.com/zoziha/fortsa.git" }
```
## `CTSA` Docs

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
2. [vmagnin/gtk-fortran](https://github.com/vmagnin/gtk-fortran)
3. [fortran-lang](https://fortran-lang.org/learn/)
4. [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
5. [zoziha/forlab](https://github.com/zoziha/forlab)
6. [toml](https://toml.io/en/)

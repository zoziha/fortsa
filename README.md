# FORTRAN-TSA
This package is still **under development**, please **do not use** this package as a production tool.

## A Univariate Time Series Analysis and ARIMA Modeling Package in Fortran

Fortran-TSA is a Fortran software package for univariate time series analysis, which is base on [rafat/CTSA](https://github.com/rafat/ctsa).  
CTSA is a C software package for univariate time series analysis. 
| | |  
|:-:|---|
| **Version:** | 0.0.1 |
| **Author:** | Fortran-TSA Contributors |
| **Web site:** | https://github.com/zoziha/fortran-tsa |
| **API-Doc Web site:** | \todo: |
| **Copyright:** | _This document_ has been placed in the public domain. |
| **License:** | _Fortran-TSA_ is released under (BSD 3- Clause Check COPYRIGHT file). |

## Getting Started
```bash
git clone https://github.com/zoziha/fortran-tsa.git
cd fortran-tsa
```
## Dependencies

Git and [fortran-lang/fpm](https://github.com/fortran-lang/fpm)

### Supported Compilers
The following combinations are tested on the default branch of `forlab`:  
|Name|Vesrion|Platform|Architecture|  
|---|---|---|---|  
|GCC Fortran(MSYS2)|10|Windows 10|x86_64|

### Build with [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
You can build using provided `fpm.toml`:
```bash
fpm build
fpm test
```
To use `fortran-tsa` within your fpm project, add the following to `fpm.toml` file:
```toml
[dependencies]
forlab = { git = "https://github.com/zoziha/fortran-tsa.git" }
```
## Fortran Docs

|**[Auto ARIMA](https://github.com/rafat/ctsa/wiki/AUTO-ARIMA)**| Auto ARIMA Class + Examples        |
|:-----------------------------------------------------|:----------------------------------|
|**[SARIMAX](https://github.com/rafat/ctsa/wiki/SARIMAX/)**| SARIMAX Class + Examples             |
|**[ARIMA](https://github.com/rafat/ctsa/wiki/ARIMA)**| ARIMA Class + Example             |
|**[Seasonal ARIMA](https://github.com/rafat/ctsa/wiki/SARIMA)**| Seasonal ARIMA Class + Example    |
|**[AR](https://github.com/rafat/ctsa/wiki/AR)**      | AR Class + Example                |
|**[ACF](https://github.com/rafat/ctsa/wiki/ACF)**    | Autocovariance, Autocorrelation and Partial Autocorrelation + Examples|
|**[References](https://github.com/rafat/ctsa/wiki/References)**| References (List Being Updated)   |

Wiki is available at 

https://github.com/rafat/ctsa/wiki

License : BSD 3- Clause Check LICENSE file

Contact rafat.hsn@gmail.com

## Links
1. [rafat/ctsa](https://github.com/rafat/ctsa)
2. [vmagnin/gtk-fortran](https://github.com/vmagnin/gtk-fortran)
3. [fortran-lang](https://fortran-lang.org/learn/)
4. [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
5. [zoziha/forlab](https://github.com/zoziha/forlab)
6. [toml](https://toml.io/en/)

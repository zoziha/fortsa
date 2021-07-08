
2021-07-06  zoziha  zuo.zhihua@qq.com

    Add `CTSA` Fortran interface.

    * src/ctsa/*.c:
    * src/fortsa_dwt.f90:
    * src/fortsa_model.f90:
    * src/fortsa_stats.f90:

    * tests/ctsa/*.c
    * tests/dwt/*.f90
    * tests/model/*.f90
    * tests/stats/*.f90

2021-07-08  zoziha  zuo.zhihua@qq.com

    Improve Fortsa API, change `type(c_ptr)` to more specific types.

    * src/fortsa_dwt.f90
    * src/fortsa_model.f90: `auto_arima_exec` `optional` scheme.
    * src/fortsa_stats.f90:

    * tests/ctsa/*.c
    * tests/dwt/*.f90
    * tests/model/*.f90: 
    * tests/stats/*.f90


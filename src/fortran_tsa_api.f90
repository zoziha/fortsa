!! `Fortran-TSA-api.f90` will be the `C-Fortran` interface to [rafat/CTSA](https://github.com/rafat/ctsa)

module fortran_tsa
    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char, c_ptr
    implicit none
    private

    public :: acvf, acvf_opt, acvf2acf

    public :: arima_set, arima_init, arima_setMethod,arima_setOptMethod, &
              arima_exec, arima_summary, arima_predict, arima_free
    public :: ar_init, ar_exec, &
                ar_summary, ar_predict, &
                ar_free

    public :: auto_arima_init,auto_arima_setApproximation, auto_arima_exec, &
                auto_arima_summary, auto_arima_predict, &
                auto_arima_free, auto_arima_setStepwise

    public :: yw, burg, hr

    !* CTSA_H_ *!
    type, bind(c) :: auto_arima_set
        integer(kind=c_int) :: N        !! length of time series
        integer(kind=c_int) :: Nused    !! length of time series after differencing, Nused = N - d
        integer(kind=c_int) :: method
        integer(kind=c_int) :: optmethod
        integer(kind=c_int) :: pmax     !! Maximum size of phi
        integer(kind=c_int) :: dmax     !! Maximum Number of times the series is to be differenced
        integer(kind=c_int) :: qmax     !! Maximum size of theta
        integer(kind=c_int) :: pmax_    !! Maximum Size of seasonal phi
        integer(kind=c_int) :: dmax_    !! Maximum number of times the seasonal series is to be differenced
        integer(kind=c_int) :: qmax_    !! Maximum size of Seasonal Theta
        integer(kind=c_int) :: p        !! size of phi
        integer(kind=c_int) :: d        !! Number of times the series is to be differenced
        integer(kind=c_int) :: q        !! size of theta
        integer(kind=c_int) :: s        !! Seasonality/Period
        integer(kind=c_int) :: p_       !! Size of seasonal phi
        integer(kind=c_int) :: d_       !! The number of times the seasonal series is to be differenced
        integer(kind=c_int) :: q_       !! size of Seasonal Theta
        integer(kind=c_int) :: r        !! Number of exogenous variables
        integer(kind=c_int) :: M        !! M = 0 if mean is 0.0 else M = 1
        integer(kind=c_int) :: ncoeff   !! Total Number of Coefficients to be estimated
        real(kind=c_double) :: phi
        real(kind=c_double) :: theta
        real(kind=c_double) :: phi_
        real(kind=c_double) :: theta_
        real(kind=c_double) :: exog
        real(kind=c_double) :: vcov     !! Variance-Covariance Matrix Of length lvcov
        integer(kind=c_int) :: lvcov    !! length of VCOV
        real(kind=c_double) :: res
        real(kind=c_double) :: mean
        real(kind=c_double) :: var
        real(kind=c_double) :: loglik
        real(kind=c_double) :: ic
        integer(kind=c_int) :: retval
        integer(kind=c_int) :: start
        integer(kind=c_int) :: imean
        integer(kind=c_int) :: idrift
        integer(kind=c_int) :: stationary
        integer(kind=c_int) :: seasonal
        integer(kind=c_int) :: Order_max
        integer(kind=c_int) :: p_start
        integer(kind=c_int) :: q_start
        integer(kind=c_int) :: P_start_
        integer(kind=c_int) :: Q_start_
        character(kind=c_char), dimension(10) :: information_criteria
        integer(kind=c_int) :: stepwise
        integer(kind=c_int) :: num_models
        integer(kind=c_int) :: approximation
        integer(kind=c_int) :: verbose
        character(kind=c_char), dimension(10) :: test
        character(kind=c_char), dimension(10) :: type
        character(kind=c_char), dimension(10) :: seas
        real(kind=c_double) :: alpha_test
        real(kind=c_double) :: alpha_seas
        real(kind=c_double) :: lambda
        real(kind=c_double) :: sigma2
        real(kind=c_double) :: aic
        real(kind=c_double) :: bic
        real(kind=c_double) :: aicc
        real(kind=c_double), dimension(0) :: params
    end type

    interface
        function auto_arima_init(pdqmax, pdqmax_, s, r, N) bind(c, name='auto_arima_init')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: pdqmax, pdqmax_
            integer(kind=c_int), value :: s, r, N
            type(c_ptr) :: auto_arima_init
        end function
    end interface

    type, bind(c) :: sarimax_set
        integer(kind=c_int) :: N        !! length of time series
        integer(kind=c_int) :: Nused    !!length of time series after differencing, Nused = N - d
        integer(kind=c_int) :: method
        integer(kind=c_int) :: optmethod
        integer(kind=c_int) :: p        !! size of phi
        integer(kind=c_int) :: d        !! Number of times the series is to be differenced
        integer(kind=c_int) :: q        !!size of theta
        integer(kind=c_int) :: s        !! Seasonality/Period
        integer(kind=c_int) :: p_       !!Size of seasonal phi
        integer(kind=c_int) :: d_       !! The number of times the seasonal series is to be differenced
        integer(kind=c_int) :: q_       !!size of Seasonal Theta
        integer(kind=c_int) :: r        !! Number of exogenous variables
        integer(kind=c_int) :: M        !! M = 0 if mean is 0.0 else M = 1
        integer(kind=c_int) :: ncoeff   !! Total Number of Coefficients to be estimated
        real(kind=c_double) :: phi
        real(kind=c_double) :: theta
        real(kind=c_double) :: phi_
        real(kind=c_double) :: theta_
        real(kind=c_double) :: exog
        real(kind=c_double) :: vcov     !! Variance-Covariance Matrix Of length lvcov
        integer(kind=c_int) :: lvcov    !!length of VCOV
        real(kind=c_double) :: res
        real(kind=c_double) :: mean
        real(kind=c_double) :: var
        real(kind=c_double) :: loglik
        real(kind=c_double) :: aic
        integer(kind=c_int) :: retval
        integer(kind=c_int) :: start
        integer(kind=c_int) :: imean
        real(kind=c_double), dimension(0) :: params
    end type sarimax_set 

    interface 
        function sarimax_init(p, d, q, p_, d_, q_, s, r, imean, N) bind(c, name='sarimax_init')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value :: p, d, q, p_, d_, q_, s, r, imean, N
            type(c_ptr) :: sarimax_init
        end function sarimax_init
    end interface

    type, bind(c) :: arima_set
        integer(kind=c_int) :: N        !! length of time series
        integer(kind=c_int) :: Nused    !! length of time series after differencing, Nused = N - d
        integer(kind=c_int) :: method
        integer(kind=c_int) :: optmethod
        integer(kind=c_int) :: p        !! size of phi
        integer(kind=c_int) :: d        !! Number of times the series is to be differenced
        integer(kind=c_int) :: q        !! size of theta
        integer(kind=c_int) :: s        !! Seasonality/Period
        integer(kind=c_int) :: p_       !! Size of seasonal phi
        integer(kind=c_int) :: d_       !! The number of times the seasonal series is to be differenced
        integer(kind=c_int) :: q_       !! size of Seasonal Theta
        integer(kind=c_int) :: r        !! Number of exogenous variables
        integer(kind=c_int) :: M        !! M = 0 if mean is 0.0 else M = 1
        integer(kind=c_int) :: ncoeff   !! Total Number of Coefficients to be estimated
        type(c_ptr) :: phi
        type(c_ptr) :: theta
        type(c_ptr) :: phi_
        type(c_ptr) :: theta_
        type(c_ptr) :: exog
        type(c_ptr) :: vcov     !! Variance-Covariance Matrix Of length lvcov
        integer(kind=c_int) :: lvcov    !! length of VCOV
        type(c_ptr) :: res
        real(kind=c_double) :: mean
        real(kind=c_double) :: var
        real(kind=c_double) :: loglik
        real(kind=c_double) :: aic
        integer(kind=c_int) :: retval
        integer(kind=c_int) :: start
        integer(kind=c_int) :: imean
        real(kind=c_double), dimension(0) :: params
    end type

    ! type(c_ptr), bind(c, name='arima_object') :: arima_object

    interface
        function arima_init(p, d, q, N) bind(c, name='arima_init')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value :: p, d, q, N
            type(c_ptr) :: arima_init
        end function
    end interface

    type, bind(c) :: sarima_set
        integer(kind=c_int) :: N        !!  length of time series
        integer(kind=c_int) :: Nused    !! length of time series after differencing, Nused = N - d - s*D
        integer(kind=c_int) :: method 
        integer(kind=c_int) :: optmethod 
        integer(kind=c_int) :: p        !!  size of phi
        integer(kind=c_int) :: d        !!  Number of times the series is to be differenced
        integer(kind=c_int) :: q        !! size of theta
        integer(kind=c_int) :: s        !!  Seasonality/Period
        integer(kind=c_int) :: P_        !! Size of seasonal phi
        integer(kind=c_int) :: D_        !!  The number of times the seasonal series is to be differenced
        integer(kind=c_int) :: Q_        !! size of Seasonal Theta
        integer(kind=c_int) :: M        !!  M = 0 if mean is 0.0 else M = 1
        integer(kind=c_int) :: ncoeff   !!  Total Number of Coefficients to be estimated
        real(kind=c_double) :: phi 
        real(kind=c_double) :: theta 
        real(kind=c_double) :: PHI_ 
        real(kind=c_double) :: THETA_ 
        real(kind=c_double) :: vcov    !!  Variance-Covariance Matrix Of length lvcov
        integer(kind=c_int) :: lvcov    !! length of VCOV
        real(kind=c_double) :: res 
        real(kind=c_double) :: mean 
        real(kind=c_double) :: var 
        real(kind=c_double) :: loglik 
        real(kind=c_double) :: aic 
        integer(kind=c_int) :: retval 
        real(kind=c_double), dimension(0) :: params 
    end type

    interface
        function sarima_init(p, d, q, s, p_, d_, q_, N)
            use, intrinsic :: iso_c_binding, only: c_int
            import sarima_set
            integer(kind=c_int), value :: p, d, q, s, p_, d_, q_, N
            type(sarima_set) :: sarima_init
        end function
    end interface

    type, bind(c) :: ar_set
        integer(kind=c_int) :: N            !! length of time series
        integer(kind=c_int) :: method
        integer(kind=c_int) :: optmethod    !! Valid only for MLE estimation
        integer(kind=c_int) :: p            !! size of phi
        integer(kind=c_int) :: order        !! order = p
        integer(kind=c_int) :: ordermax     !! Set Maximum order to be fit
        real(kind=c_double) :: phi
        real(kind=c_double) :: res
        real(kind=c_double) :: mean
        real(kind=c_double) :: var
        real(kind=c_double) :: aic
        integer(kind=c_int) :: retval
        real(kind=c_double), dimension(0) :: params
    end type

    interface
        function ar_init(method, N) bind(c, name = 'ar_init')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            integer(kind=c_int), value :: method, N
            type(c_ptr) :: ar_init
        end function
    end interface

    interface
        !!! exec routines 🔻
        subroutine sarimax_exec(obj, inp, xreg) bind(c, name='sarimax_exec')
            use, intrinsic :: iso_c_binding, only: c_double
            import sarimax_set
            type(sarimax_set) :: obj
            real(kind=c_double) :: inp, xreg
        end subroutine sarimax_exec

        subroutine arima_exec(obj, x) bind(c, name='arima_exec')
            use, intrinsic :: iso_c_binding, only: c_ptr, c_double
            type(c_ptr), value :: obj
            type(c_ptr), value :: x
        end subroutine arima_exec

        subroutine sarima_exec(obj, x) bind(c, name='sarima_exec')
            use, intrinsic :: iso_c_binding, only: c_double, c_ptr
            import sarima_set
            type(c_ptr), value :: obj
            real(kind=c_double) :: x
        end subroutine sarima_exec

        subroutine auto_arima_exec(obj, inp, xreg) bind(c, name='auto_arima_exec')
            use, intrinsic :: iso_c_binding, only: c_double, c_ptr
            type(c_ptr), value :: obj
            type(c_ptr), value :: inp, xreg
        end subroutine auto_arima_exec

        subroutine ar_exec(obj, inp) bind(c, name='ar_exec')
            use, intrinsic :: iso_c_binding, only: c_double, c_ptr
            import ar_set
            type(c_ptr), value :: obj
            type(c_ptr), value :: inp
        end subroutine ar_exec
        !!! predict routines 🔻
        subroutine arima_predict(obj, inp, L, xpred, amse) bind(c, name='arima_predict')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_double
            import arima_set
            type(c_ptr), value :: obj
            type(c_ptr), value :: inp, xpred, amse
            integer(kind=c_int), value :: L
        end subroutine arima_predict

        subroutine sarima_predict(obj, inp, L, xpred, amse) bind(c, name='sarima_predict')
            use, intrinsic :: iso_c_binding, only: c_int, c_double
            import sarima_set
            type(sarima_set) :: obj
            real(kind=c_double) :: inp, xpred, amse
            integer(kind=c_int), value :: L
        end subroutine sarima_predict

        subroutine sarimax_predict(obj, inp, xreg, L, newxreg, xpred, amse) bind(c, name='sarimax_predict')
            use, intrinsic :: iso_c_binding, only: c_int, c_double
            import sarimax_set
            type(sarimax_set) :: obj
            real(kind=c_double) :: inp, xreg, newxreg, xpred, amse
            integer(kind=c_int), value :: L
        end subroutine sarimax_predict

        subroutine auto_arima_predict(obj, inp, xreg, L, newxreg, xpred, amse) bind(c, name='auto_arima_predict')
            use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
            type(c_ptr), value :: obj
            type(c_ptr), value :: inp, xreg, newxreg, xpred, amse
            integer(kind=c_int), value :: L
        end subroutine auto_arima_predict

        subroutine ar_predict(obj, inp, L, xpred, amse) bind(c, name='ar_predict')
            use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
            type(c_ptr), value :: obj
            type(c_ptr), value :: inp, xpred, amse
            integer(kind=c_int), value :: L
        end subroutine ar_predict

        subroutine ar(inp, N, p, method, phi, var) bind(c, name='ar')
            use, intrinsic :: iso_c_binding, only: c_int, c_double
            import ar_set
            real(kind=c_double) :: inp, phi, var
            integer(kind=c_int), value :: N, p, method
        end subroutine ar
        !!! setMethod routines 🔻
        subroutine arima_setMethod(obj, value) bind(c, name='arima_setMethod')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(kind=c_int), value :: value
        end subroutine arima_setMethod

        subroutine sarima_setMethod(obj, value) bind(c, name='sarima_setMethod')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(kind=c_int), value :: value
        end subroutine sarima_setMethod

        subroutine auto_arima_setMethod(obj, value) bind(c, name='auto_arima_setMethod')
            use, intrinsic :: iso_c_binding, only: c_int
            import auto_arima_set
            type(auto_arima_set) :: obj
            integer(kind=c_int), value :: value
        end subroutine auto_arima_setMethod

        subroutine sarimax_setMethod(obj, value) bind(c, name='sarimax_setMethod')
            use, intrinsic :: iso_c_binding, only: c_int
            import sarimax_set
            type(sarimax_set) :: obj
            integer(kind=c_int), value :: value
        end subroutine sarimax_setMethod
        !!! setOptMethod routines 🔻
        subroutine arima_setOptMethod(obj, value) bind(c, name='arima_setOptMethod')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(kind=c_int), value :: value
        end subroutine arima_setOptMethod

        subroutine sarima_setOptMethod(obj, value) bind(c, name='sarima_setOptMethod')
            use, intrinsic ::  iso_c_binding, only: c_int
            import sarima_set
            type(sarima_set) :: obj
            integer(kind=c_int), value :: value
        end subroutine sarima_setOptMethod

        subroutine auto_arima_setOptMethod(obj, value) bind(c, name='auto_arima_setOptMethod')
            use, intrinsic :: iso_c_binding, only: c_int
            import auto_arima_set
            type(auto_arima_set) :: obj
            integer(kind=c_int), value :: value
        end subroutine auto_arima_setOptMethod

        subroutine arima_vcov(obj, vcov) bind(c, name='arima_vcov')
            use, intrinsic :: iso_c_binding, only: c_double
            import arima_set
            type(arima_set) :: obj
            real(kind=c_double) :: vcov
        end subroutine arima_vcov

        subroutine sarima_vcov(obj, vcov) bind(c, name='sarima_vcov')
            use, intrinsic :: iso_c_binding, only: c_double
            import sarima_set
            type(sarima_set) :: obj
            real(kind=c_double) :: vcov
        end subroutine sarima_vcov

        subroutine sarimax_vcov(obj, vcov) bind(c, name='sarimax_vcov')
            use, intrinsic :: iso_c_binding, only: c_double
            import sarimax_set
            type(sarimax_set) :: obj
            real(kind=c_double) :: vcov
        end subroutine sarimax_vcov

        subroutine auto_arima_setApproximation(obj, approximation) bind(c, name='auto_arima_setApproximation')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(kind=c_int), value :: approximation
        end subroutine auto_arima_setApproximation

        subroutine auto_arima_setStepwise(obj, stepwise) bind(c, name='auto_arima_setStepwise')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(kind=c_int), value :: stepwise
        end subroutine auto_arima_setStepwise

        subroutine auto_arima_setStationary(obj, stationary) bind(c, name='auto_arima_setStationary')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr) :: obj
            integer(kind=c_int), value :: stationary
        end subroutine auto_arima_setStationary

        subroutine auto_arima_setSeasonal(obj, seasonal) bind(c, name='auto_arima_setSeasonal')
            use, intrinsic :: iso_c_binding, only: c_int
            import auto_arima_set
            type(auto_arima_set) :: obj
            integer(kind=c_int), value :: seasonal
        end subroutine auto_arima_setSeasonal

        subroutine auto_arima_setStationarityParameter(obj, test, alpha, type) bind(c, name='auto_arima_setStationarityParameter')
            use, intrinsic :: iso_c_binding, only: c_double, c_char
            import auto_arima_set
            type(auto_arima_set) :: obj
            character(kind=c_char) :: test, type
                !!\tocheck:
            real(kind=c_double), value :: alpha
        end subroutine auto_arima_setStationarityParameter

        subroutine auto_arima_setSeasonalParameter(obj, test, alpha) bind(c, name='auto_arima_setSeasonalParameter')
            use, intrinsic :: iso_c_binding, only: c_double, c_char
            import auto_arima_set
            type(auto_arima_set) :: obj
            character(kind=c_char) :: test
            real(kind=c_double), value :: alpha
        end subroutine auto_arima_setSeasonalParameter

        subroutine auto_arima_setVerbose(obj, verbose) bind(c, name='auto_arima_setVerbose')
            use, intrinsic :: iso_c_binding, only: c_int
            import auto_arima_set
            type(auto_arima_set) :: obj
            integer(kind=c_int), value :: verbose
        end subroutine auto_arima_setVerbose
        !!! summary routines 🔻
        subroutine arima_summary(obj) bind(c, name='arima_summary')
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
        end subroutine arima_summary

        subroutine sarima_summary(obj) bind(c, name='sarima_summary')
            import sarima_set
            type(sarima_set), value :: obj
        end subroutine sarima_summary

        subroutine sarimax_summary(obj) bind(c, name='sarimax_summary')
            import sarimax_set
            type(sarimax_set) :: obj
        end subroutine sarimax_summary

        subroutine auto_arima_summary(obj) bind(c, name='auto_arima_summary')
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
        end subroutine auto_arima_summary

        subroutine ar_estimate(x, N, method) bind(c, name='ar_estimate')
            use, intrinsic :: iso_c_binding, only: c_int, c_double
            real(kind=c_double) :: x
            integer(kind=c_int), value :: N, method
        end subroutine ar_estimate

        subroutine ar_summary(obj) bind(c, name='ar_summary')
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
        end subroutine ar_summary

        subroutine model_estimate(x, N, d, pmax, h) bind(c, name='model_estimate')
            use, intrinsic :: iso_c_binding, only: c_int, c_double
            real(kind=c_double) :: x
            integer(kind=c_int), value :: N, d, pmax, h
        end subroutine model_estimate

        subroutine pacf(vec, N, par, M) bind(c, name='pacf')
            use, intrinsic :: iso_c_binding, only: c_int, c_double
            real(kind=c_double) :: vec, par
            integer(kind=c_int), value :: N, M
        end subroutine pacf

        subroutine pacf_opt(vec, N, method, par, M) bind(c, name='pacf_opt')
            use, intrinsic :: iso_c_binding, only: c_int, c_double
            real(kind=c_double) :: vec, par
            integer(kind=c_int), value :: N, method, M
        end subroutine pacf_opt

        subroutine acvf(vec, N, par, M) bind(c, name='acvf')
            use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
            ! real(kind=c_double) :: vec, par
            type(c_ptr), value :: vec, par
            integer(kind=c_int), value :: N, M
        end subroutine acvf

        subroutine acvf_opt(vec, N, method, par, M) bind(c, name='acvf_opt')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: vec, par
            integer(kind=c_int), value :: N, method, M
        end subroutine acvf_opt

        subroutine acvf2acf(acf, M) bind(c, name='acvf2acf')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: acf
            integer(kind=c_int), value :: M
        end subroutine acvf2acf

        !!! free routines 🔻
        subroutine arima_free(obj) bind(c, name='arima_free')
            !! free arima struct memory
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
                !! pointer points to `arima_object`
        end subroutine arima_free

        subroutine sarima_free(obj) bind(c, name='sarima_free')
            !! free sarima struct memory
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
                !! pointer points to `sarima_object`
        end subroutine sarima_free

        subroutine sarimax_free(obj) bind(c, name='sarimax_free')
            !! free sarimax struct memory
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
                !! pointer points to `sarimax_object`
        end subroutine sarimax_free

        subroutine auto_arima_free(obj) bind(c, name='auto_arima_free')
            !! free auto_arima struct memory
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
                !! pointer points to `auto_arima_object`
        end subroutine auto_arima_free

        subroutine ar_free(obj) bind(c, name='ar_free')
            !! free ar struct memory
            use, intrinsic :: iso_c_binding, only: c_ptr
            type(c_ptr), value :: obj
                !! pointer points to `ar_object`
        end subroutine ar_free
        !!! Yule-Walker, Burg and Hannan Rissanen Algorithms for Initial Parameter Estimation
        subroutine yw(x, N, p, phi, var) bind(c, name='yw')
            !! Yule-Walker Algorithms for Initial Parameter Estimation
            use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
            type(c_ptr), value :: var
                !!\FIXME:
            type(c_ptr), value :: x, phi
            integer(kind=c_int), value :: N, p
        end subroutine yw

        subroutine burg(x, N, p, phi, var) bind(c, name='burg')
            !! Burg Algorithms for Initial Parameter Estimation
            use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
            type(c_ptr), value :: var
            type(c_ptr), value :: x, phi
            integer(kind=c_int), value :: N, p
        end subroutine burg

        subroutine hr(x, N, p, q, phi, theta, var) bind(c, name='hr')
            !! Hannan Rissanen Algorithms for Initial Parameter Estimation
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: x, phi, theta, var
            integer(kind=c_int), value :: N, p, q
        end subroutine hr
    end interface
    !* CTSA_H_ *!
contains

end module fortran_tsa

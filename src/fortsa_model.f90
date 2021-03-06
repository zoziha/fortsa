!> author: 左志华
!> date: 2022-07-08
!>
!> Time Series Analysis Model
!> 时间序列分析模型
module fortsa_model_m

    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char, c_ptr, c_null_ptr, c_loc
    implicit none
    private

    public :: arima_set, arima_init, arima_setMethod, arima_setOptMethod, &
              arima_exec, arima_summary, arima_predict, arima_free

    public :: ar_init, ar_exec, &
              ar_summary, ar_predict, &
              ar_free

    public :: auto_arima_init, auto_arima_setApproximation, auto_arima_exec, &
              auto_arima_summary, auto_arima_predict, &
              auto_arima_free, auto_arima_setStepwise, auto_arima_setVerbose

    public :: sarima_init, sarima_predict, sarima_setMethod, sarima_setOptMethod, &
              sarima_vcov, sarima_exec, sarima_free, sarima_summary

    public :: sarimax_init, sarimax_setMethod, sarimax_exec, &
              sarimax_summary, sarimax_predict, sarimax_free

    public :: yw, burg, hr

    public :: auto_arima_set

    type, bind(c) :: auto_arima_set
        integer(c_int) :: N        !! length of time series
        integer(c_int) :: Nused    !! length of time series after differencing, Nused = N - d
        integer(c_int) :: method
        integer(c_int) :: optmethod
        integer(c_int) :: pmax     !! Maximum size of phi
        integer(c_int) :: dmax     !! Maximum Number of times the series is to be differenced
        integer(c_int) :: qmax     !! Maximum size of theta
        integer(c_int) :: pmax_    !! Maximum Size of seasonal phi
        integer(c_int) :: dmax_    !! Maximum number of times the seasonal series is to be differenced
        integer(c_int) :: qmax_    !! Maximum size of Seasonal Theta
        integer(c_int) :: p        !! size of phi
        integer(c_int) :: d        !! Number of times the series is to be differenced
        integer(c_int) :: q        !! size of theta
        integer(c_int) :: s        !! Seasonality/Period
        integer(c_int) :: p_       !! Size of seasonal phi
        integer(c_int) :: d_       !! The number of times the seasonal series is to be differenced
        integer(c_int) :: q_       !! size of Seasonal Theta
        integer(c_int) :: r        !! Number of exogenous variables
        integer(c_int) :: M        !! M = 0 if mean is 0.0 else M = 1
        integer(c_int) :: ncoeff   !! Total Number of Coefficients to be estimated
        real(c_double) :: phi
        real(c_double) :: theta
        real(c_double) :: phi_
        real(c_double) :: theta_
        real(c_double) :: exog
        real(c_double) :: vcov     !! Variance-Covariance Matrix Of length lvcov
        integer(c_int) :: lvcov    !! length of VCOV
        real(c_double) :: res
        real(c_double) :: mean
        real(c_double) :: var
        real(c_double) :: loglik
        real(c_double) :: ic
        integer(c_int) :: retval
        integer(c_int) :: start
        integer(c_int) :: imean
        integer(c_int) :: idrift
        integer(c_int) :: stationary
        integer(c_int) :: seasonal
        integer(c_int) :: Order_max
        integer(c_int) :: p_start
        integer(c_int) :: q_start
        integer(c_int) :: P_start_
        integer(c_int) :: Q_start_
        character(c_char), dimension(10) :: information_criteria
        integer(c_int) :: stepwise
        integer(c_int) :: num_models
        integer(c_int) :: approximation
        integer(c_int) :: verbose
        character(c_char), dimension(10) :: test
        character(c_char), dimension(10) :: type
        character(c_char), dimension(10) :: seas
        real(c_double) :: alpha_test
        real(c_double) :: alpha_seas
        real(c_double) :: lambda
        real(c_double) :: sigma2
        real(c_double) :: aic
        real(c_double) :: bic
        real(c_double) :: aicc
        real(c_double), dimension(0) :: params
    end type auto_arima_set

    interface
        function auto_arima_init(pdqmax, pdqmax_, s, r, N) bind(c, name='auto_arima_init')
            import :: c_int, c_ptr
            integer(c_int) :: pdqmax(*), pdqmax_(*)
            integer(c_int), value :: s, r, N
            type(c_ptr) :: auto_arima_init
        end function auto_arima_init
    end interface

    type, bind(c) :: sarimax_set
        integer(c_int) :: N        !! length of time series
        integer(c_int) :: Nused    !!length of time series after differencing, Nused = N - d
        integer(c_int) :: method
        integer(c_int) :: optmethod
        integer(c_int) :: p        !! size of phi
        integer(c_int) :: d        !! Number of times the series is to be differenced
        integer(c_int) :: q        !!size of theta
        integer(c_int) :: s        !! Seasonality/Period
        integer(c_int) :: p_       !!Size of seasonal phi
        integer(c_int) :: d_       !! The number of times the seasonal series is to be differenced
        integer(c_int) :: q_       !!size of Seasonal Theta
        integer(c_int) :: r        !! Number of exogenous variables
        integer(c_int) :: M        !! M = 0 if mean is 0.0 else M = 1
        integer(c_int) :: ncoeff   !! Total Number of Coefficients to be estimated
        real(c_double) :: phi
        real(c_double) :: theta
        real(c_double) :: phi_
        real(c_double) :: theta_
        real(c_double) :: exog
        real(c_double) :: vcov     !! Variance-Covariance Matrix Of length lvcov
        integer(c_int) :: lvcov    !!length of VCOV
        real(c_double) :: res
        real(c_double) :: mean
        real(c_double) :: var
        real(c_double) :: loglik
        real(c_double) :: aic
        integer(c_int) :: retval
        integer(c_int) :: start
        integer(c_int) :: imean
        real(c_double), dimension(0) :: params
    end type sarimax_set

    interface
        function sarimax_init(p, d, q, p_, d_, q_, s, r, imean, N) bind(c, name='sarimax_init')
            import :: c_int, c_ptr
            integer(c_int), value :: p, d, q, p_, d_, q_, s, r, imean, N
            type(c_ptr) :: sarimax_init     !! `sarimax_set` struct
        end function sarimax_init
    end interface

    type, bind(c) :: arima_set
        integer(c_int) :: N         !! length of time series
        integer(c_int) :: Nused     !! length of time series after differencing, Nused = N - d
        integer(c_int) :: method
        integer(c_int) :: optmethod
        integer(c_int) :: p         !! size of phi
        integer(c_int) :: d         !! Number of times the series is to be differenced
        integer(c_int) :: q         !! size of theta
        integer(c_int) :: s         !! Seasonality/Period
        integer(c_int) :: p_        !! Size of seasonal phi
        integer(c_int) :: d_        !! The number of times the seasonal series is to be differenced
        integer(c_int) :: q_        !! size of Seasonal Theta
        integer(c_int) :: r         !! Number of exogenous variables
        integer(c_int) :: M         !! M = 0 if mean is 0.0 else M = 1
        integer(c_int) :: ncoeff    !! Total Number of Coefficients to be estimated
        type(c_ptr) :: phi
        type(c_ptr) :: theta
        type(c_ptr) :: phi_
        type(c_ptr) :: theta_
        type(c_ptr) :: exog
        type(c_ptr) :: vcov         !! Variance-Covariance Matrix Of length lvcov
        integer(c_int) :: lvcov     !! length of VCOV
        type(c_ptr) :: res
        real(c_double) :: mean
        real(c_double) :: var
        real(c_double) :: loglik
        real(c_double) :: aic
        integer(c_int) :: retval
        integer(c_int) :: start
        integer(c_int) :: imean
        real(c_double), dimension(0) :: params
    end type arima_set

    ! type(c_ptr), bind(c, name='arima_object') :: arima_object

    interface
        function arima_init(p, d, q, N) bind(c, name='arima_init')
            import :: c_int, c_ptr
            integer(c_int), value :: p, d, q, N
            type(c_ptr) :: arima_init
        end function arima_init
    end interface

    type, bind(c) :: sarima_set
        integer(c_int) :: N         !!  length of time series
        integer(c_int) :: Nused     !! length of time series after differencing, Nused = N - d - s*D
        integer(c_int) :: method
        integer(c_int) :: optmethod
        integer(c_int) :: p         !!  size of phi
        integer(c_int) :: d         !!  Number of times the series is to be differenced
        integer(c_int) :: q         !! size of theta
        integer(c_int) :: s         !!  Seasonality/Period
        integer(c_int) :: P_        !! Size of seasonal phi
        integer(c_int) :: D_        !!  The number of times the seasonal series is to be differenced
        integer(c_int) :: Q_        !! size of Seasonal Theta
        integer(c_int) :: M         !!  M = 0 if mean is 0.0 else M = 1
        integer(c_int) :: ncoeff    !!  Total Number of Coefficients to be estimated
        real(c_double) :: phi
        real(c_double) :: theta
        real(c_double) :: PHI_
        real(c_double) :: THETA_
        real(c_double) :: vcov      !!  Variance-Covariance Matrix Of length lvcov
        integer(c_int) :: lvcov     !! length of VCOV
        real(c_double) :: res
        real(c_double) :: mean
        real(c_double) :: var
        real(c_double) :: loglik
        real(c_double) :: aic
        integer(c_int) :: retval
        real(c_double), dimension(0) :: params
    end type sarima_set

    interface
        function sarima_init(p, d, q, s, p_, d_, q_, N) bind(c, name='sarima_init')
            import :: c_int, c_ptr
            integer(c_int), value :: p, d, q, s, p_, d_, q_, N
            type(c_ptr) :: sarima_init
        end function sarima_init
    end interface

    type, bind(c) :: ar_set
        integer(c_int) :: N            !! length of time series
        integer(c_int) :: method
        integer(c_int) :: optmethod    !! Valid only for MLE estimation
        integer(c_int) :: p            !! size of phi
        integer(c_int) :: order        !! order = p
        integer(c_int) :: ordermax     !! Set Maximum order to be fit
        real(c_double) :: phi
        real(c_double) :: res
        real(c_double) :: mean
        real(c_double) :: var
        real(c_double) :: aic
        integer(c_int) :: retval
        real(c_double), dimension(0) :: params
    end type ar_set

    interface
        function ar_init(method, N) bind(c, name='ar_init')
            import :: c_int, c_ptr
            integer(c_int), value :: method, N
            type(c_ptr) :: ar_init
        end function ar_init
    end interface

    interface
        ! ------------------------------ exec routines ------------------------------ !
        subroutine ctsa_sarimax_exec(obj, inp, xreg) bind(c, name='sarimax_exec')
            import :: c_ptr, c_double
            type(c_ptr), value :: obj
            real(c_double) :: inp(*)
            type(c_ptr), value :: xreg
        end subroutine ctsa_sarimax_exec

        subroutine arima_exec(obj, x) bind(c, name='arima_exec')
            import :: c_ptr, c_double
            type(c_ptr), value :: obj
            real(c_double) :: x(*)
        end subroutine arima_exec

        subroutine sarima_exec(obj, x) bind(c, name='sarima_exec')
            import :: c_double, c_ptr
            type(c_ptr), value :: obj
            real(c_double) :: x(*)
        end subroutine sarima_exec

        subroutine ctsa_auto_arima_exec(obj, inp, xreg) bind(c, name='auto_arima_exec')
            import :: c_double, c_ptr
            type(c_ptr), value :: obj
            real(c_double) :: inp(*)
            type(c_ptr), value :: xreg
        end subroutine ctsa_auto_arima_exec

        subroutine ar_exec(obj, inp) bind(c, name='ar_exec')
            import :: c_double, c_ptr
            type(c_ptr), value :: obj
            real(c_double) :: inp(*)
        end subroutine ar_exec

        ! ------------------------------ predict routines ------------------------------ !
        subroutine arima_predict(obj, inp, L, xpred, amse) bind(c, name='arima_predict')
            import :: c_int, c_ptr, c_double
            type(c_ptr), value :: obj
            real(c_double) :: inp(*), xpred(*), amse(*)
            integer(c_int), value :: L
        end subroutine arima_predict

        subroutine sarima_predict(obj, inp, L, xpred, amse) bind(c, name='sarima_predict')
            import :: c_int, c_double, c_ptr
            type(c_ptr), value :: obj
            real(c_double) :: inp(*), xpred(*), amse(*)
            integer(c_int), value :: L
        end subroutine sarima_predict

        subroutine sarimax_predict(obj, inp, xreg, L, newxreg, xpred, amse) &
            bind(c, name='sarimax_predict')
            import :: c_ptr, c_int, c_double
            type(c_ptr), value :: obj
            real(c_double) :: inp(*), xreg(*), newxreg(*), xpred(*), amse(*)
            integer(c_int), value :: L
        end subroutine sarimax_predict

        !> Predictes the next L values of the time series
        subroutine auto_arima_predict(obj, inp, xreg, L, newxreg, xpred, amse) bind(c)
            import :: c_int, c_ptr, c_double

            !> Pointer points to `auto_arima_object`
            type(c_ptr), value :: obj

            !> Input array
            real(c_double) :: inp(*)

            !> TODO:
            real(c_double) :: xreg(*), newxreg(*)

            !> Prediction array
            real(c_double) :: xpred(*)

            !> Standard errors array
            real(c_double) :: amse(*)

            !> Length of prediction
            integer(c_int), value :: L

        end subroutine auto_arima_predict

        subroutine ar_predict(obj, inp, L, xpred, amse) bind(c, name='ar_predict')
            import :: c_int, c_double, c_ptr
            type(c_ptr), value :: obj
            real(c_double) :: inp(*), xpred(*), amse(*)
            integer(c_int), value :: L
        end subroutine ar_predict

        subroutine ar(inp, N, p, method, phi, var) bind(c, name='ar')
            import :: c_int, c_double
            real(c_double) :: inp, phi, var
            integer(c_int), value :: N, p, method
        end subroutine ar

        ! ------------------------------ setMethod routines ------------------------------ !
        subroutine arima_setMethod(obj, value) bind(c, name='arima_setMethod')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: value
        end subroutine arima_setMethod

        subroutine sarima_setMethod(obj, value) bind(c, name='sarima_setMethod')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: value
        end subroutine sarima_setMethod

        subroutine auto_arima_setMethod(obj, value) bind(c, name='auto_arima_setMethod')
            import :: c_ptr, c_int
            type(c_ptr), value :: obj
            integer(c_int), value :: value
        end subroutine auto_arima_setMethod

        subroutine sarimax_setMethod(obj, value) bind(c, name='sarimax_setMethod')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: value
        end subroutine sarimax_setMethod

        ! ------------------------------ setOptMethod routines ------------------------------ !
        subroutine arima_setOptMethod(obj, value) bind(c, name='arima_setOptMethod')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: value
        end subroutine arima_setOptMethod

        subroutine sarima_setOptMethod(obj, value) bind(c, name='sarima_setOptMethod')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: value
        end subroutine sarima_setOptMethod

        subroutine auto_arima_setOptMethod(obj, value) bind(c, name='auto_arima_setOptMethod')
            import :: c_ptr, c_int
            type(c_ptr), value :: obj
            integer(c_int), value :: value
        end subroutine auto_arima_setOptMethod

        subroutine arima_vcov(obj, vcov) bind(c, name='arima_vcov')
            import :: c_ptr, c_double
            type(c_ptr), value :: obj
            real(c_double) :: vcov
        end subroutine arima_vcov

        subroutine sarima_vcov(obj, vcov) bind(c, name='sarima_vcov')
            import :: c_double, c_ptr
            type(c_ptr) :: obj
            real(c_double) :: vcov
        end subroutine sarima_vcov

        subroutine sarimax_vcov(obj, vcov) bind(c, name='sarimax_vcov')
            import :: c_ptr, c_double
            type(c_ptr), value :: obj
            real(c_double) :: vcov
        end subroutine sarimax_vcov

        subroutine auto_arima_setApproximation(obj, approximation) &
            bind(c, name='auto_arima_setApproximation')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: approximation
        end subroutine auto_arima_setApproximation

        subroutine auto_arima_setStepwise(obj, stepwise) bind(c, name='auto_arima_setStepwise')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: stepwise
        end subroutine auto_arima_setStepwise

        subroutine auto_arima_setStationary(obj, stationary) bind(c, name='auto_arima_setStationary')
            import :: c_int, c_ptr
            type(c_ptr) :: obj
            integer(c_int), value :: stationary
        end subroutine auto_arima_setStationary

        subroutine auto_arima_setSeasonal(obj, seasonal) bind(c, name='auto_arima_setSeasonal')
            import :: c_ptr, c_int
            type(c_ptr), value :: obj
            integer(c_int), value :: seasonal
        end subroutine auto_arima_setSeasonal

        subroutine auto_arima_setStationarityParameter(obj, test, alpha, type) &
            bind(c, name='auto_arima_setStationarityParameter')
            import :: c_ptr, c_double, c_char
            type(c_ptr), value :: obj
            character(c_char) :: test, type
                !!\tocheck:
            real(c_double), value :: alpha
        end subroutine auto_arima_setStationarityParameter

        subroutine auto_arima_setSeasonalParameter(obj, test, alpha) &
            bind(c, name='auto_arima_setSeasonalParameter')
            import :: c_ptr, c_double, c_char
            type(c_ptr), value :: obj
            character(c_char) :: test
            real(c_double), value :: alpha
        end subroutine auto_arima_setSeasonalParameter

        subroutine auto_arima_setVerbose(obj, verbose) bind(c, name='auto_arima_setVerbose')
            import :: c_int, c_ptr
            type(c_ptr), value :: obj
            integer(c_int), value :: verbose
        end subroutine auto_arima_setVerbose

        ! ------------------------------ summary routines ------------------------------ !
        subroutine arima_summary(obj) bind(c, name='arima_summary')
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine arima_summary

        subroutine sarima_summary(obj) bind(c, name='sarima_summary')
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine sarima_summary

        subroutine sarimax_summary(obj) bind(c, name='sarimax_summary')
            import :: c_ptr
            type(c_ptr), value :: obj
                !! `sarimax_set` struct
        end subroutine sarimax_summary

        subroutine auto_arima_summary(obj) bind(c, name='auto_arima_summary')
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine auto_arima_summary

        subroutine ar_estimate(x, N, method) bind(c, name='ar_estimate')
            import :: c_int, c_double
            real(c_double) :: x
            integer(c_int), value :: N, method
        end subroutine ar_estimate

        subroutine ar_summary(obj) bind(c, name='ar_summary')
            import :: c_ptr
            type(c_ptr), value :: obj
        end subroutine ar_summary

        subroutine model_estimate(x, N, d, pmax, h) bind(c, name='model_estimate')
            import :: c_int, c_double
            real(c_double) :: x
            integer(c_int), value :: N, d, pmax, h
        end subroutine model_estimate

        !! free arima struct memory
        subroutine arima_free(object) bind(c, name='arima_free')
            import :: c_ptr
            type(c_ptr), value :: object
                !! pointer points to `arima_objectect`
        end subroutine arima_free

        !! free sarima struct memory
        subroutine sarima_free(object) bind(c, name='sarima_free')
            import :: c_ptr
            type(c_ptr), value :: object
                !! pointer points to `sarima_object`
        end subroutine sarima_free

        !! free sarimax struct memory
        subroutine sarimax_free(object) bind(c, name='sarimax_free')
            import :: c_ptr
            type(c_ptr), value :: object
                !! pointer points to `sarimax_object`
        end subroutine sarimax_free

        !> Free auto_arima struct memory
        subroutine auto_arima_free(object) bind(c, name='auto_arima_free')
            import :: c_ptr

            !> Pointer points to `auto_arima_object`
            type(c_ptr), value :: object

        end subroutine auto_arima_free

        !! free ar struct memory
        subroutine ar_free(object) bind(c, name='ar_free')
            import :: c_ptr
            type(c_ptr), value :: object
                !! pointer points to `ar_object`
        end subroutine ar_free

        ! Yule-Walker, Burg and Hannan Rissanen Algorithms for Initial Parameter Estimation
        !! Yule-Walker Algorithms for Initial Parameter Estimation
        subroutine yw(x, N, p, phi, var) bind(c, name='yw')
            import :: c_int, c_double
            real(c_double) :: var
            real(c_double) :: x(*), phi(*)
            integer(c_int), value :: N, p
        end subroutine yw

        !! Burg Algorithms for Initial Parameter Estimation
        subroutine burg(x, N, p, phi, var) bind(c, name='burg')
            import :: c_int, c_double
            real(c_double) :: var
            real(c_double) :: x(*), phi(*)
            integer(c_int), value :: N, p
        end subroutine burg

        !! Hannan Rissanen Algorithms for Initial Parameter Estimation
        subroutine hr(x, N, p, q, phi, theta, var) bind(c, name='hr')
            import :: c_int, c_double
            real(c_double) :: x(*), phi(*), theta(*), var
            integer(c_int), value :: N, p, q
        end subroutine hr
    end interface

contains

    subroutine sarimax_exec(obj, inp, xreg)
        type(c_ptr), intent(in) :: obj
        real(c_double), intent(in) :: inp(*)
        real(c_double), intent(out), optional, target :: xreg(*)
        if (present(xreg)) then
            call ctsa_sarimax_exec(obj, inp, c_loc(xreg))
        else
            call ctsa_sarimax_exec(obj, inp, c_null_ptr)
        end if
    end subroutine sarimax_exec

    subroutine auto_arima_exec(obj, inp, xreg)
        type(c_ptr), intent(in) :: obj
        real(c_double), intent(in) :: inp(*)
        real(c_double), intent(out), optional, target :: xreg(*)
        if (present(xreg)) then
            call ctsa_auto_arima_exec(obj, inp, c_loc(xreg))
        else
            call ctsa_auto_arima_exec(obj, inp, c_null_ptr)
        end if
    end subroutine auto_arima_exec

end module fortsa_model_m

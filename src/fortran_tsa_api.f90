!! `Fortran-TSA-api.f90` will be the `C-Fortran` interface to `CTSA`

module fortran_tsa
    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_char
    implicit none
    private

    type, bind(c, name='auto_arima_set') :: auto_arima_set
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
            import auto_arima_set
            use, intrinsic :: iso_c_binding, only: c_int
            integer(kind=c_int) :: pdqmax, pdqmax_
            integer(kind=c_int), value :: s, r, N
            type(auto_arima_set) :: auto_arima_init
                !! \TOCHECK:
        end function
    end interface

    type, bind(c, name='arima_set') :: arima_set
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
        real(kind=c_double) :: aic
        integer(kind=c_int) :: retval
        integer(kind=c_int) :: start
        integer(kind=c_int) :: imean
        real(kind=c_double), dimension(0) :: params
    end type

    interface
        function arima_init(p, d, q, N)
            import arima_set
            use, intrinsic :: iso_c_binding, only: c_int
            integer(kind=c_int), value :: p, d, q, N
            type(arima_set) :: arima_init
        end function
    end interface

    type, bind(c, name='sarima_set') :: sarima_set
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
        real(kind=c_double) :: params[0] 
    end type

    interface
        function sarima_init(p, d, q, s, p_, d_, q_, N)
            import sarima_set
            use, intrinsic :: iso_c_binding, only: c_int
            integer(kind=c_int), value :: p, d, q, s, p_, d_, q_, N
            type(sarima_set) :: sarima_init
        end function
    end interface

contains

end module fortran_tsa

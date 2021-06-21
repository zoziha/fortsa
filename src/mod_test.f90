!! Test for C-Fortran interoperability

module fortran_tsa_test
    use iso_c_binding
    implicit none
    public
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
    end type arima_set

    interface
        function arima_init(p, d, q, N) bind(c, name='arima_init')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            import arima_set
            integer(kind=c_int), value :: p, d, q, N
            type(arima_set) :: arima_init
        end function
    end interface

contains
    
end module
program test_model_sarimax

    !! ARMA example for a stationary time series with no seasonal components and no exogenous variable.
    use stdlib_error, only: error_stop
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use forlab_io, only: file, disp
    use fortsa_model, only: sarimax_init, sarimax_setMethod, sarimax_exec, &
                            sarimax_summary, sarimax_predict, sarimax_free
    implicit none
    integer :: i, N, d, D_, L
    real(8), target, allocatable :: inp(:)
    integer :: p, q, p_, q_, s, r, imean
    real(8), target, allocatable :: phi(:), theta(:)
    real(8), target, allocatable :: xpred(:), amse(:)
    type(c_ptr) :: obj
    type(file) :: infile

    !! Make sure all the parameter values are correct and consistent with other values. eg., if xreg is NULL r should be 0
    !! or if P = D = Q = 0 then make sure that s is also 0.
    !! Recheck the values if the program fails to execute.

    p = 0
    d = 1
    q = 0
    s = 0
    p_ = 0
    d_ = 0
    q_ = 0
    r = 0

    imean = 1

    L = 5
    allocate (phi(p), theta(q), xpred(L), amse(L))
    infile = file('example/data/seriesA.txt', 'r')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    N = infile%lines

    allocate (inp(N))
    do i = 1, N
        read (infile%unit, *) inp(i)
    end do

    obj = sarimax_init(p, d, q, p_, d_, q_, s, r, imean, N)

    !!## setMethod
    !! Method 0 ("CSS-MLE") is default. The method also accepts values 1 ("MLE") and 2 ("CSS")
    call sarimax_setMethod(obj, 0)

    !!## sarimax_exec(object, input time series, exogenous time series)
    !! set exogenous to NULL if deadling only with a univariate time series.
    call sarimax_exec(obj, inp)
    call sarimax_summary(obj)

    !!## sarimax_predict(sarimax_object obj, double *inp, double *xreg, int L,double *newxreg, double *xpred, double *amse)
    !! inp - Input Time Series
    !! xreg - Exogenous Time Series
    !! L - L point prediction
    !! newxreg - Exogenous Time Series of dimension r * L where r is the number of exogenus time series and L is the length of each
    !! xpred - L future values
    !! amse - MSE for L future values
    call sarimax_predict(obj, inp, %ref([0.0d0]), L, %ref([0.0d0]), xpred, &
                         amse)
    call disp(xpred, 'Predicted Values : ')
    call disp(sqrt(amse), 'Standard Errors : ')

    call sarimax_free(obj)
    deallocate (inp, phi, theta, xpred, amse)

end program test_model_sarimax

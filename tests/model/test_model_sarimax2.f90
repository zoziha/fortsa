!! SARIMAX example
program test_model_sarimax2

    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use forlab_io, only: disp, file
    use stdlib_error, only: error_stop
    use fortsa_model, only: sarimax_init, sarimax_setMethod, sarimax_exec, &
                            sarimax_summary, sarimax_predict, sarimax_free
    implicit none
    integer :: i, N, d, d_, l
    real(8), target, allocatable :: inp(:)
    integer :: p, q, p_, q_, s, r
    real(8), target, allocatable :: phi(:), theta(:)
    real(8), target, allocatable :: phi_(:), theta_(:)
    real(8), target, allocatable :: xpred(:), amse(:)
    type(c_ptr) :: obj
        !! `sarimax_set` strcuct
    integer :: imean = 1
    type(file) :: infile

    !! Make sure all the parameter values are correct and consistent with other values. eg., if xreg is NULL r should be 0
    !! or if P = D = Q = 0 then make sure that s is also 0.
    !! Recheck the values if the program fails to execute.

    p = 2
    d = 1
    q = 2
    s = 12
    p_ = 1
    d_ = 1
    q_ = 1
    r = 0

    L = 5

    allocate (phi(p), theta(q), phi_(p_), theta_(q_), xpred(L), amse(L))
    infile = file('example/data/seriesG.txt', 'r')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    N = infile%lines

    allocate (inp(N))
    do i = 1, N
        read (infile%unit, *) inp(i)
        inp(i) = log(inp(i))
    end do
    call infile%close()

    obj = sarimax_init(p, d, q, p_, d_, q_, s, r, imean, N)

    !!## setMethod()
    !! Method 0 ("CSS-MLE") is default. The method also accepts values 1 ("MLE") and 2 ("CSS")
    call sarimax_setMethod(obj, 2)

    !!## sarimax_exec(object, input time series, exogenous time series)
    !! set exogenous to NULL if deadling only with a univariate time series.
    call sarimax_exec(obj, c_loc(inp(1)), c_null_ptr)
    call sarimax_summary(obj)

    !!## sarimax_predict(sarimax_object obj, double *inp, double *xreg, int L,double *newxreg, double *xpred, double *amse)
    !! inp - Input Time Series
    !! xreg - Exogenous Time Series
    !! L - L point prediction
    !! newxreg - Exogenous Time Series of dimension r * L where r is the number of exogenus time series and L is the length of each
    !! xpred - L future values
    !! amse - MSE for L future values
    call sarimax_predict(obj, c_loc(inp(1)), c_null_ptr, L, c_null_ptr, c_loc(xpred(1)), &
                         c_loc(amse(1)))
    call disp(exp(xpred), 'Predicted Values : ')
    call disp(sqrt(amse), 'Standard Errors : ')

    call sarimax_free(obj)
    deallocate (inp, phi, theta, phi_, theta_, xpred, amse)

end program test_model_sarimax2

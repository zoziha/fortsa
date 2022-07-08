!! ARIMA example with exogenous variables
program test_model_sarimax3

    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_null_ptr
    use forlab, only: disp, file
    use stdlib_error, only: error_stop
    use fortsa_model_m, only: sarimax_init, sarimax_setMethod, sarimax_exec, &
                            sarimax_summary, sarimax_predict, sarimax_free
    implicit none
    integer :: i, N, d, d_, L, line_num
    real(8), target, allocatable :: inp(:)
    integer :: p, q, p_, q_, s, r
    real(8), allocatable, target :: xpred(:), amse(:), xreg(:), newxreg(:)
    type(c_ptr) :: obj
    integer :: imean = 1

    type(file) :: infile
    real(8), allocatable :: data(:, :)

    !! Make sure all the parameter values are correct and consistent with other values. eg., if xreg is NULL r should be 0
    !!  or if P = D = Q = 0 then make sure that s is also 0.
    !! Recheck the values if the program fails to execute.

    p = 2
    d = 0
    q = 2
    s = 0
    p_ = 0
    d_ = 0
    q_ = 0
    r = 2

    L = 5

    allocate (xpred(L), amse(L))
    infile = file('ctsa/data/e1m.dat')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    line_num = infile%countlines()
    N = line_num - L

    allocate (data(line_num, 3))
    allocate (inp(N), xreg(N*2), newxreg(L*2))
    do i = 1, line_num
        read (infile%unit, *) data(i, :)
            !! read temp data
    end do
    call infile%close()

    inp = data(1:N, 1)
    xreg(:N) = data(1:N, 2)
    xreg(N + 1:) = data(1:N, 3)

    newxreg(:L) = data(N + 1:, 2)
    newxreg(L + 1:) = data(N + 1:, 3)

    obj = sarimax_init(p, d, q, p_, d_, q_, s, r, imean, N)

    !!## setMethod()
    !! Method 0 ("CSS-MLE") is default. The method also accepts values 1 ("MLE") and 2 ("CSS")
    call sarimax_setMethod(obj, 2)

    !!## sarimax_exec(object, input time series, exogenous time series)
    !! set exogenous to NULL if deadling only with a univariate time series.
    call sarimax_exec(obj, inp, xreg)
    call sarimax_summary(obj)

    !!## sarimax_predict(sarimax_object obj, double *inp, double *xreg, int L,double *newxreg, double *xpred, double *amse)
    !! inp - Input Time Series
    !! xreg - Exogenous Time Series
    !! L - L point prediction
    !! newxreg - Exogenous Time Series of dimension r * L where r is the number of exogenus time series and L is the length of each
    !! xpred - L future values
    !! amse - MSE for L future values
    call sarimax_predict(obj, inp, xreg, L, newxreg, &
                         xpred, amse)
    call disp(xpred, 'Predicted Values : ')
    call disp(sqrt(amse), 'Standard Errors : ')

    call sarimax_free(obj)
    deallocate (inp, xpred, amse, xreg, newxreg)

end program test_model_sarimax3

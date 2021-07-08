program test_model_autoarima2

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_loc
    use stdlib_error, only: error_stop
    use forlab_io, only: file, disp
    use fortsa_model, only: auto_arima_init, auto_arima_setApproximation, auto_arima_setStepwise, auto_arima_setVerbose, &
                            auto_arima_exec, auto_arima_summary, auto_arima_predict, auto_arima_free
    implicit none
    integer :: i, d, d_, L
    real(8), allocatable :: inp(:)
    integer :: p, q, p_, q_, s, r
    real(8), allocatable :: xpred(:), amse(:)
    type(c_ptr) :: obj

    integer :: order(3), seasonal(3)

    type(file) :: infile
    !! Make sure all the parameter values are correct and consistent with other values. eg., if xreg is NULL r should be 0
    !! or if P = D = Q = 0 then make sure that s is also 0.
    !! Recheck the values if the program fails to execute.
    p = 5
    d = 2
    q = 5
    s = 12
    p_ = 2
    d_ = 1
    q_ = 2
    r = 0

    order = [p, d, q]
    seasonal = [p_, d_, q_]

    L = 5

    infile = file('example/data/seriesG.txt', 'r')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()

    allocate (inp(infile%lines), xpred(L), amse(L))
    do i = 1, infile%lines
        read (infile%unit, *) inp(i)
    end do

    obj = auto_arima_init(order, seasonal, s, r, infile%lines)
    call auto_arima_setApproximation(obj, 1)
    call auto_arima_setStepwise(obj, 1)
    call auto_arima_setVerbose(obj, 1)

    call auto_arima_exec(obj, inp)
        !!\TODO: optional or something
    call auto_arima_summary(obj)
    call auto_arima_predict(obj, inp, %ref([0.0d0]), L, %ref([0.0d0]), xpred, amse)

    call disp('Forecast : 5 Point Look Ahead')
    call disp(xpred, 'Predicted Values : ')
    call disp(exp(sqrt(amse)), 'Standard Errors  : ')

    call auto_arima_free(obj)

    deallocate (inp, xpred, amse)
    call infile%close()

end program test_model_autoarima2

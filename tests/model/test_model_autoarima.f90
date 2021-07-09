program test_model_autoarima

    use forlab_io, only: disp, file
    use fortsa_model, only: auto_arima_init, auto_arima_setApproximation, auto_arima_exec, &
                            auto_arima_summary, auto_arima_predict, &
                            auto_arima_free, auto_arima_setStepwise, auto_arima_setVerbose
    use, intrinsic :: iso_c_binding, only: c_loc, c_ptr, c_null_ptr
    use stdlib_error, only: error_stop
    integer :: i, s, r, L
    integer :: p, d, q, p_, d_, q_
    real(8), allocatable :: xpred(:), amse(:)
    type(c_ptr) :: obj = c_null_ptr
    type(file) :: infile
    real(8), allocatable :: inp(:)
    integer :: order(3), seasonal(3)

    p = 5
    d = 2
    q = 5
    p_ = 2
    d_ = 1
    q_ = 2

    order = [p, d, q]
    seasonal = [p_, d_, q_]

    s = 0
    r = 0
    L = 5

    infile = file('example/data/seriesA.txt', 'r')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    allocate (inp(infile%lines), xpred(L), amse(L))

    do i = 1, infile%lines
        read (infile%unit, *) inp(i)
    end do

    ! obj = c_loc(set)
    ! call c_f_pointer(obj, set)

    obj = auto_arima_init(order, seasonal, s, r, infile%lines)
    call auto_arima_setApproximation(obj, 0)
    call auto_arima_setStepwise(obj, 0)
    ! call auto_arima_setVerbose(obj, 1)

    call auto_arima_exec(obj, inp)
        !!\TOCHECK: optional
    call auto_arima_summary(obj)
    call auto_arima_predict(obj, inp, %ref([0.0d0]), L, %ref([0.0d0]), xpred, amse)

    call disp('Predicted Values : ')
    call disp(xpred)
    call disp('Standard Errors : ')
    call disp(sqrt(amse))

    call auto_arima_free(obj)
    deallocate (inp, xpred, amse)
    call infile%close()

end program test_model_autoarima

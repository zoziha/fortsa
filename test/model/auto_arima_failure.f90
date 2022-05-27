program auto_arima_failure
    
    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_loc
    use fortsa_model_m, only: auto_arima_init, auto_arima_setApproximation, auto_arima_setStepwise, auto_arima_setVerbose, &
                            auto_arima_exec, auto_arima_summary, auto_arima_predict, auto_arima_free
    implicit none
    integer :: i, N, d, d_, L
    real(8), allocatable, target :: inp(:)
    integer :: p, q, p_, q_, s, r
    real(8), allocatable, target :: xpred(:), amse(:)
    type(c_ptr) :: obj

    integer, target :: order(3), seasonal(3)

    type(file) :: infile
    integer :: lines

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

    infile = file('example/data/seriesG.txt')
    if (.not. infile%exist()) then
    end if
    call infile%open('r')
    lines = infile%countlines()

    allocate (inp(lines), xpred(L), amse(L))
    do i = 1, lines
        read (infile%unit, *) inp(i)
    end do

    obj = auto_arima_init(order, seasonal, s, r, lines)
    call auto_arima_setApproximation(obj, 1)
    call auto_arima_setStepwise(obj, 1)
    call auto_arima_setVerbose(obj, 1)

    call auto_arima_exec(obj, inp)
    call auto_arima_summary(obj)
    call auto_arima_predict(obj, inp, %ref([0.0d0]), L, %ref([0.0d0]), xpred, amse)


    call auto_arima_free(obj)

    deallocate (inp, xpred, amse)
    call infile%close()

end program auto_arima_failure

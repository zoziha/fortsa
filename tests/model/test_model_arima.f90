program test_model_arima

    use forlab_io, only: disp, file
    use fortsa_model, only: arima_set, arima_init, &
                            arima_setMethod, arima_exec, &
                            arima_summary, arima_predict, &
                            arima_free, arima_setOptMethod
    use stdlib_error, only: error_stop
    use, intrinsic :: iso_c_binding
        !!\TODO:
    integer :: i, d, L
    integer :: p, q
    real(8), allocatable :: xpred(:), amse(:)
    type(c_ptr) :: obj = c_null_ptr
    target obj

    type(file) :: infile
    real(8), allocatable :: inp(:)

    p = 0
    d = 1
    q = 0

    L = 5

    infile = file('example/data/seriesA.txt', 'r')
    if (.not. infile%exist()) call error_stop('Error: File not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    allocate (inp(infile%lines), xpred(L), amse(L))

    do i = 1, infile%lines
        read (infile%unit, *) inp(i)
    end do

    obj = arima_init(p, d, q, infile%lines)
    call arima_setMethod(obj, 0)
    call arima_setOptMethod(obj, 5)
    call arima_exec(obj, inp)
    call arima_summary(obj)
    call arima_predict(obj, inp, L, xpred, amse)

    call disp('Predicted Values : ')
    call disp(xpred)
    call disp('Standard Errors : ')
    call disp(sqrt(amse))

    call arima_free(obj)
    deallocate (inp, xpred, amse)
    call infile%close()

end program test_model_arima

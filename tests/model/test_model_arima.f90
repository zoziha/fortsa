program test_model_arima
    use forlab_io, only: disp, file
    use ctsa_api, only: arima_set, arima_init, &
                            arima_setMethod,arima_exec, &
                            arima_summary, arima_predict, &
                            arima_free, arima_setOptMethod
    use stdlib_error, only: error_stop
    use, intrinsic :: iso_c_binding
    integer :: i, d, L
    integer :: p, q
    real(8), target, allocatable :: xpred(:), amse(:)
    type(c_ptr) :: obj = c_null_ptr
    target obj
    ! type(arima_set), target :: set
    ! target set
    ! type(arima_set) :: obj
    type(file) :: infile
    real(8), target, allocatable :: inp(:)

    p = 0
    d = 1
    q = 0

    L = 5

    infile = file('example/data/seriesA.txt', 'r')
    if(.not.infile%exist()) call error_stop('Error: File not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    allocate(inp(infile%lines), xpred(L), amse(L))

    do i = 1, infile%lines
        read(infile%unit, *) inp(i)
    enddo

    ! obj = c_loc(set)
    ! call c_f_pointer(obj, set)

    obj = arima_init(p, d, q, infile%lines)
    call arima_setMethod(obj, 0)
    call arima_setOptMethod(obj, 5)
    call arima_exec(obj, c_loc(inp(1)))
    call arima_summary(obj)
    call arima_predict(obj, c_loc(inp(1)), L, c_loc(xpred(1)), c_loc(amse(1)))
    
    call disp('Predicted Values : ')
    call disp(xpred)
    call disp('Standard Errors : ')
    call disp(sqrt(amse))

    call arima_free(obj)
    deallocate(inp, xpred, amse)
    call infile%close()
    
end program test_model_arima
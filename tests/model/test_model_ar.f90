program test_model_ar
    use forlab_io, only: disp, file
    use ctsa_api, only: ar_init, ar_exec, &
                            ar_summary, ar_predict, &
                            ar_free
    use stdlib_error, only: error_stop
    use, intrinsic :: iso_c_binding
    implicit none
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
    if(.not.infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    allocate(inp(infile%lines), xpred(L), amse(L))

    do i = 1, infile%lines
        read(infile%unit, *) inp(i)
    enddo

    ! obj = c_loc(set)
    ! call c_f_pointer(obj, set)

    obj = ar_init(0, infile%lines)
    call ar_exec(obj, c_loc(inp(1)))
    call ar_summary(obj)
    call ar_predict(obj, c_loc(inp(1)), L, c_loc(xpred(1)), c_loc(amse(1)))
    
    call disp('Predicted Values : ')
    call disp(xpred)
    call disp('Standard Errors : ')
    call disp(sqrt(amse))

    call ar_free(obj)
        !!\FIXME:
    deallocate(inp, xpred, amse)
    call infile%close()
end program test_model_ar
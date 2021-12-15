program test_model_ar
    
    use forlab, only: disp, file
    use fortsa_model, only: ar_init, ar_exec, &
                            ar_summary, ar_predict, &
                            ar_free
    use stdlib_error, only: error_stop
    use, intrinsic :: iso_c_binding, only: c_loc, c_null_ptr, c_ptr
    implicit none
    integer :: i, d, L
    integer :: p, q, line_num
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

    infile = file('example/data/seriesA.txt')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    line_num = infile%countlines()
    allocate (inp(line_num), xpred(L), amse(L))

    do i = 1, line_num
        read (infile%unit, *) inp(i)
    end do

    obj = ar_init(0, line_num)
    call ar_exec(obj, inp)
    call ar_summary(obj)
    call ar_predict(obj, inp, L, xpred, amse)

    call disp(xpred, 'Predicted Values : ')
    call disp(sqrt(amse), 'Standard Errors : ')

    call ar_free(obj)
    deallocate (inp, xpred, amse)
    call infile%close()

end program test_model_ar

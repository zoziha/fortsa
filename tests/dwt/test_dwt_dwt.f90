program test_dwt_dwt

    !! <Fortran 2018 with Parallel Programming> Page.432
    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_char, c_f_pointer, c_null_char
    use forlab_io, only: file, disp
    use stdlib_error, only: error_stop
    use fortsa_dwt, only: wave_init, wt_init, &
                          wave_summary, wt_summary, &
                          modwt, imodwt, &
                          wave_free, wt_free, wt_set
    implicit none
    real(8), allocatable, target :: inp(:), out(:), diff(:), data(:)
    integer :: N, i, J
    type(c_ptr) :: obj
        !! wave_object
    type(c_ptr) :: wt
        !! wt_object
    type(wt_set), pointer :: wt_
        !! wt_object
    real(8), allocatable, target :: output_(:)
    real(8), pointer :: fp(:)

    type(file) :: infile

    obj = wave_init('db4'//c_null_char)
    call wave_summary(obj)

    infile = file('example/data/signal.txt', 'r')
    if (.not. infile%exist()) call error_stop('Error: file not exist : '//infile%filename)
    call infile%open()
    call infile%countlines()
    call disp(infile%lines, 'file number of lines is : ')
    allocate (data(infile%lines))

    do i = 1, infile%lines
        read (infile%unit, *) data(i)
    end do
    call infile%close()
    N = 177

    allocate (inp(N), out(N), diff(N))
    inp(:N) = data(:N)
    J = 2

    wt = wt_init(obj, 'modwt'//c_null_char, N, J)
            !! Initialize the wavelet transform object
    call c_f_pointer(wt, wt_)

    call modwt(wt, inp)
        !! MODWT output can be accessed using wt->output vector. Use wt_summary to find out how to extract appx and detail coefficients

    allocate (output_(wt_%outlength), fp(wt_%outlength))
    call c_f_pointer(wt_%output, fp, reshape([wt_%outlength], shape=[1]))
        !!\FIXME: rm unused variables
    call disp(fp(1:wt_%outlength))
        !!\TODO: c_f_pointer, page427

    call imodwt(wt, out)

    do i = 1, wt_%siglength
        diff(i) = out(i) - inp(i)
    end do
    call disp(maxval(abs(diff)), 'MAX : ')   !! If Reconstruction succeeded then the output should be a small value.
    !!\TODO: detto

    call wt_summary(wt)

    call wave_free(obj)
    call wt_free(wt)
    deallocate (inp, out, diff, data)

end program test_dwt_dwt

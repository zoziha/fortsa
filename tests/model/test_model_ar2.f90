program test_model_ar2

    use forlab_io, only: disp, file
    use forlab_stats, only: mean
    use fortsa_model, only: ar_init, ar_exec, &
                            ar_summary, ar_predict, &
                            ar_free
    use fortsa_model, only: yw, burg, hr
    use stdlib_error, only: error_stop
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_double
    implicit none
    integer :: i, d, L
    integer :: p, q
    real(8), target, allocatable :: phi(:), theta(:)
    type(file) :: infile
    real(8), target, allocatable :: inp(:)
    real(8) :: wmean
    real(c_double), target :: var
        !! mean var

    p = 7
    d = 1
    q = 0

    L = 5

    infile = file('example/data/seriesA.txt', 'r')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    allocate (inp(infile%lines), phi(p))

    do i = 1, infile%lines
        read (infile%unit, *) inp(i)
    end do

    wmean = mean(inp)
        !! forlab mean

    ! obj = c_loc(set)
    ! call c_f_pointer(obj, set)
    call disp('AR Coefficients Using Yule Walker Algorithm : ')
    call yw(c_loc(inp(1)), infile%lines, p, c_loc(phi(1)), c_loc(var))
    call disp(phi, 'PHI : ')
    call disp(var, 'VAR : ')

    call disp('AR Coefficients Using Burg Algorithm : ')
    call burg(c_loc(inp(1)), infile%lines, p, c_loc(phi(1)), c_loc(var))
    call disp(phi, 'PHI : ')
    call disp(var, 'VAR : ')

    p = 1
    q = 1

    deallocate (phi)
    allocate (phi(p), theta(q))
    call disp('ARMA Coefficients Using Hannan Rissanen Algorithm : ')
    call hr(c_loc(inp), infile%lines, p, q, c_loc(phi(1)), c_loc(theta(1)), c_loc(var))
    call disp(phi, 'PHI : ')
    call disp(theta, 'THETA : ')
    call disp(var, 'VAR : ')

    deallocate (inp, phi, theta)
    call infile%close()

end program test_model_ar2

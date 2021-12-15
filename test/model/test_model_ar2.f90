program test_model_ar2

    use forlab, only: disp, file
    use forlab, only: mean
    use fortsa_model, only: ar_init, ar_exec, &
                            ar_summary, ar_predict, &
                            ar_free
    use fortsa_model, only: yw, burg, hr
    use stdlib_error, only: error_stop
    implicit none
    integer :: i, d, L
    integer :: p, q, line_num
    real(8), allocatable :: phi(:), theta(:)
    type(file) :: infile
    real(8), allocatable :: inp(:)
    real(8) :: wmean
    real(8) :: var
        !! mean var

    p = 7
    d = 1
    q = 0

    L = 5

    infile = file('example/data/seriesA.txt')
    if (.not. infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    line_num = infile%countlines()
    allocate (inp(line_num), phi(p))

    do i = 1, line_num
        read (infile%unit, *) inp(i)
    end do

    wmean = mean(inp)
        !! forlab mean

    print *, 'AR Coefficients Using Yule Walker Algorithm : '
    call yw(inp, line_num, p, phi, var)
    call disp(phi, 'PHI : ')
    call disp(var, 'VAR : ')

    print *, 'AR Coefficients Using Burg Algorithm : '
    call burg(inp, line_num, p, phi, var)
    call disp(phi, 'PHI : ')
    call disp(var, 'VAR : ')

    p = 1
    q = 1

    deallocate (phi)
    allocate (phi(p), theta(q))
    print *, 'ARMA Coefficients Using Hannan Rissanen Algorithm : '
    call hr(inp, line_num, p, q, phi, theta, var)
    call disp(phi, 'PHI : ')
    call disp(theta, 'THETA : ')
    call disp(var, 'VAR : ')

    deallocate (inp, phi, theta)
    call infile%close()

end program test_model_ar2

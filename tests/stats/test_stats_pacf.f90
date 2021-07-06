program test_model_pacf
    use, intrinsic :: iso_c_binding, only: c_loc
    use forlab_io, only: disp, file
    use stdlib_error, only: error_stop
    use fortsa_stats, only: pacf, pacf_opt
    implicit none
    integer :: i, N, M
    real(8), target, allocatable :: inp(:), par(:)
    integer :: method

    type(file) :: infile

    infile = file('example/data/seriesC.txt', 'r')
    if(.not.infile%exist()) call error_stop('Error: file not exist, '//infile%filename)
    call infile%open()
    call infile%countlines()
    call disp(infile%lines, 'Linenumber in file is:')
    N = infile%lines

    allocate(inp(N))
    do i = 1, N
        read(infile%unit, *) inp(i)
    end do

    M = 10
    allocate(par(M))

    !! Default Method is Yule-Walker
    call disp('Default Method : pacf')
    call pacf(c_loc(inp(1)), N, c_loc(par(1)), M)
    call disp(par)

    !! pacf_opt : Method 0 Yule Walker 
    method = 0
    call disp('pacf_opt Method 0 Yule Walker')
    call pacf_opt(c_loc(inp(1)), N, method, c_loc(par(1)), M)
    call disp(par)

    !! pacf_opt : Method 1 Burg 
    method = 1
    call disp('pacf_opt Method 1 Burg')
    call pacf_opt(c_loc(inp(1)), N, method, c_loc(par(1)), M)
    call disp(par)

    !! pacf_opt : Method 2 MLE (Box-Jenkins) 
    method = 2
    call disp('pacf_opt Method 2 MLE (Box-Jenkins)')
    call pacf_opt(c_loc(inp(1)), N, method, c_loc(par(1)), M)
    call disp(par)

    deallocate(inp, par)

end program test_model_pacf
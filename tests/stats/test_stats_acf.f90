program test_acf_acf

    use forlab_io, only: disp, file
    use fortsa_stats, only: acvf, acvf_opt, acvf2acf
    implicit none
    type(file) :: infile
    real(8), allocatable :: inp(:), acf(:)
    integer :: method, i

    infile = file('example/data/seriesC.txt', 'r')
    call infile%open()
    call infile%countlines()
    allocate (inp(infile%lines), acf(10))
    call disp(infile%lines, 'Linenumber in file is: ')

    do i = 1, infile%lines
        read (infile%unit, *) inp(i)
    end do

    call disp('Default Method: acvf')
    call acvf(inp, infile%lines, acf, 10)
    call disp(acf)

    method = 0
    call disp('acvf_opt Method 0 General Method:')
    call acvf_opt(inp, infile%lines, method, acf, 10)
    call disp(acf)

    method = 1
    call disp('acvf_opt Method 1 FFT Method:')
    call acvf_opt(inp, infile%lines, method, acf, 10)
    call disp(acf)

    call disp('Autocorrelation:')
    call acvf2acf(acf, 10)
    call disp(acf)

    deallocate (inp)
    deallocate (acf)
    call infile%close()

end program test_acf_acf

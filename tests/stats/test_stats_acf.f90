module test_stats

    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_stats

contains

    subroutine collect_stats(testsuite)

        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("vaild", test_stats_acf) &
                    ]

    end subroutine collect_stats

    subroutine test_stats_acf(error)

        use forlab_io, only: file
        use fortsa_stats, only: acvf, acvf_opt, acvf2acf
        type(error_type), allocatable, intent(out) :: error
        type(file) :: infile
        real(8), allocatable :: inp(:), acf(:)
        integer :: i
        real(8) :: acf_(10)

        infile = file('example/data/seriesC.txt', 'r')
        call infile%open()
        call infile%countlines()
        call check(error, infile%lines, 226)
        if (allocated(error)) return

        allocate (inp(infile%lines), acf(10))
        do i = 1, infile%lines
            read (infile%unit, *) inp(i)
        end do
        call infile%close()
        
        acf_ = [4.223, 4.128, 3.987, 3.810, 3.608, 3.388, 3.157, 2.922, 2.683, 2.444]
        call acvf(inp, infile%lines, acf, 10)
        do i = 1, 10
            call check(error, acf(i), acf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do
        
        call acvf_opt(inp, infile%lines, 0, acf, 10)
        do i = 1, 10
            call check(error, acf(i), acf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do
        
        call acvf_opt(inp, infile%lines, 1, acf, 10)
        do i = 1, 10
            call check(error, acf(i), acf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do
        
        acf_ = [1.000, 0.9776, 0.9441, 0.9022, 0.8543, 0.8024, 0.7476, 0.6919, 0.6354, 0.5788]
        call acvf2acf(acf, 10)
        do i = 1, 10
            call check(error, acf(i), acf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do
        
    end subroutine test_stats_acf

end module test_stats

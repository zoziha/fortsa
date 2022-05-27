module test_stats

    use fortsa_stats_m, only: acvf, acvf_opt, acvf2acf
    use fortsa_stats_m, only: pacf, pacf_opt
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_stats

contains

    subroutine collect_stats(testsuite)

        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("FCN: test_stats_acf", test_stats_acf), &
                    new_unittest("FCN: test_stats_pacf", test_stats_pacf) &
                    ]

    end subroutine collect_stats

    subroutine test_stats_acf(error)
        type(error_type), allocatable, intent(out) :: error
        real(8), allocatable :: inp(:), acf(:)
        integer :: i, line_num
        real(8) :: acf_(10)

        line_num = countlines('example/data/seriesC.txt')
        open (1, file='example/data/seriesC.txt')
        call check(error, line_num, 226)
        if (allocated(error)) return

        allocate (inp(line_num), acf(10))
        do i = 1, line_num
            read (1, *) inp(i)
        end do
        close (1)

        acf_ = [4.223, 4.128, 3.987, 3.810, 3.608, 3.388, 3.157, 2.922, 2.683, 2.444]
        call acvf(inp, 226, acf, 10)
        do i = 1, 10
            call check(error, acf(i), acf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do

        call acvf_opt(inp, 226, 0, acf, 10)
        do i = 1, 10
            call check(error, acf(i), acf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do

        !> FFT Based Method
        call acvf_opt(inp, 226, 1, acf, 10)
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

    subroutine test_stats_pacf(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i, line_num, unit
        real(8), allocatable :: inp(:), par(:)
        real(8) :: pacf_(10)

        line_num = countlines('example/data/seriesC.txt')
        open (newunit=unit, file='example/data/seriesC.txt')
        call check(error, line_num, 226)
        if (allocated(error)) return

        allocate (inp(line_num), par(10))
        do i = 1, line_num
            read (unit, *) inp(i)
        end do
        close (unit)

        pacf_ = [0.9776, -0.2602, -0.1570, -0.9331E-01, -0.5745E-01, &
                 -0.4563E-01, -0.1217E-01, -0.3751E-01, -0.2236E-01, -0.9845E-02]
        !> Default Method is Yule-Walker
        call pacf(inp, 226, par, 10)
        do i = 1, 10
            call check(error, par(i), pacf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do

        !> pacf_opt : Method 0 Yule Walker
        call pacf_opt(inp, 226, 0, par, 10)
        do i = 1, 10
            call check(error, par(i), pacf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do

        pacf_ = [0.9999, -0.8173, 0.1115E-01, 0.1499E-01, -0.5958E-01, &
                 -0.3016E-01, 0.3299E-01, 0.6476E-02, 0.8042E-01, -0.7123E-02]
        !> pacf_opt : Method 1 Burg
        call pacf_opt(inp, 226, 1, par, 10)
        do i = 1, 10
            call check(error, par(i), pacf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do

        !> pacf_opt : Method 2 MLE (Box-Jenkins)
        pacf_ = [1.000, -0.8325, -0.2928E-01, -0.2786E-01, -0.1042, &
                 -0.7958E-01, -0.8415E-02, -0.3885E-01, 0.4349E-01, -0.4086E-01]
        call pacf_opt(inp, 226, 2, par, 10)
        do i = 1, 10
            call check(error, par(i), pacf_(i), thr=0.001_8)
            if (allocated(error)) return
        end do

    end subroutine test_stats_pacf

    integer function countlines(filename)
        character(*), intent(in) :: filename
        integer :: fid, ierr
        countlines = 0
        open (newunit=fid, file=filename)
        do
            read (fid, *, iostat=ierr)
            if (is_iostat_end(ierr)) exit
            countlines = countlines + 1
        end do
        close (fid)
    end function countlines

end module test_stats

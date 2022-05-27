module test_dwt_m

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_char, c_f_pointer, c_null_char
    use fortsa_dwt_m, only: wave_init, wt_init, wave_summary, wt_summary, modwt, imodwt, &
                          wave_free, wt_free, wt_set
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none

    public :: collect_dwt

contains

    subroutine collect_dwt(testsuite)

        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("FCN: test_dwt", test_dwt) &
                    ]

    end subroutine collect_dwt

    subroutine test_dwt(error)
        type(error_type), allocatable, intent(out) :: error
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

        integer :: line_num

        obj = wave_init('db4'//c_null_char)
        call wave_summary(obj)

        line_num = countlines('example/data/signal.txt')
        print *, line_num
        open (1, file='example/data/signal.txt')
        allocate (data(line_num))

        do i = 1, line_num
            read (1, *) data(i)
        end do
        close (1)
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
        !!\TODO: c_f_pointer, page427

        call imodwt(wt, out)

        do i = 1, wt_%siglength
            diff(i) = out(i) - inp(i)
        end do
        !!\TODO: detto

        call wt_summary(wt)

        call wave_free(obj)
        call wt_free(wt)
        deallocate (inp, out, diff, data)

    contains

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

    end subroutine test_dwt

end module test_dwt_m

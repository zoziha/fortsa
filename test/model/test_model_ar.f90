module test_model_ar

    use fortsa_model_m, only: ar_init, ar_exec, &
                              ar_summary, ar_predict, &
                              ar_free
    use, intrinsic :: iso_c_binding, only: c_loc, c_null_ptr, c_ptr
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_model_ar

contains

    subroutine collect_model_ar(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test_model_ar", test_model_ar_vaild) &
                    ]

    end subroutine collect_model_ar

    subroutine test_model_ar_vaild(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: i, d, L
        integer :: p, q, line_num
        real(8), target, allocatable :: xpred(:), amse(:)
        type(c_ptr) :: obj = c_null_ptr
        target obj
        ! type(arima_set), target :: set
        ! target set
        ! type(arima_set) :: obj
        character(*), parameter :: file_name = 'ctsa/data/seriesA.txt'
        real(8), target, allocatable :: inp(:)

        p = 0
        d = 1
        q = 0

        L = 5

        line_num = countlines(file_name)
        open (1, file=file_name)
        allocate (inp(line_num), xpred(L), amse(L))

        do i = 1, line_num
            read (1, *) inp(i)
        end do

        obj = ar_init(0, line_num)
        call ar_exec(obj, inp)
        call ar_summary(obj)
        call ar_predict(obj, inp, L, xpred, amse)

        print "(a)", "Predicted Values : "
        print *, xpred

        print "(a)", "Standard Errors : "
        print *, sqrt(amse)

        call ar_free(obj)
        deallocate (inp, xpred, amse)
        close (1)

    end subroutine test_model_ar_vaild

    include "../countlines.finc"

end module test_model_ar

!> author: 左志华
!> date: 2022-07-08
!>
!> fortsa_dwt (module) presents for test.
!> 小波变换 Dwt
module fortsa_dwt_m

    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_double
    implicit none
    private

    public :: wave_init, wt_init, wave_summary, wt_summary, modwt, imodwt, &
              wave_free, wt_free, wt_set

    type, bind(c) :: wt_set
        type(c_ptr) :: wave
        type(c_ptr) :: cobj
        character(kind=c_char), dimension(10) :: method
        integer(kind=c_int) :: siglength                    !! Length of the original signal.
        integer(kind=c_int) :: modwtsiglength               !! Modified signal length for MODWT
        integer(kind=c_int) :: outlength                    !! Length of the output DWT vector
        integer(kind=c_int) :: lenlength
                                !! Length of the Output Dimension Vector "length"
        integer(kind=c_int) :: J                            !! Number of decomposition Levels
        integer(kind=c_int) :: MaxIter                      !! Maximum Iterations J <= MaxIter
        integer(kind=c_int) :: even
                                !! even = 1 if signal is of even length. even = 0 otherwise
        character(kind=c_char), dimension(10) :: ext
                                !! Type of Extension used - "per" or "sym"
        character(kind=c_char), dimension(10) :: cmethod    !! Convolution Method - "direct" or "FFT"

        integer(kind=c_int) :: N
        integer(kind=c_int) :: cfftset
        integer(kind=c_int) :: zpad
        integer(kind=c_int) :: length(102)
        type(c_ptr) :: output
        real(kind=c_double), dimension(0) :: params
    end type wt_set

    interface

        function wave_init(wname) bind(c)
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: wname(*)
            type(c_ptr) :: wave_init                            !! pointer point to `wave_object`
        end function wave_init

        function wt_init(wave, method, siglength, J) bind(c)
            import :: c_int, c_ptr, c_char
            type(c_ptr), value :: wave                          !! pointer point to `wave_object`
            character(kind=c_char), intent(in) :: method(*)     !! const char*
            integer(c_int), value :: siglength, J
            type(c_ptr) :: wt_init                              !! wt_object
        end function wt_init

        subroutine wave_summary(obj) bind(c)
            import :: c_ptr
            type(c_ptr), value :: obj                   !! `wave_object`
        end subroutine wave_summary

        subroutine wt_summary(wt) bind(c)
            import :: c_ptr
            type(c_ptr), value :: wt                    !! wt_object
        end subroutine wt_summary

        subroutine modwt(wt, inp) bind(c)
            import :: c_ptr, c_double
            type(c_ptr), value :: wt                    !! wt_object
            real(kind=c_double), intent(in) :: inp(*)   !! const double*
        end subroutine modwt

        subroutine imodwt(wt, dwtop) bind(c)
            import :: c_ptr, c_double
            type(c_ptr), value :: wt                    !! wt_object
            real(kind=c_double) :: dwtop(*)
        end subroutine imodwt

        subroutine wave_free(object) bind(c)
            import :: c_ptr
            type(c_ptr), value :: object                !! wave_object
        end subroutine wave_free

        subroutine wt_free(object) bind(c)
            import :: c_ptr
            type(c_ptr), value :: object                !! wt_object
        end subroutine wt_free

    end interface

end module fortsa_dwt_m

module fortsa_stats
    implicit none
    private

    public :: acvf, acvf_opt, acvf2acf
    public :: pacf, pacf_opt

    interface
        subroutine acvf(vec, N, par, M) bind(c, name='acvf')
            use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr
            type(c_ptr), value :: vec, par
            integer(kind=c_int), value :: N, M
        end subroutine acvf

        subroutine acvf_opt(vec, N, method, par, M) bind(c, name='acvf_opt')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: vec, par
            integer(kind=c_int), value :: N, method, M
        end subroutine acvf_opt

        subroutine acvf2acf(acf, M) bind(c, name='acvf2acf')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: acf
            integer(kind=c_int), value :: M
        end subroutine acvf2acf

        subroutine pacf(vec, N, par, M) bind(c, name='pacf')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: vec, par
                !! `c_double` type
            integer(kind=c_int), value :: N, M
        end subroutine pacf

        subroutine pacf_opt(vec, N, method, par, M) bind(c, name='pacf_opt')
            use, intrinsic :: iso_c_binding, only: c_int, c_ptr
            type(c_ptr), value :: vec, par
            integer(kind=c_int), value :: N, method, M
        end subroutine pacf_opt
    end interface
end module fortsa_stats
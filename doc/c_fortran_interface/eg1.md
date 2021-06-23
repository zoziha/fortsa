! Fortran program
use iso_c_binding
interface
subroutine swap(a,b) bind(c)
use iso_c_binding
type(c_ptr), value :: a,b
end subroutine swap
end interface
integer(c_int), target:: a=10,b=20
print *, a, b
call swap(c_loc(a), c_loc(b))
print *,a, b
end

/* c program */
void swap (int *a, int *b)
{ int t;
t=*a;
*a=*b;
*b=t;
}
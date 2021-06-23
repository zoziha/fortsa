## C-Fortran Interoperability
### Pointer in c struct

```c
struct c_struct{
    int N;
    double *phi;
}
```
Correspondingly:
```fortran
type, bind(c) :: f_type
    integer(c_int) :: n
    type(c_ptr) :: p
end type
```
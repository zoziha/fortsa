# Workflows

## Plans
### CTSA Translation
All the examples and interfaces in ctsa are implemented through Fortran, and they can run correctly.

### Robust Fortran Interface
Encapsulate the c-fortran interface to avoid dynamic memory leaks.

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
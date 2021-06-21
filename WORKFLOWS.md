# Workflows

## 

## C-Fortran Interoperability
### pointer in c struct

```c
struct c_struct{
    int N;
    double *phi
}
```
Acco
```fortran
type, bind(c) :: f_type
    integer(c_int) :: n
    type(c_ptr) :: p
edn type
```
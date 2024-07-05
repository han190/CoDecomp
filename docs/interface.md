# Application Programming Interfaces

## `SIZE` -- Determine the size of a decomposition type
### Description
__PURE FUNCTION.__ Determine the size of a `decomposition_type` along a specified dimension _DIM_ and an option _OPT_. If _DIM_ is present, the size along the specified dimension is returned. The default _DIM_ is zero, and the product of shape will be returned. The _OPT_ shall be one of the "local", "global" or "local_max". The default _OPT_ is "local".

### Syntax
```
decomp_size = SIZE(decomp[, dim[, opt]])
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp |`class(decomposition_type)`|`intent(in)` |
| dim    | `integer` | `intent(in), optional` |
| opt    | `character(len=*)` | `intent(in), optional` |

### Return value
The return value is of type `INTEGER` and of the default integer kind.

### Example
```fortran
program main
  use, non_intrinsic :: module_decomposition
  implicit none

  type(decomposition_type) :: decomp
  integer, dimension(2) :: num_tasks, num_procs
  integer :: i

  num_tasks = [48, 32]
  num_procs = [2, num_images()/2]
  call decompose(decomp, num_tasks, num_procs)
  if (this_image() == 1) then
    print "(*(i0, 1x))", &
      & (size(decomp, dim=i, opt="global"), i=0, 2) ! 1536 48 32
  end if
end program main
```

## `SHAPE` -- Determine the shape of a decomposition type
### Description
__PURE FUNCTION.__ Determine the shape of a decomposition type.

### Syntax
```
decomp_size = SHAPE(decomp[, opt])
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp |`class(decomposition_type)`|`intent(in)` |
| opt    | `character(len=*)` | `intent(in), optional` |

### Return value
An `INTEGER` array of rank one with as many elements as `decomp%num_ranks`.

### Example
```fortran
program main
  use, non_intrinsic :: module_decomposition
  implicit none

  type(decomposition_type) :: decomp
  integer, dimension(2) :: num_tasks, num_procs

  num_tasks = [48, 32]
  num_procs = [2, num_images()/2]
  call decompose(decomp, num_tasks, num_procs)
  if (this_image() == 1) print "(*(i0, 1x))", SHAPE(decomp) ! 48 32
end program main
```

## `THIS_IMAGE` -- Function that returns the cosubscript index of a decomposition type
### Description
__THIS_IMAGE.__ Returns the cosubscript for this decomposition type.

### Syntax
```
image = THIS_IMAGE(decomp[, dim])
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp |`class(decomposition_type)`|`intent(in)` |
| dim    | `integer` | `intent(in), optional` |

### Return value
The return value is of type `INTEGER` and of the default integer kind.

### Example
```fortran
program main
  use, non_intrinsic :: module_decomposition
  implicit none

  type(decomposition_type) :: decomp
  integer, dimension(2) :: num_tasks, num_procs

  num_tasks = [48, 32]
  num_procs = [2, num_images()/2]
  call decompose(decomp, num_tasks, num_procs)
  if (this_image() == 1) print "(*(i0, 1x))", THIS_IMAGE(decomp) ! 1
end program main
```
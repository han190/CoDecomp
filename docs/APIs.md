# APIs

## `DECOMPOSE`
### Description
__PURE SUBROUTINE.__ Domain decomposition tool. If manual decomposition is used, both `num_tasks` and `num_procs` must present, and the size of the two shall be the same. If automatic decomposition is applied, then we need at least the total number of tasks. The subroutine will try to decompose images into combinations that are as close as possible to each other. For example, if 12 images is decomposed into a rank-2 decomposition, the result would be 4x3.

### Syntax
```
call DECOMPOSE(decomp, num_tasks, num_procs)
call DECOMPOSE(decomp, num_tasks[, num_ranks[, num_procs]])
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(out)` |
| num_tasks | `integer` | `dimension(:), intent(in)` |
| num_procs | `integer` | `dimension(:), intent(in)` |

| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(out)` |
| num_tasks | `integer` | `integer(in)` |
| num_ranks | `integer` | `optional, intent(in)` |
| num_procs | `integer` | `optional, intent(in)` |

## `SIZE`
### Description
__PURE FUNCTION.__ Determine the size of a `decomposition_type` along a specified dimension _DIM_ and an option _OPT_. If _DIM_ is present, the size along the specified dimension is returned. The default _DIM_ is zero, and the product of shape will be returned. The _OPT_ shall be one of the "local", "global" or "local_max". The default _OPT_ is "local".

### Syntax
```
decomp_size = SIZE(decomp[, dim[, opt]])
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(in)` |
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

## `SHAPE`
### Description
__PURE FUNCTION.__ Determine the shape of a decomposition type.

### Syntax
```
decomp_size = SHAPE(decomp[, opt])
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(in)` |
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

## `BASE_INDEX`
### Description
__PURE FUNCTION.__ Returns the global base index of the current image.

### Syntax
```
result = BASE_INDEX(decomp)
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(in)` |

### Return value
An `INTEGER` array that is `decomp%base_index`.

## `REMAINDER`
### Description
__PURE FUNCTION.__ Returns the remainder of the current image. Zero if domain decomposition is balanced. Otherwise, a nonzer value of `decomp%remainder` is returned.

### Syntax
```
result = REMAINDER(decomp)
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(in)` |

## `THIS_IMAGE`
### Description
__PURE FUNCTION.__ Returns the cosubscript for this decomposition type.

### Syntax
```
result = THIS_IMAGE(decomp[, dim])
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(in)` |
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

## `COSHAPE`
### Description
__PURE FUNCTION.__ Returns the sizes of codimensions of a decomposition type.

### Syntax
```
result = COSHAPE(decomp)
```

### Arguments
| Name   | Type & Kind | Attribute |
|:-------|:------------|:---------|
| decomp | `class(decomposition_type)` | `intent(in)` |

### Return value
An `INTEGER` array that is `decomp%num_procs`.
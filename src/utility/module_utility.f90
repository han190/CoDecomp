module module_utility

use, intrinsic :: iso_fortran_env, only: int64
implicit none

public :: convert
public :: reallocate
public :: optional_argument
private

!> Convert index from array/integer form to integer/array form.
interface convert
  module procedure :: convert_arr2int
  module procedure :: convert_int2arr
end interface convert

!> Reallocate
interface reallocate
  module procedure :: reallocate_complex32_3d
  module procedure :: reallocate_int32_1d
  module procedure :: reallocate_int64_1d
  module procedure :: reallocate_real32_2d
  module procedure :: reallocate_real32_3d
end interface reallocate

!> Fill optional argument with default value if not present.
interface optional_argument
  module procedure :: optional_arg_int32
  module procedure :: optional_arg_char
end interface optional_argument

interface
  !> Convert index from array form to integer form.
  pure module function convert_arr2int(shapes, index_arr) result(index_int)
    integer, intent(in) :: shapes(:), index_arr(:)
    integer :: index_int
  end function convert_arr2int

  !> Convert index from integer form to array form.
  pure module function convert_int2arr(shapes, index_int) result(index_arr)
    integer, intent(in) :: shapes(:), index_int
    integer, allocatable :: index_arr(:)
  end function convert_int2arr

  !> Reallocate 1D integer array.
  pure module subroutine reallocate_int32_1d(array, new_size)
    integer, allocatable, intent(inout) :: array(:)
    integer, intent(in) :: new_size
  end subroutine reallocate_int32_1d

  !> Reallocate 1D long integer array.
  pure module subroutine reallocate_int64_1d(array, new_size)
    integer(int64), allocatable, intent(inout) :: array(:)
    integer(int64), intent(in) :: new_size
  end subroutine reallocate_int64_1d

  !> Reallocate 2D real array.
  pure module subroutine reallocate_real32_2d(array, new_size)
    real, allocatable, intent(inout) :: array(:, :)
    integer, intent(in) :: new_size(:)
  end subroutine reallocate_real32_2d

  !> Reallocate 3D real array.
  pure module subroutine reallocate_real32_3d(array, new_size)
    real, allocatable, intent(inout) :: array(:, :, :)
    integer, intent(in) :: new_size(:)
  end subroutine reallocate_real32_3d

  !> Reallocate 3D complex array.
  pure module subroutine reallocate_complex32_3d(array, new_size)
    complex, allocatable, intent(inout) :: array(:, :, :)
    integer, intent(in) :: new_size(:)
  end subroutine reallocate_complex32_3d

  !> Fill optional argument (int32) with default value if not present.
  pure module function optional_arg_int32(opt_arg, default_val) result(ret)
    integer, intent(in), optional :: opt_arg
    integer, intent(in) :: default_val
    integer :: ret
  end function optional_arg_int32

  !> Fill optional argument (char) with default value if not present.
  pure module function optional_arg_char(opt_arg, default_val) result(ret)
    character(len=*), intent(in), optional :: opt_arg
    character(len=*), intent(in) :: default_val
    character(len=:), allocatable :: ret
  end function optional_arg_char
end interface

end module module_utility

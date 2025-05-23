module module_decomposition

use, non_intrinsic :: module_utility, only: convert, optional_argument
implicit none

public :: decomposition
public :: decompose
public :: write(formatted)
public :: size, shape
public :: coshape, this_image
private

!> Decomposition type
type :: decomposition(rank)
  integer, len :: rank
  integer :: global_size(rank)
  integer :: num_procs(rank)
  integer :: local_size_max(rank)
  integer :: local_size(rank)
  integer :: co_index(rank)
  integer :: remainder(rank)
  integer :: base_index(rank)
end type decomposition

!> Wrapper decompose
interface decompose
  module procedure :: decompose_manual
end interface decompose

!> UDDTIO for decomposition
interface write(formatted)
  module procedure :: write_formatted
end interface write(formatted)

!> Override intrinsic size
interface size
  module procedure :: get_size
end interface size

!> Override intrinsic shape
interface shape
  module procedure :: get_shape
end interface shape

!> Override intrinsic coshape
interface coshape
  module procedure :: get_coshape
end interface coshape

!> Override intrinsic this_image
interface this_image
  module procedure :: get_thisimage
end interface this_image

interface
  !> Decomposition type constructor
  module function decompose_manual(num_tasks, num_procs) result(decomp)
    integer, intent(in) :: num_tasks(:), num_procs(:)
    type(decomposition(rank=:)), allocatable :: decomp
  end function decompose_manual

  !> write(formatted)
  module subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(decomposition(rank=*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
  end subroutine write_formatted

  !> Compute local index from global index
  module function get_location(decomp, global_index, recompute) result(local_index)
    type(decomposition(rank=*)), intent(inout) :: decomp
    integer, intent(in) :: global_index(:)
    logical, intent(in), optional :: recompute
    integer, allocatable :: local_index(:)
  end function get_location

  !> The "size" function for decomposition
  pure module function get_size(decomp, dim, opt) result(ret)
    type(decomposition(rank=*)), intent(in) :: decomp
    integer, intent(in), optional :: dim
    character(len=*), intent(in), optional :: opt
    integer :: ret
  end function get_size

  !> The `shape` function for decomposition
  pure module function get_shape(decomp, opt) result(ret)
    type(decomposition(rank=*)), intent(in) :: decomp
    character(len=*), intent(in), optional :: opt
    integer, allocatable :: ret(:)
  end function get_shape

  !> Get base index
  pure module function base_index(decomp) result(ret)
    type(decomposition(rank=*)), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function base_index

  !> Get remainder
  pure module function remainder(decomp) result(ret)
    type(decomposition(rank=*)), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function remainder

  !> Get number of processors
  pure module function get_coshape(decomp) result(ret)
    type(decomposition(rank=*)), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function get_coshape

  !> Get co_index
  pure module function get_thisimage(decomp, dim) result(ret)
    type(decomposition(rank=*)), intent(in) :: decomp
    integer, intent(in), optional :: dim
    integer :: ret
  end function get_thisimage
end interface

end module module_decomposition
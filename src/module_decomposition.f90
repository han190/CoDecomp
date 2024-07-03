module module_decomposition

use, non_intrinsic :: module_utility, only: convert, optional_argument
implicit none

public :: decomposition_type
public :: decompose
public :: write(formatted)
public :: get_location
public :: copy_decomp
public :: size
public :: shape
public :: base_index
public :: remainder
public :: coshape
public :: this_image
private

!> Decomposition type
type :: decomposition_type
  integer :: num_ranks
  integer, allocatable :: global_size(:)
  integer, allocatable :: num_procs(:)
  integer, allocatable :: local_size_max(:)
  integer, allocatable :: local_size(:)
  integer, allocatable :: co_index(:)
  integer, allocatable :: remainder(:)
  integer, allocatable :: base_index(:)
end type decomposition_type

!> Wrapper of decompose procedures
interface decompose
  module procedure :: decompose_manual
  module procedure :: decompose_auto
end interface decompose

!> UDDTIO for decomposition_type
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
  module subroutine decompose_manual(decomp, num_tasks, num_procs)
    class(decomposition_type), intent(out) :: decomp
    integer, intent(in) :: num_tasks(:), num_procs(:)
  end subroutine decompose_manual

  !> Compute local index from global index.
  module function get_location(decomp, global_index, recompute) result(local_index)
    class(decomposition_type), intent(inout) :: decomp
    integer, intent(in) :: global_index(:)
    logical, intent(in), optional :: recompute
    integer, allocatable :: local_index(:)
  end function get_location

  !> Copy metadata
  module subroutine copy_decomp(from, to)
    class(decomposition_type), intent(in) :: from
    class(decomposition_type), intent(out) :: to
  end subroutine copy_decomp

  !> Automatically decompose tasks based on number of ranks provided.
  module subroutine decompose_auto(decomp, num_tasks, num_ranks, num_procs)
    class(decomposition_type), intent(out) :: decomp
    integer, intent(in) :: num_tasks
    integer, intent(in), optional :: num_ranks, num_procs
  end subroutine decompose_auto

  !> write(formatted)
  module subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    class(decomposition_type), intent(in) :: dtv
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list (:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
  end subroutine write_formatted

  !> The "size" function for decomposition_type
  pure module function get_size(decomp, dim, opt) result(ret)
    class(decomposition_type), intent(in) :: decomp
    integer, intent(in), optional :: dim
    character(len=*), intent(in), optional :: opt
    integer :: ret
  end function get_size

  !> The `shape` function for decomposition_type
  pure module function get_shape(decomp, opt) result(ret)
    class(decomposition_type), intent(in) :: decomp
    character(len=*), intent(in), optional :: opt
    integer, allocatable :: ret(:)
  end function get_shape

  !> Get base index
  pure module function base_index(decomp) result(ret)
    class(decomposition_type), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function base_index

  !> Get remainder
  pure module function remainder(decomp) result(ret)
    class(decomposition_type), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function remainder

  !> Get number of processors
  pure module function get_coshape(decomp) result(ret)
    class(decomposition_type), intent(in) :: decomp
    integer, allocatable :: ret(:)
  end function get_coshape

  !> Get co_index
  pure module function get_thisimage(decomp, dim) result(ret)
    class(decomposition_type), intent(in) :: decomp
    integer, intent(in), optional :: dim
    integer :: ret
  end function get_thisimage
end interface

end module module_decomposition

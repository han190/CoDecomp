module module_matrix

use, non_intrinsic :: module_decomposition
use, non_intrinsic :: module_utility
implicit none

!> Integer Row partitioned 2D matrix type
type, extends(decomposition_type) :: matrix_type
  integer, allocatable :: values(:,:)
end type matrix_type

contains

!> Decompose matrix.
function decompose_matrix(num_tasks, num_procs) result(matrix)
  integer, intent(in) :: num_tasks(2), num_procs(2)
  type(matrix_type) :: matrix

  call decompose(matrix, num_tasks, num_procs)
  if (allocated(matrix%values)) deallocate (matrix%values)
  allocate (matrix%values(matrix%local_size(1), matrix%local_size(2)))
end function decompose_matrix

!> Fill matrix with an integer global index.
subroutine fill_matrix(matrix)
  type(matrix_type), intent(inout) :: matrix
  integer :: global_index(2), i, j

  associate (global_size => shape(matrix, opt="global"), &
    & local_size => shape(matrix, opt="local"))
    do concurrent (i=1:local_size(1), j=1:local_size(2))
      global_index = base_index(matrix) + [i,j]
      matrix%values(i,j) = convert(global_size, global_index)
    end do
  end associate
end subroutine fill_matrix

!> Swap a 2-element array
pure function swap(array) result(swapped)
  integer, intent(in) :: array(2)
  integer :: swapped(2)

  swapped = [array(2), array(1)]
end function swap

!> Transpose matrix
function transpose_matrix(matrix) result(transposed)
  type(matrix_type), intent(in) :: matrix
  type(matrix_type) :: transposed
  integer, allocatable :: buffer(:,:)[:]
  integer :: global_size(2), local_size(2), local_size_max(2)
  integer :: num_procs(2)!, this_proc(2)

  !> Transpose local image
  global_size = shape(matrix, opt="global")
  num_procs = coshape(matrix)
  transposed = decompose_matrix(swap(global_size), swap(num_procs))

  local_size = shape(matrix, opt="local")
  local_size_max = shape(matrix, opt="local_max")
  allocate (buffer(local_size_max(2), local_size_max(1))[*])
  buffer(1:local_size(2), 1:local_size(1)) = transpose( &
    & matrix%values(1:local_size(1), 1:local_size(2)))
end function transpose_matrix

end module module_matrix

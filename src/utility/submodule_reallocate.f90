submodule(module_utility) submodule_reallocate
implicit none
contains

!> Reallocate 1D integer array.
pure module subroutine reallocate_int32_1d(array, new_size)
  integer, allocatable, intent(inout) :: array(:)
  integer, intent(in) :: new_size

  if (.not. allocated(array)) then
    allocate (array(new_size))
  else if (size(array) /= new_size) then
    deallocate (array)
    allocate (array(new_size))
  end if
end subroutine reallocate_int32_1d

!> Reallocate 1D long integer array.
pure module subroutine reallocate_int64_1d(array, new_size)
  integer(int64), allocatable, intent(inout) :: array(:)
  integer(int64), intent(in) :: new_size

  if (.not. allocated(array)) then
    allocate (array(new_size))
  else if (size(array) /= new_size) then
    deallocate (array)
    allocate (array(new_size))
  end if
end subroutine reallocate_int64_1d

!> Reallocate 2D real array.
pure module subroutine reallocate_real32_2d(array, new_size)
  real, allocatable, intent(inout) :: array(:,:)
  integer, intent(in) :: new_size(:)

  if (rank(array) /= size(new_size)) &
    & error stop "[reallocate_real32_2d] Invalid shape."
  if (.not. allocated(array)) then
    allocate (array(new_size(1), new_size(2)))
  else if (any(shape(array) /= new_size)) then
    deallocate (array)
    allocate (array(new_size(1), new_size(2)))
  end if
end subroutine reallocate_real32_2d

!> Reallocate 3D real array.
pure module subroutine reallocate_real32_3d(array, new_size)
  real, allocatable, intent(inout) :: array(:,:,:)
  integer, intent(in) :: new_size(:)

  if (rank(array) /= size(new_size)) &
    & error stop "[reallocate_real32_3d] Invalid shape."
  if (.not. allocated(array)) then
    allocate (array(new_size(1), new_size(2), new_size(3)))
  else if (any(shape(array) /= new_size)) then
    deallocate (array)
    allocate (array(new_size(1), new_size(2), new_size(3)))
  end if
end subroutine reallocate_real32_3d

!> Reallocate 3D complex array.
pure module subroutine reallocate_complex32_3d(array, new_size)
  complex, allocatable, intent(inout) :: array(:,:,:)
  integer, intent(in) :: new_size(:)

  if (rank(array) /= size(new_size)) &
    & error stop "[reallocate_complex32_3d] Invalid shape."
  if (.not. allocated(array)) then
    allocate (array(new_size(1), new_size(2), new_size(3)))
  else if (any(shape(array) /= new_size)) then
    deallocate (array)
    allocate (array(new_size(1), new_size(2), new_size(3)))
  end if
end subroutine reallocate_complex32_3d

end submodule submodule_reallocate

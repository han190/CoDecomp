submodule(module_utility) submodule_convert
implicit none
contains

!> Convert index from array form to integer form.
pure module function convert_arr2int(shapes, index_arr) result(index_int)
  integer, intent(in) :: shapes(:), index_arr(:)
  integer :: index_int
  integer :: i

  if (size(shapes) /= size(index_arr)) &
    & error stop "[convert_arr2int] Invalid index."
  if (size(shapes) == 1) then
    index_int = index_arr(1)
    return
  end if

  index_int = index_arr(1)
  do i = size(index_arr), 2, -1
    index_int = index_int + (index_arr(i) - 1) * product(shapes(:i - 1))
  end do
end function convert_arr2int

!> Convert index from integer form to array form.
pure module function convert_int2arr(shapes, index_int) result(index_arr)
  integer, intent(in) :: shapes(:), index_int
  integer, allocatable :: index_arr(:)
  integer :: tmp, rem, i

  if (index_int > product(shapes)) &
    & error stop "[convert_int2arr] Invalid index."
  if (size(shapes) == 1) then
    index_arr = [index_int]
    return
  end if

  call reallocate(index_arr, size(shapes))
  tmp = index_int
  do i = size(shapes), 2, -1
    associate (p => product(shapes(1:i - 1)))
      rem = tmp - (tmp - 1) / p * p
      index_arr(i) = (tmp - rem) / p + 1
      tmp = rem
    end associate
  end do
  index_arr(1) = rem
end function convert_int2arr

end submodule submodule_convert

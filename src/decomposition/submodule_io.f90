submodule(module_decomposition) submodule_io
implicit none
contains

!> Convert integer to string.
pure function int2str(n) result(str)
  integer, intent(in) :: n
  character(len=:), allocatable :: str
  integer :: m

  m = floor(log10(real(abs(n)))) + 1
  if (n < 0) m = m + 1

  if (allocated(str)) then
    if (len(str) /= m) then
      deallocate (str)
      allocate (character(len=m) :: str)
    end if
  else
    allocate (character(len=m) :: str)
  end if
  write (str, "(i0)") n
end function int2str

!> write(formatted)
module subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
  class(decomposition(rank=*)), intent(in) :: dtv
  integer, intent(in) :: unit
  character(len=*), intent(in) :: iotype
  integer, intent(in) :: v_list (:)
  integer, intent(out) :: iostat
  character(len=*), intent(inout) :: iomsg
  character(len=:), allocatable :: write_format
  integer, parameter :: num_comps = 7
  character(len=20) :: component_names(num_comps)
  integer, allocatable :: components(:,:)
  integer :: num_ranks, i, i_start, i_end

  num_ranks = dtv%rank
  write_format = "(a, ':', "//int2str(num_ranks)//"(i0, ','), tl1, ';')"

  component_names = [character(len=20) :: "N", "P", "M", "R", "U", "A", "K"]
  components = reshape([dtv%global_size, dtv%num_procs, dtv%local_size_max, &
    & dtv%remainder, dtv%local_size, dtv%co_index, dtv%base_index], &
    & [num_ranks, num_comps])

  iostat = 0
  write (unit, "(a, 1x)") "[DECOMP"

  select case (trim(iotype))
  case ("LISTDIRECTED")
    do i = 5, 7
      write (unit, fmt=write_format, iostat=iostat) &
        & trim(component_names(i)), components(:, i)
    end do
  case ("DT")
    select case (size(v_list))
    case (0)
      i_start = 5
      i_end = 7
    case (1)
      i_start = max(v_list(1), 1)
      i_end = 7
    case (2)
      i_start = max(v_list(1), 1)
      i_end = min(v_list(size(v_list)), 7)
    case default
      error stop "Invalid size of v_list."
    end select

    do i = i_start, i_end
      write (unit, fmt=write_format, iostat=iostat) &
        & trim(component_names(i)), components(:, i)
    end do
  case default
    error stop "[write_formatted] Invalid format."
  end select
  write (unit, "(tl1, a)", iostat=iostat) "]"
end subroutine write_formatted

end submodule submodule_io

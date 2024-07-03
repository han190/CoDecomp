submodule(module_decomposition) submodule_io
implicit none
contains

!> Convert integer to string.
pure function integer_to_string(n) result(str)
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
end function integer_to_string

!> write(formatted)
module subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
  class(decomposition_type), intent(in) :: dtv
  integer, intent(in) :: unit
  character(len=*), intent(in) :: iotype
  integer, intent(in) :: v_list (:)
  integer, intent(out) :: iostat
  character(len=*), intent(inout) :: iomsg
  character(len=:), allocatable :: write_format
  integer, parameter :: num_comps = 7
  character(len=20) :: component_names(num_comps)
  integer, allocatable :: components(:,:)
  integer :: num_ranks, i

  !> Avoid compiler blames.
  associate (v_list_ => v_list, iomsg_ => iomsg)
  end associate

  num_ranks = dtv%num_ranks
  write_format = "(2x, a, 1x, '::', 1x, '[', "// &
    & integer_to_string(num_ranks)//"(i0, ','), tl1, ']', /)"

  component_names = [character(len=25) :: &
    & "Decomposition", &
    & "Global size", "Local size", &
    & "Max local size", "Co-index", &
    & "Remainder", "Global based index"]
  components = reshape([dtv%num_procs, dtv%global_size, &
    & dtv%local_size, dtv%local_size_max, &
    & dtv%co_index, dtv%remainder, dtv%base_index], &
    & [num_ranks, num_comps])

  select case (trim(iotype))
  case ("LISTDIRECTED", "DT")
    iostat = 0
    write (unit, "(/, a, /)") "<<< decomposition_type"
    write (unit, fmt="(2x, a, 1x, '::', 1x, i0, /)", iostat=iostat) &
      & "Number of ranks", dtv%num_ranks
    do i = 1, num_comps
      write (unit, fmt=write_format, iostat=iostat) &
        & trim(component_names(i)), components(:, i)
    end do
    write (unit, "(a, /)") ">>>"
  case default
    error stop "[write_formatted] Invalid format."
  end select
end subroutine write_formatted

end submodule submodule_io

submodule(module_decomposition) submodule_auto
implicit none
contains

!> Prime factorization
pure function prime_factors(n) result(factors)
  integer, intent(in) :: n
  integer, allocatable :: factors(:)
  integer :: i, tmp

  i = 2
  tmp = n

  if (allocated(factors)) deallocate (factors)
  factors = [integer ::]

  do while (i * i <= tmp)
    if (modulo(tmp, i) /= 0) then
      i = i + 1
    else
      tmp = tmp / i
      factors = [factors, i]
    end if
  end do
  if (tmp > 1) factors = [factors, tmp]
end function prime_factors

!> Group prime factors
pure function group_factors(factors, num_groups) result(grouped)
  integer, intent(in) :: factors(:), num_groups
  integer :: grouped(num_groups)
  type :: group_type
    integer, allocatable :: sub_group(:)
  end type group_type
  type(group_type) :: groups(num_groups)
  integer :: products(num_groups), location, i

  groups = group_type([integer :: 1])
  do i = 1, size(factors)
    products = [(product(groups(i)%sub_group), i=1, num_groups)]
    location = minloc(products, dim=1)
    if (location >= 1 .and. location <= num_groups) then
      groups(location)%sub_group = [ &
        & groups(location)%sub_group, factors(i)]
    end if
  end do
  grouped = [(product(groups(i)%sub_group), i=1, num_groups)]
end function group_factors

!> Wrapper of primer_factors and group_factors
pure subroutine factorize(n, grouped)
  integer, intent(in) :: n
  integer, intent(out) :: grouped(:)
  integer :: num_groups

  num_groups = size(grouped)
  grouped = group_factors(prime_factors(n), num_groups)
end subroutine factorize

!> Automatically decompose tasks based on number of ranks provided.
module subroutine decompose_auto(decomp, num_tasks, num_ranks, num_procs)
  class(decomposition_type), intent(out) :: decomp
  integer, intent(in) :: num_tasks
  integer, intent(in), optional :: num_ranks, num_procs
  integer, allocatable :: procs(:), tasks(:)
  integer :: num_procs_, num_ranks_

  num_ranks_ = optional_argument(num_ranks, 1)
  num_procs_ = optional_argument(num_procs, num_images())

  allocate (procs(num_ranks_), tasks(num_ranks_))
  call factorize(num_procs_, procs)
  call factorize(num_tasks, tasks)
  call decompose_manual(decomp, tasks, procs)
end subroutine decompose_auto

end submodule submodule_auto

submodule(module_decomposition) submodule_manual
implicit none
contains

!> Compute remainder
impure elemental function compute_r(n, p) result(r)
  integer, intent(in) :: n, p
  integer :: r

  r = mod(n, p)
end function compute_r

!> Compute maximum local size
impure elemental function compute_m(n, p) result(m)
  integer, intent(in) :: n, p
  integer :: m

  m = (n - 1) / p + 1
end function compute_m

!> Compute local size
impure elemental function compute_m_alpha(r, alpha, m) result(m_alpha)
  integer, intent(in) :: r, alpha, m
  integer :: m_alpha

  if (r == 0) then
    m_alpha = m
    return
  end if

  if (alpha <= r) then
    m_alpha = m
  else
    m_alpha = m - 1
  end if
end function compute_m_alpha

!> Compute co-index
impure elemental function compute_alpha(r, m, k) result(alpha)
  integer, intent(in) :: r, m, k
  integer :: alpha

  if (r == 0) then
    alpha = (k - 1) / m + 1
    return
  end if

  if (k <= m * r) then
    alpha = (k - 1) / m + 1
  else
    alpha = (k - r - 1) / (m - 1) + 1
  end if
end function compute_alpha

!> Compute base index
impure elemental function compute_k0_alpha(r, alpha, m_alpha) result(k0_alpha)
  integer, intent(in) :: r, alpha, m_alpha
  integer :: k0_alpha

  if (r == 0) then
    k0_alpha = (alpha - 1) * m_alpha
    return
  end if

  if (alpha <= r) then
    k0_alpha = (alpha - 1) * m_alpha
  else
    k0_alpha = (alpha - 1) * m_alpha + r
  end if
end function compute_k0_alpha

!> Decomposition type constructor
module subroutine decompose_manual(decomp, num_tasks, num_procs)
  class(decomposition_type), intent(out) :: decomp
  integer, intent(in) :: num_tasks(:), num_procs(:)
  integer, dimension(size(num_tasks)) :: m, r, alpha, m_alpha, k0_alpha

  if (size(num_tasks) /= size(num_procs)) error stop &
    & "[decompose_manual] Invalid processors or size."

  m = compute_m(num_tasks, num_procs)
  r = compute_r(num_tasks, num_procs)
  alpha = convert(num_procs, this_image())
  m_alpha = compute_m_alpha(r, alpha, m)
  k0_alpha = compute_k0_alpha(r, alpha, m_alpha)

  decomp%num_ranks = size(num_tasks)
  decomp%global_size = num_tasks
  decomp%num_procs = num_procs
  decomp%local_size_max = m
  decomp%remainder = r
  decomp%co_index = alpha
  decomp%local_size = m_alpha
  decomp%base_index = k0_alpha
end subroutine decompose_manual

!> Compute local index from global index.
module function get_location(decomp, global_index, recompute) result(local_index)
  class(decomposition_type), intent(inout) :: decomp
  integer, intent(in) :: global_index(:)
  logical, intent(in), optional :: recompute
  integer, allocatable :: local_index(:)

  if (size(global_index) /= decomp%num_ranks) error stop &
    & "[get_location] Invalid global index."
  if (present(recompute)) then
    if (.not. recompute) then
      local_index = global_index - decomp%base_index
      return
    end if
  end if

  associate (k => global_index, &
    & r => decomp%remainder, &
    & alpha => decomp%co_index, &
    & m => decomp%local_size_max, &
    & m_alpha => decomp%local_size, &
    & k0 => decomp%base_index)

  alpha = compute_alpha(r, m, k)
  m_alpha = compute_m_alpha(r, alpha, m)
  k0 = compute_k0_alpha(r, alpha, m_alpha)
  local_index = k - k0
  end associate
end function get_location

!> Copy allocatable array
pure function copy_allocatable(arr) result(ret)
  integer, allocatable, intent(in) :: arr(:)
  integer, allocatable :: ret(:)

  if (.not. allocated(arr)) error stop &
    & "[copy_allocatable] Input array not allocated."
  ret = arr
end function copy_allocatable

!> Copy meta data
module subroutine copy_decomp(from, to)
  class(decomposition_type), intent(in) :: from
  class(decomposition_type), intent(out) :: to

  to%num_ranks = from%num_ranks
  to%global_size = copy_allocatable(from%global_size)
  to%num_procs = copy_allocatable(from%num_procs)
  to%local_size_max = copy_allocatable(from%local_size_max)
  to%local_size = copy_allocatable(from%local_size)
  to%co_index = copy_allocatable(from%co_index)
  to%remainder = copy_allocatable(from%remainder)
  to%base_index = copy_allocatable(from%base_index)
end subroutine copy_decomp

end submodule submodule_manual

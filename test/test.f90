program main

  use, non_intrinsic :: module_decomposition
  implicit none

  type(decomposition_type(num_ranks=:)), allocatable :: decomp
  integer, dimension(2) :: num_tasks, num_procs

  if (num_images() < 2) error stop "At least 2 images required."
  num_tasks = [48, 32]
  num_procs = [2, num_images()/2]
  decomp = decompose(num_tasks, num_procs)

  critical
    print *, decomp
  end critical
  sync all

end program main

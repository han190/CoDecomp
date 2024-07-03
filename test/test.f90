program main

  use, non_intrinsic :: module_decomposition
  implicit none

  type(decomposition_type) :: decomp
  integer, dimension(2) :: num_tasks, num_procs

  if (num_images() < 2) error stop "At least 2 images required."
  num_tasks = [48, 32]
  num_procs = [2, num_images()/2]
  call decompose(decomp, num_tasks, num_procs)
  if (this_image() == 1) print *, decomp

  sync all
  call decompose(decomp, product(num_tasks), num_ranks=2)
  if (this_image() == 1) print *, decomp

end program main

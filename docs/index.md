# CoDecomp
CoDecomp is a coarray domain decomposition toolkit.

## Compile and test with FPM
Compile and test by OpenCoarrays (built with OpenMPI), for example, with 8 images,
```bash
fpm test --compiler caf --runner "cafrun -n 8 --use-hwthread-cpus"
```

## Quick Start
```fortran
program main
  use, non_intrinsic :: module_decomposition
  implicit none

  type(decomposition_type) :: decomp
  integer, dimension(2) :: num_tasks, num_procs

  num_tasks = [48, 32]
  num_procs = [2, num_images()/2]
  call decompose(decomp, num_tasks, num_procs)
  if (this_image() == 1) print *, decomp
end program main
```

## Miscellaneous
The icon of this library is inspired by Piet Mondrian's [Composition II in Red, Blue, and Yellow](https://en.wikipedia.org/wiki/Piet_Mondrian#/media/File:Piet_Mondriaan,_1930_-_Mondrian_Composition_II_in_Red,_Blue,_and_Yellow.jpg).

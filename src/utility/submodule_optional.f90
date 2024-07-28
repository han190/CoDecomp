submodule(module_utility) submodule_optional
implicit none
contains

!> Fill optional argument (int32) with default value if not present.
pure module function optional_arg_int32(opt_arg, default_val) result(ret)
  integer, intent(in), optional :: opt_arg
  integer, intent(in) :: default_val
  integer :: ret

  if (present(opt_arg)) then
    ret = opt_arg
  else
    ret = default_val
  end if
end function optional_arg_int32

!> Fill optional argument (char) with default value if not present.
pure module function optional_arg_char(opt_arg, default_val) result(ret)
  character(len=*), intent(in), optional :: opt_arg
  character(len=*), intent(in) :: default_val
  character(len=:), allocatable :: ret

  if (present(opt_arg)) then
    ret = opt_arg
  else
    ret = default_val
  end if
end function optional_arg_char

end submodule submodule_optional

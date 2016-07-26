program simple
  implicit none

  integer :: x
  real    :: t
  real :: v, s, y, fooz
  logical :: check
  real :: foon

  real :: d
  real  :: c

   y = x ** 1 !x * x

  x = 20.0  ! initial
  t = 3.0   !  values
  v = x / t
  s = abs(v)
  d = (y / t) / c

  check = s == v
  if (check) then 
     print *, "Velocity is positive"
  endif 
  
  print *, s, square( v )
  call squareS(c, d)
  print *, c, d

contains

  real function square(x)
    real :: x
    square = x * x
  end function square

  subroutine squareS(x, y)
    real :: x, y
    intent(in) :: x
    intent(out) :: y
    y = x * x
  end subroutine squareS
  
end program

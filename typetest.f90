module typetest
implicit none

   type coord
      real :: x, y, z
      integer :: i
   end type coord

contains

   subroutine fill(p)   
      type(coord), intent(inout) :: p
      p%x = 3.5
      p%y = 1.7
      p%z = 2.3
      p%i = 42
   end subroutine fill

end module typetest

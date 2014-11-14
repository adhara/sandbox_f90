subroutine f90sub(a, b)
implicit none

   integer, intent(in)                    :: a
   real, intent(out)                      :: b

   integer :: i = 1
   b = 0

   do i = 1, a
      b = b + 0.1 * i
   end do

end subroutine f90sub

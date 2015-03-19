
module intertest
implicit none
contains

   subroutine f90sub(a, b)

      integer, intent(in) :: a
      real, intent(out)   :: b

      integer :: i = 1
      b = 0

      do i = 1, a
         b = b + i**2
      end do

   end subroutine f90sub

end module intertest


! After compiled, functions inside modules get exported with names according to extravagant compiler specific rules such __intertest_MOD_f90modsub (gfortran) 
! Therefore, it is good practice to generate a shell routine outside modules, so that exported symbols used in cpp code are not dependent on the f90 compiler used. They will, nevertheless, be preceded by _ in all compilers.
subroutine intertest_f90sub(a, b)

   use intertest

   integer, intent(in) :: a
   real, intent(out)   :: b

   call f90sub(a, b)

end subroutine intertest_f90sub


subroutine intertest_f90matrix(m, d1, d2)
implicit none

   integer, intent(in)                      :: d1, d2
   integer, dimension(d1, d2), intent(out)  :: m

   integer :: i1, i2

   do i2 = 1, d2
      do i1 = 1, d1
         m(i1, i2) = (d2 * (i1 -1)) + d1
      end do
   end do

end subroutine intertest_f90matrix


subroutine intertest_f90matrixflat(m, m_size)
implicit none

   integer, intent(in)                         :: m_size
   integer, dimension(m_size), intent(inout)   :: m

   write (0,*), "flat form: ", m

end subroutine intertest_f90matrixflat

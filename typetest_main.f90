program typetestmain
   use typetest
   implicit none

   type(coord) :: pt
   call fill(pt)
   print *, pt%x, pt%y, pt%z, pt%i

end program typetestmain

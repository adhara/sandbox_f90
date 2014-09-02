module test_module
   implicit none
contains 
   function test()
      integer :: test
      test = 42
   end function test
end module test_module

program show
   use test_module
   implicit none
   print *, "The number is", test()
end program show

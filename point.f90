!receives the polar coordinates of a point

function rad(x)
   implicit none
   real, parameter :: pi = 3.1415926
   real, intent(in) :: x
   real :: rad

   rad = x * pi/180.0
   print *, "rad = ", rad
end function rad


program point_coord
   implicit none

   integer :: r, theta
   real    :: x, y, rad

   print *, "Please, input r and theta (with space separator)"
   read *, r, theta 
   y = r * sin(rad(real(theta)))
   x = r * cos(rad(real(theta)))

   print *, x, y

end program

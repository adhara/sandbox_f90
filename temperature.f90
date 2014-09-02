program temperature_conv
implicit none
   integer :: k, c, f
   print*, "Please enter temperature in F"
   read*, f
   c = 5*(f-32)/9
   k = c + 273
   print *, "This is equal to ", c, "C or ", k, "K" 
end program temperature_conv

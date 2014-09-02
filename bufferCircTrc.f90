module trcbuffer
implicit none

type TrcNode   
   integer                        :: inLine
   real, dimension(:), pointer    :: amostras
   integer, dimension(:), pointer :: header
   type(TrcNode), pointer         :: next
end type TrcNode

type TrcBufferCirc
   type(TrcNode), dimension(:), pointer :: tracos
   type(TrcNode), pointer               :: head
   type(TrcNode), pointer               :: curr
   type(TrcNode), pointer               :: tmp
end type TrcBufferCirc

contains 

   function new_buffer(capacidade, amstPorTrc, headerSize)
      integer, intent(in)            :: capacidade, amstPorTrc, headerSize
      integer                        :: i = 1 
      type(TrcBufferCirc)            :: new_buffer
      real, dimension(:), pointer    :: amostras
      integer, dimension(:), pointer  :: header

      allocate(new_buffer%tracos(capacidade))
      do while (i <= capacidade)
         new_buffer%tracos(i)%inLine = i
!         nullify(amostras)
!         nullify(header)
!         allocate(amostras(amstPorTrc))
!         allocate(header(headerSize))
         allocate(new_buffer%tracos(i)%amostras(amstPorTrc))
         allocate(new_buffer%tracos(i)%header(headerSize))
         i = i + 1
      end do  
       
!      new_buffer%head => new_buffer%tracos(1)
!      new_buffer%curr => new_buffer%tracos(1)
!      new_buffer%tmp => new_buffer%tracos(1)
   end function new_buffer

end module trcbuffer

program test
use trcbuffer

type(TrcBufferCirc)  :: buf
integer              :: i = 1

buf = new_buffer(5, 5, 3)

do while (i <= 5)
   buf%tracos(i)%amostras = (/ (i**2, i=1,5) /)
   buf%tracos(i)%header = (/ (i**3, i=1,3) /)
   i = i + 1
end do

i = 1
do while (i <= 5)
   print *, "inline", buf%tracos(i)%inLine
   print *, "amostras",  buf%tracos(i)%amostras
   print *, "header", buf%tracos(i)%header
   print *, "---------------------------------------------------"
   deallocate(buf%tracos(i)%header)
   deallocate(buf%tracos(i)%amostras)
   i = i + 1
end do

deallocate(buf%tracos)

end program test


module intertest
implicit none
contains

   subroutine f90sub(a, b)

      integer, intent(in)                    :: a
      real, intent(out)                      :: b

      integer :: i = 1
      b = 0

      do i = 1, a
         b = b + 0.5 * i
      end do

   end subroutine f90sub

end module intertest


! Compiladores geram assinaturas extravagantes para rotinas dentro de modulos (por exemplo, no gfortran, a rotina acima seria exportada como
! __intertest_MOD_f90modsub
! Por causa disso, eh boa pratica gerar uma casca de funcao fora de modulos, para evitar que o codigo fique dependente de compiler
subroutine intertest_f90sub(a, b)

   use intertest

   integer, intent(in) :: a
   real, intent(out)   :: b

   call f90sub(a, b)

end subroutine intertest_f90sub

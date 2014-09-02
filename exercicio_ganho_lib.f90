! Módulo contendo a função comum para o exercício de ganho
! Todas as amostras de entrada recebem a aplicação de um fator de ganho determinado por 
! t ** a, se t > T
! T ** a, se t < T
! recebe como argumentos o fator de ganho a aplicar (a), 
!                        o tempo inicial (T) em segundos, 
!                        o número de amostras por traço,
!                        o intervalo de amostragem em microssegundos e
!                        os vetores (floats) de amostras de entrada e saída

module ex_ganho_lib
   implicit none
contains

   subroutine ganho(fator, tempo_inicial, numAmst, intAmst, ent, sai)
   
      integer :: i
      real :: t
      real, intent(in) :: fator, tempo_inicial
      integer, intent(in) :: numAmst, intAmst
      real, intent(in) , dimension(numAmst) :: ent
      real, intent(out), dimension(numAmst) :: sai
   
      do i = 1, numAmst
         t = ((i-1) * intAmst) / 1000000.0
         if (i == 50) print *, "t = ", t
         if (t < tempo_inicial) then
            sai(i) = ent(i) * (tempo_inicial**fator)
         else
            sai(i) = ent(i) * (t**fator)
         end if
      end do

   end subroutine ganho

   subroutine testando()
      print *, "testando, testando, 1, 2, 3! ------------------------------------"
   end subroutine testando

end module ex_ganho_lib

! Teste para o módulo
!program show
!   use ex_ganho_lib
!   implicit none
   
!   real :: fator, tempo_inicial
!   integer intAmst, numAmst, a
!   real, dimension(2) :: ent, sai
      
!   fator = 2.0
!   tempo_inicial = 0.0002
!   intAmst = 2000
!   numAmst = 2
!   ent(1) = 1.0
!   ent(2) = 2.0
!   call testando()
!   call ganho(fator, tempo_inicial, numAmst, intAmst, ent, sai)
!   print *, sai(1), sai(2)
   
!end program show

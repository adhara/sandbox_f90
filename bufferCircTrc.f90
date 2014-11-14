module trcbuffer
implicit none

type TrcNode   
   integer                        :: inLine
   real, dimension(:), pointer    :: amostras, header
   type(TrcNode), pointer         :: next
end type TrcNode

type TrcBufferCirc
   integer                             :: capacidade
   integer                             :: estado
   integer                             :: inlineCorrente
   integer                             :: trcsLineCorrente
   integer                             :: ultimaInlineLida

   type(TrcNode), dimension(:), pointer:: tracos

   type(TrcNode), pointer              :: head
   type(TrcNode), pointer              :: curr
   type(TrcNode), pointer              :: calc
   type(TrcNode), pointer              :: grv
   type(TrcNode), pointer              :: novaInline

end type TrcBufferCirc

integer, parameter :: BUF_ENCHENDO  = 1
integer, parameter :: BUF_SEMICHEIO = 2
integer, parameter :: BUF_CHEIO     = 3
integer, parameter :: BUF_ESVAZIANDO= 4
integer, parameter :: BUF_VAZIO     = 5


contains 

   function new_buffer(capacidade, amstPorTrc, headerSize)
      integer, intent(in)            :: capacidade, amstPorTrc, headerSize
      integer                        :: i = 1 
      type(TrcBufferCirc)            :: new_buffer
      type(TrcNode), pointer         :: tmp

      nullify(new_buffer%head)
      nullify(new_buffer%curr)
      nullify(tmp)

      ! aloca o espaço do buffer
      allocate(new_buffer%tracos(capacidade))
      new_buffer%capacidade       = capacidade
      new_buffer%estado           = BUF_ENCHENDO
      new_buffer%ultimaInlineLida = -1
      new_buffer%inlineCorrente   = -1
      new_buffer%trcsLineCorrente = 0

      ! aloca o espaço de cada traço
      do while (i <= capacidade)
         allocate(new_buffer%tracos(i)%amostras(amstPorTrc))
         allocate(new_buffer%tracos(i)%header(headerSize))
      
         ! o próximo do anterior aponta para o atual, a não ser que esse seja o primeiro
         if (associated(new_buffer%curr)) then
            new_buffer%curr%next => new_buffer%tracos(i)
         else
            new_buffer%head => new_buffer%tracos(i)
         end if

         ! termina a lista circular apontando o próximo de volta para o primeiro
         if (i == capacidade) then
            new_buffer%tracos(i)%next => new_buffer%head
         end if
                  
         new_buffer%curr => new_buffer%tracos(i)
         i = i + 1
      end do
            
      new_buffer%head => new_buffer%tracos(1)
      new_buffer%curr => new_buffer%tracos(1)
      new_buffer%calc => new_buffer%tracos(1)
      new_buffer%grv => new_buffer%tracos(1)
      nullify(new_buffer%novaInline)

   end function new_buffer

   
   function percorre(buf, indice)

      integer                          :: indice, i
      type(TrcBufferCirc), intent(in)  :: buf
      type(TrcNode), pointer           :: percorre
      
      percorre => buf%head
      i = 1
      do while (i < indice)
         percorre => percorre%next
         i = i + 1
      end do
      
   end function


   subroutine free_buffer(buf)

      type(TrcBufferCirc) :: buf
      integer :: i
             
      i = 1      
      do while (i <= buf%capacidade )
         deallocate(buf%tracos(i)%amostras)
         deallocate(buf%tracos(i)%header)
         i = i + 1
      end do      
      deallocate(buf%tracos)

   end subroutine


   subroutine printBuffer (buf, inicioIntervalo, fimIntervalo)

      integer                         :: i, j
      type(trcBufferCirc), intent(in) :: buf
      integer, intent(in)             :: inicioIntervalo, fimIntervalo
      type(trcNode), pointer          :: this
   
      print *, "******************************BUFFER******************************"
      print *, "estado             ", buf%estado
      print *, "tracos para calculo", buf%trcsLineCorrente
      print *, "inline corrente    ", buf%inlineCorrente
      print *, "ultima inline lida ", buf%ultimaInlineLida

      i = 1
      do while (i <= buf%capacidade)
         print *, "----------------------------------------------" 
         this => percorre(buf, i)
         if (associated(buf%head, target=this)) then
            print *, "<h>"
         end if
         if (associated(buf%curr, target=this)) then
            print *, "<a>"
         end if
         if (associated(buf%calc, target=this)) then
            print *, "<c>"
         end if
         if (associated(buf%grv, target=this)) then
            print *, "<g>"
         end if

         print *, "Traco da inline ", this%inline, "na posicao ", i
         print *, this%amostras(inicioIntervalo:fimIntervalo)
         i = i + 1
      end do

   end subroutine printBuffer


   subroutine produz(buf, amostras, header, inLine)
  
      type(TrcBufferCirc), intent(inout):: buf
      real, dimension(:), intent(in)    :: amostras, header
      integer, intent(in)               :: inLine
  
      buf%curr%amostras(1:size(amostras))    = amostras
      buf%curr%header(1:size(header))      = header
      buf%curr%inLine      = inLine

      if (inLine /= buf%ultimaInlineLida) then
         buf%ultimaInlineLida = inLine
         if (buf%ultimaInlineLida /= -1) then
            buf%novaInline => buf%curr
         end if
      end if

      buf%curr => buf%curr%next

   end subroutine produz


   subroutine processaEstado(buf)
  
      type(TrcBufferCirc), intent(inout):: buf

      select case(buf%estado)

         case(BUF_CHEIO)
            if (buf%ultimaInlineLida /= buf%inlineCorrente) then
               buf%trcsLineCorrente = buf%trcsLineCorrente - 1
               buf%estado = BUF_ESVAZIANDO
            end if

         case(BUF_ENCHENDO)
            buf%trcsLineCorrente =  buf%trcsLineCorrente + 1
            if (buf%trcsLineCorrente >= real(buf%capacidade) / 2) then
               buf%estado = BUF_SEMICHEIO
            end if
            if (buf%inlineCorrente < 0) then
               buf%inlineCorrente = buf%curr%inLine
            end if

         case(BUF_SEMICHEIO)
            buf%trcsLineCorrente = buf%trcsLineCorrente + 1
            if (buf%trcsLineCorrente == buf%capacidade) then
               buf%estado = BUF_CHEIO
            end if

         case(BUF_ESVAZIANDO)
            buf%trcsLineCorrente = buf%trcsLineCorrente - 1
            if (buf%trcsLineCorrente < real(buf%capacidade) / 2) then
               if (associated(buf%novaInline)) then
                  buf%calc => buf%novaInline
                  buf%grv => buf%novaInline
                  buf%inlineCorrente = buf%novaInline%inLine
                  buf%trcsLineCorrente = buf%capacidade - buf%trcsLineCorrente
                  buf%estado = BUF_SEMICHEIO
                  nullify(buf%novaInline)
               else
                  buf%estado = BUF_VAZIO
               end if
            end if
      end select

   end subroutine processaEstado


   subroutine calculaVetorMediaTracos(buf, trcSai)
   
      type(TrcBufferCirc), intent(in)   :: buf
      real, dimension(:), intent(inout) :: trcSai
      integer                           :: i,j
      real                              :: soma
      type(TrcNode), pointer            :: trc
      
      j = 1
      do while (j <= size(buf%head%amostras))
         soma = 0.0
         trc => buf%calc
         i = 1
         do while (i <= buf%trcsLineCorrente)
            soma = soma + trc%amostras(j)
            trc => trc%next
            i = i + 1
         end do
         trcSai(j) = soma / buf%trcsLineCorrente
         j = j + 1
      end do

   end subroutine calculaVetorMediaTracos


   subroutine consome(buf, amstSai, headerSai)

      type(TrcBufferCirc), intent(inout) :: buf
      real, intent(inout), dimension(:)  :: amstSai, headerSai
 
      call calculaVetorMediaTracos(buf, amstSai)
      headerSai = buf%grv%header

      buf%grv => buf%grv%next
      if (buf%estado /= BUF_SEMICHEIO) then
         buf%calc => buf%calc%next
      end if

   end subroutine consome


end module trcBuffer


program test
use trcbuffer

type(TrcBufferCirc)           :: buf
type(TrcNode), pointer        :: teste
real, dimension(1:12,1:5)     :: trcEnt
real, dimension(1:5)          :: trcSai
real, dimension(1:12,1:3)     :: headerEnt
real, dimension(1:3)          :: headerSai
integer                       :: j, i = 1

buf = new_buffer(5, 5, 3)

! preenche os traços
do while (i <= 12)
   trcEnt(i,:) = (/ (i * real(j), j=1,5) /)
   headerEnt(i,:) = (/ (i**2 * real(j), j=1,3) /)
   i = i + 1
end do

i = 1
do while (buf%estado /= BUF_VAZIO)

   if (i <= 12) then
      call produz(buf, trcEnt(i,:), headerEnt(i,:), (i-1)/6)
   else
      buf%ultimaInlineLida = -1
   end if
   
   call processaEstado(buf)
   call printBuffer(buf, 1, 5)
   print *, "" 
  
   if ((buf%estado /= BUF_ENCHENDO) .and. (buf%estado /= BUF_VAZIO)) then
      call consome(buf, trcSai, headerSai)
      print *, "media", trcSai
      print *, ""
   end if

   i = i + 1
end do

call free_buffer(buf)

!end program test

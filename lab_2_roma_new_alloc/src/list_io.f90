
module List_IO
   use Environment

   implicit none

   type symbol
      character(kind=CH_) :: char = ""
      class(symbol),allocatable :: next
   end type symbol


   type command
      class(command), allocatable :: next
   end type command

   type, extends(command) :: delete

      integer(I_) :: start = 0
      integer(I_) :: last = 0
   end type delete

   type, extends(command) :: input
      
      integer(I_) :: start = 0
      class(symbol), allocatable :: symbolism
   end type input

contains



end module List_IO



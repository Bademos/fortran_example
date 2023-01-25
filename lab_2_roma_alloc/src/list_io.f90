
module List_IO
   use Environment

   implicit none

   type symbol
      character(kind=CH_) :: char = ""
      class(symbol), allocatable :: next
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
      class(symbol),allocatable :: symbolism 
   end type input

contains
   function Read_sym_list(Input_File,List_comm) result(List)
      class(symbol), allocatable        :: List
      class(command), allocatable ,intent(inout)      :: List_comm
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, newunit=In)
        call  Read_value(In,list)
        call Read_command(In,List_comm)
      close (In)
   end function Read_sym_list


   recursive subroutine Read_value(In,Elem)
      class(symbol), allocatable  :: Elem
      integer, intent(in)     :: In
      integer  IO

      character(kind=CH_)  :: char = ""
      integer              :: value = 0
      allocate(Elem)

      read (In, '(a1)', iostat=IO, advance='no') char
      if (IO == 0) then
               Elem%char=char
               call Read_value(In,Elem%next)
      else
         deallocate(Elem)
      end if
   end subroutine Read_value

recursive subroutine Read_sym_value(hui,wrd) 
        class(symbol), allocatable  :: wrd
        character(*), intent(in)     :: hui
        integer  IO 
        
        character(kind=CH_)  :: char = " "
         !allocate(wrd)


        char = hui(1:1)

        if (len(hui)>0) then
              allocate(wrd,  source=symbol(char=char))
                 call Read_sym_value(hui(2:),wrd%next)
        else
           !deallocate(wrd)
        end if
     end subroutine Read_sym_value


   recursive subroutine Read_command(In,Comm)
      class(command), allocatable :: Comm
      integer, intent(in)     :: In
      character(kind=CH_)     :: typ
      class(symbol), allocatable  :: symb
      integer,parameter                 :: max_len=100
      character(max_len)   :: third,hui
      character(:),allocatable  :: the

     integer(I_)             :: start,endl
      integer IO
       character(:), allocatable  :: format
 
       
       allocate(Comm)
        format = '(a1,i4,a)'
        read (In, format, iostat=IO) typ,start,third
         
        if (typ=="I") then
           ! read(third,'(a4)',iostat=IO) hui
            the = trim(THIRD)
           ! call Read_sym_value(the,symb)
            
         else if (typ=="D") then
            read(third,'(i2)',iostat=IO) endl
         else
        end if

        call Handle_IO_status(IO, "reading line from file")
        if (IO == 0) then
           if (typ=="I") then
               allocate(Comm,source=input(start=start,symbolism=symb))
            else if(typ=="D") then
              allocate(Comm, source=delete(start=start, last=endl))

            end if
            call Read_command(In,Comm%next)
        else
           deallocate(Comm) ! не нужно
        end if

   end subroutine Read_command
   
   


   subroutine Output_command(Output_File,Comm,Command_Name,Position)
      character(*), intent(in) :: Output_File,Position, Command_Name
      class(command),allocatable   :: Comm
      integer                  :: Out
         
     open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') Command_Name
          call Output_com_value(Out, Comm)
       close (Out)
    end subroutine Output_command
 
recursive subroutine Output_com_value(Out, Com)
       integer, intent(in)     :: Out
       class(command), allocatable     :: Com
 
       integer  :: IO
 
       if (allocated(Com)) then
          select type(Com)
            type is (input)
               write(Out, '(/a, 1x,i2)') "I",Com%start
               call output_value(Out,Com%symbolism)
               
              write(Out, '(a)') " "
            type is (delete)

               write(Out, '(/a,1x,i2,1x,i2)') "D", Com%start,Com%last
          end select
          call Handle_IO_status(IO, "writing list")
          call Output_com_value(Out, Com%next)
       end if
    end subroutine Output_com_value
   

    subroutine Output_list(Output_File, List, List_Name, Position)
      character(*), intent(in)   :: Output_File, Position, List_Name
      class(symbol), allocatable        :: List
      integer  :: Out
      
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_value(Out, Elem)
      integer, intent(in)     :: Out
      class(symbol), allocatable     :: Elem
      
      integer  :: IO

      if (allocated(Elem)) then 
               write (Out, '(a1)', advance='no', iostat=IO) Elem%char 
         call Handle_IO_status(IO, "writing list")
         call Output_value(Out, Elem%next)
      end if
   end subroutine Output_value


! Модуль процессов  пришлось объединить с модулем ввода/вывода так как
!вылезала ошибка 'explicite interface required polymorphic argument'

! recursive subroutine Process(current, pos, from, num)
!    type(symbol), pointer       :: current
!    integer,intent(in)              :: pos, from, num
!    integer                         :: to
!    type(symbol) ,pointer       :: tmp
!
!
!    to = from + num
!
!    if(pos >= from.and.pos <= to) then
!       if (from == 0) then
!          tmp => current
!          current => current%next
!          tmp%next=>null()
!            
!       else
!     
!       tmp => current%next
!       current%next =>  current%next%next
!       end if
!       deallocate(tmp)
!    end if
!
!
!    if (associated(current%next).and.pos<from) then
!       Call Process(current%next, pos+1, from, num)
!    else if (associated(current%next).and.pos<to) then
!       Call Process(current, pos+1, from, num)
!    end if
! end subroutine Process
!
!recursive subroutine Conjuct(main_list, tail)
!      class(symbol) , pointer::main_list, tail,tmp
!         if (associated(main_list%next)) then
!            call Conjuct(main_list%next, tail)
!         else
!            main_list%next => tail
!         end if
!end subroutine Conjuct
!
!!recursive function Conjunction(main_list, tail) result(res_list)
! !  class(symbol), pointer:: main_list, tail, tmp, res_list
!
!
!recursive subroutine Insert(List,pos,pos_ins,sub_list)
!      class(symbol), pointer :: List, sub_list, tmp
!      integer, intent(in)    :: pos, pos_ins
!
!      print*, pos_ins
!      if ( pos_ins==0) then
!              tmp => list
!              call conjuct(sub_list,tmp)
!              list => sub_list
!              print *, "help!!!!!!!"
!
!      else
!       if (associated(List%next).and.pos<pos_ins) then
!       
!          if (pos_ins==0) then
!             tmp => list
!             call conjuct(sub_list,tmp)
!             list => sub_list
!             print *, "help!!!!!!!"
!          else
!             print *, "mee!!!!",pos_ins
!          Call Insert(List%next, pos+1, pos_ins, sub_list)
!          end if
!    
!       else if (pos==pos_ins) then
!         if (associated(List%next)) then
!             tmp => List%next
!             List%next => Null()
!             call conjuct(list,sub_list)
!             call conjuct(list, tmp)
!         else
!            call conjuct(list, sub_list)
!         end if
!       
!       end if
!    end if
!end subroutine Insert
!
!recursive   subroutine Exibition (Out,List, List_com)
!     class(symbol) , pointer    :: List,sym
!     class(command), pointer    :: List_com
!     integer                    :: start
!     integer, intent(in)        :: out
!     call output_value(Out, List)
!
!       if (Associated(List_com)) then
!           select type(List_com)
!             type is (input)
!                write(Out, '(/a, 1x,i2)') "I",List_com%start
!                call output_value(Out,List_com%symbolism)
!
!               write(Out, '(a)') " "
!
!               call Insert(List,1,List_com%start-1,List_com%symbolism)
!             type is (delete)
!                write(Out, '(/a,1x,i2,1x,i2)') "D", List_com%start,List_com%last
!
!                call Process(List,1,List_com%start-1,List_com%last) 
!           end select
!           call Exibition(out,List, List_com%next)
!        end if
!
!
!
!  end subroutine Exibition
!
!  subroutine Output_ex(Output_File,List,List_com,Command_Name,Position)
!       character(*), intent(in) :: Output_File,Position, Command_Name
!       class(command),pointer   :: List_com
!       class(symbol), pointer   :: List
!       integer                  :: Out
!
!      open (file=Output_File, position=Position, newunit=Out)
!          write (out, '(/a)') Command_Name
!           call Exibition(Out, List, List_com)
!        close (Out)
!     end subroutine Output_ex



end module List_IO



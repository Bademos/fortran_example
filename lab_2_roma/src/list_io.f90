
module List_IO
   use Environment

   implicit none

   type symbol
      character(kind=CH_) :: char = ""
      class(symbol), pointer :: next => Null()
   end type symbol


   type command
      class(command), pointer :: next  => Null()
   end type command

   type, extends(command) :: delete

      integer(I_) :: start = 0
      integer(I_) :: last = 0
   end type delete

   type, extends(command) :: input
      
      integer(I_) :: start = 0
      class(symbol),pointer :: symbolism =>Null()
   end type input

contains
   function Read_sym_list(Input_File,List_comm) result(List)
      class(symbol), pointer        :: List
      class(command), pointer ,intent(inout)      :: List_comm
      character(*), intent(in)   :: Input_File
      integer  In

      open (file=Input_File, newunit=In)
        List => Read_value(In)
        List_comm => Read_command(In)
      close (In)
   end function Read_sym_list


   recursive function Read_value(In) result(Elem)
      class(symbol), pointer  :: Elem
      integer, intent(in)     :: In
      integer  IO

      character(kind=CH_)  :: char = ""
      integer              :: value = 0
      read (In, '(a1)', iostat=IO, advance='no') char
      if (IO == 0) then
               allocate (Elem, source=symbol(char=char))
         Elem%next => Read_value(In)
      else
         nullify (Elem)
      end if
   end function Read_value

recursive function Read_sym_value(hui) result(wrd)
        class(symbol), pointer  :: wrd
        character(*), intent(in)     :: hui
        integer  IO
  
        character(kind=CH_)  :: char = ""
        char = hui(1:1)
        if (len(hui)>0) then
                 allocate (wrd, source=symbol(char=char))
                 wrd%next => Read_sym_value(hui(2:))
        else
           nullify (wrd)
        end if
     end function Read_sym_value


   recursive function Read_command(In) result(Comm)
      class(command), pointer :: Comm
      integer, intent(in)     :: In
      character(kind=CH_)     :: typ
      class(symbol), pointer  :: symb
      integer,parameter                 :: max_len=100
      character(max_len)   :: third,hui
      character(:),allocatable  :: the

     integer(I_)             :: start,endl
      integer IO
       character(:), allocatable  :: format
 
       
        format = '(a1,i4,a)'
        read (In, format, iostat=IO) typ,start,third
         
        if (typ=="I") then
           ! read(third,'(a4)',iostat=IO) hui
            the = trim(THIRD)
            symb => Read_sym_value(the)
            
         else if (typ=="D") then
            read(third,'(i2)',iostat=IO) endl
         else
        end if

        call Handle_IO_status(IO, "reading line from file")
        if (IO == 0) then
           if (typ=="I") then
               allocate(Comm,source=input(start=start,symbolism=symb))
           else
              allocate(Comm, source=delete(start=start, last=endl))

            end if
            Comm%next => Read_command(In)
        else
           nullify (Comm) ! не нужно
        end if

   end function Read_command
   
   


   subroutine Output_command(Output_File,Comm,Command_Name,Position)
      character(*), intent(in) :: Output_File,Position, Command_Name
      class(command),pointer   :: Comm
      integer                  :: Out
         
     open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') Command_Name
          call Output_com_value(Out, Comm)
       close (Out)
    end subroutine Output_command
 
recursive subroutine Output_com_value(Out, Com)
       integer, intent(in)     :: Out
       class(command), pointer     :: Com
 
       integer  :: IO
 
       if (Associated(Com)) then
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
      class(symbol), pointer        :: List
      integer  :: Out
      
      open (file=Output_File, position=Position, newunit=Out)
         write (out, '(/a)') List_Name
         call Output_value(Out, List)
      close (Out)
   end subroutine Output_list

   recursive subroutine Output_value(Out, Elem)
      integer, intent(in)     :: Out
      class(symbol), pointer     :: Elem
      
      integer  :: IO

      if (Associated(Elem)) then 
               write (Out, '(a1)', advance='no', iostat=IO) Elem%char 
         call Handle_IO_status(IO, "writing list")
         call Output_value(Out, Elem%next)
      end if
   end subroutine Output_value


! Модуль процессов  пришлось объединить с модулем ввода/вывода так как
!вылезала ошибка 'explicite interface required polymorphic argument'

 recursive subroutine Process(current, pos, from, num)
    type(symbol), pointer       :: current
    integer,intent(in)              :: pos, from, num
    integer                         :: to
    type(symbol) ,pointer       :: tmp


    to = from + num

    if(pos >= from.and.pos <= to) then
       if (from == 0) then
          tmp => current
          current => current%next
          tmp%next=>null()
            
       else
     
       tmp => current%next
       current%next =>  current%next%next
       end if
       deallocate(tmp)
    end if


    if (associated(current%next).and.pos<from) then
       Call Process(current%next, pos+1, from, num)
    else if (associated(current%next).and.pos<to) then
       Call Process(current, pos+1, from, num)
    end if
 end subroutine Process

recursive subroutine Conjuct(main_list, tail)
      class(symbol) , pointer::main_list, tail,tmp
         if (associated(main_list%next)) then
            call Conjuct(main_list%next, tail)
         else
            main_list%next => tail
         end if
end subroutine Conjuct

!recursive function Conjunction(main_list, tail) result(res_list)
 !  class(symbol), pointer:: main_list, tail, tmp, res_list


recursive subroutine Insert(List,pos,pos_ins,sub_list)
      class(symbol), pointer :: List, sub_list, tmp
      integer, intent(in)    :: pos, pos_ins

      print*, pos_ins
      if ( pos_ins==0) then
              tmp => list
              call conjuct(sub_list,tmp)
              list => sub_list
              print *, "help!!!!!!!"

      else
       if (associated(List%next).and.pos<pos_ins) then
       
          if (pos_ins==0) then
             tmp => list
             call conjuct(sub_list,tmp)
             list => sub_list
             print *, "help!!!!!!!"
          else
             print *, "mee!!!!",pos_ins
          Call Insert(List%next, pos+1, pos_ins, sub_list)
          end if
    
       else if (pos==pos_ins) then
         if (associated(List%next)) then
             tmp => List%next
             List%next => Null()
             call conjuct(list,sub_list)
             call conjuct(list, tmp)
         else
            call conjuct(list, sub_list)
         end if
       
       end if
    end if
end subroutine Insert

recursive   subroutine Exibition (Out,List, List_com)
     class(symbol) , pointer    :: List,sym
     class(command), pointer    :: List_com
     integer                    :: start
     integer, intent(in)        :: out
     call output_value(Out, List)

       if (Associated(List_com)) then
           select type(List_com)
             type is (input)
                write(Out, '(/a, 1x,i2)') "I",List_com%start
                call output_value(Out,List_com%symbolism)

               write(Out, '(a)') " "

               call Insert(List,1,List_com%start-1,List_com%symbolism)
             type is (delete)
                write(Out, '(/a,1x,i2,1x,i2)') "D", List_com%start,List_com%last

                call Process(List,1,List_com%start-1,List_com%last) 
           end select
           call Exibition(out,List, List_com%next)
        end if



  end subroutine Exibition

  subroutine Output_ex(Output_File,List,List_com,Command_Name,Position)
       character(*), intent(in) :: Output_File,Position, Command_Name
       class(command),pointer   :: List_com
       class(symbol), pointer   :: List
       integer                  :: Out

      open (file=Output_File, position=Position, newunit=Out)
          write (out, '(/a)') Command_Name
           call Exibition(Out, List, List_com)
        close (Out)
     end subroutine Output_ex



end module List_IO



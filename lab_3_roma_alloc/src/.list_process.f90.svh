b0VIM 8.2      ��cVl   vadique                                 debian                                  ~vadique/homework/lab_3_roma_alloc/src/list_process.f90                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      utf-8U3210    #"! U                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 tp           ~                            D       �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ad  �   �     ~       �  �  �  �  �  �  �  �  �  �  d  <    �  �  �    {  F  9    �  �  �  �  �  �  b  U  +    �  �  �  �  `  >        �  �  �  �  ;    �
  �
  �
  �
  �
  W
  V
  2
  
  �	  �	  �	  }	  |	  f	  S	  H	  %	  $	  #	  "	  !	   	  �  �  �  �  l  7       �  �  �  �  �  �  O  "  �  �  �  �  �  �  r  G  !      �  �  �  �  m  >  <      �  �  �  g  X  *      �  �  �  �  �  �  W  0  	  �  �  �  �  k  D  D                                                   if(Allocated(set2%next)) then          if(Allocated(set2%next)) then                   if(Allocated(set2%next)) then        else          end if             call alterSetDivide(set1,set2)          if(Allocated(set2%next)) then          call Delete(set2, set2%value)       if(isContainChar(set2%value,set1,.false.)) then        type(node), allocatable :: set1,set2, tmp    recursive subroutine alterSetDivide(set1,set2)       end subroutine Delete       end if           end if              call Delete(current%next, value)           else              call move_alloc(temp, current)              call move_alloc(current%next, temp)           if (current%value == value) then        if (Allocated(current)) then          type(node), allocatable :: temp          character(CH_), intent(in)     :: value        type(node), allocatable, intent(inout) :: current     pure recursive subroutine Delete(current, value)       end subroutine Get       end if          value = ""        else           call move_alloc(temp, Elem)           call move_alloc(Elem%next, temp)           !call move_alloc(Elem%next, Elem)           !Elem = Elem%next            value = Elem%value        if (allocated(Elem)) then          type(node), allocatable :: temp        character(Ch_), intent(out)  :: value        type(node), allocatable, intent(inout) :: Elem     pure subroutine Get(Elem, value)       end subroutine Put       end if          call Put(Elem%next, value)        else           Elem%value = value           allocate (Elem)           !allocate (Elem, source=node(value=value))        if (.not. Allocated(Elem)) then          character(CH_), intent(in)     :: value        type(node), allocatable, intent(inout) :: Elem recursive subroutine Put(Elem, value)                end subroutine setMaking                        end if                end if                  call setMaking(List%next, Res)                else                 call setMaking(List,Res)                 call  Conjuct(res,tmp)                 ! deallocate(tmp)                 !tmp%next => null()                  call move_alloc(tmp%next, list)                 !list => tmp%next                  call move_alloc(list,tmp)               ! allocate(tmp)                 !tmp => list                 .not.(List%value=='.'))then                 .not.(List%value==' ').and..not.(List%value==',').and.&                if(.not.isContainChar(List%value,res,.false.).and.&              if (Allocated(List%next)) then                               end if                 ! deallocate(res%next)                 !res%next=>Null()                 call move_alloc(res%next, List)                ! List =>  res%next                                  call move_alloc(List,res)                ! res => List                ! Allocate(res)               if(.not.allocated(res)) then                          type(node), allocatable :: List, Res,tmp           recursive subroutine setMaking(List, Res)           end function isContainChar              end if             Res = isContainChar(chr, list%next, res)                       If (.not.Res.and.allocated(list%next)) then              Res =  (chr == list%value)          Res = isChar                character(kind=CH_), intent(in) :: chr          type(node),intent(in)  :: list          logical              :: Res          logical , intent(in) :: isChar   pure recursive function isContainChar(chr,list,isChar) result(Res)   contains    implicit none     use List_IO       use Environment module List_Process  ad  |  �     D       �  �  �  �  �  �  �  �  �  �  _      �  �  n  [  >  ,  �  �  �  �  �  Q  ?    �  �  �  k  T  '    �  �  �  w  a  N  M  �
  �
  �
  �
  �
  �
  �
  V
  )
  (
  
  
  �	  �	  �	  y	  I	  H	  9	  .	  	  �  �  �  �  �  �  �  �  �  n  K    
  �  �  �  x  A  ?  =       �                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      end module Lisend module List_process   end subroutine Conjeeeeenend module List_process   end subroutine Conjuct       end module List_pend module List_process   end subroutine Conjuct           end if   end module List_process   end subroutineend module List_proceenend module List_process   end subroutine Conjuct           end end module List_process   end subroend module List_end module List_process   end subreeeeeeeeend module List_process   end subroutine Conjuct           end if          !call move_alloc(tail,main_list)          main_list = tail       else         end if            call move_alloc(tail, main_list%next)           ! main_list%next = tail         else             call Conjuct(main_list%next, tail)         if (allocated(main_list%next)) then        if(Allocated(main_list)) then      type(node), allocatable::main_list, tail  recursive subroutine Conjuct(main_list, tail)               end subroutine  setDivide                  end if                   call setDivide(set1,set2, set3)                if (Allocated(set2%next).and.Allocated(set1).and.allocated(set2)) then              end if                end if                   call move_alloc(tmp2,set2)                   call move_alloc(set2%next,tmp2)                   !set2=> set2%next                else                   end if                      call conjuct(set3, tmp)                   else                      !call move_alloc(tmp,set3)                      set3 = tmp                   if (.not.Allocated(set3)) then                  ! tmp%next => Null()                  call move_alloc(tmp1, set2)                                   !call move_alloc(set2%next, tmp1)                  tmp1 = set2%next                                    ! set2 => set2%next                   print *, "hello"                  ! call move_alloc(set2,tmp)                                     tmp = set2                                      if(.not.(isContainChar(set2%value,set1,.false.))) then                   print*, set2%value                if(Allocated (set2)) then                                type(node), allocatable :: set1, set2, set3,tmp, tmp1,tmp2 recursive subroutine setDivide(set1,set2,set3)         end subroutine alterSetDivide        end if          end if           call alterSetDivide(set1,set2%next) 
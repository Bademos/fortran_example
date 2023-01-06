
module List_Process
   use Environment
   use List_IO

   implicit none
contains


pure   recursive function isContainChar(chr,list,isChar) result(Res)
         logical , intent(in) :: isChar
         logical              :: Res
         type(symbol),intent(in)  :: list
         character(kind=CH_), intent(in) :: chr
         Res = isChar      
         Res =  (chr == list%char)
   
         If (.not.Res.and.associated(list%next)) then
            
            Res = isContainChar(chr, list%next, res)
         end if
  
   end function isContainChar
            
         
         

   
         recursive subroutine setMaking(List,Res) 
            type(symbol), pointer :: List, Res,tmp
            
             if(.not.associated(res)) then
                res => List
                List =>  res%next
                res%next=>Null()
             end if
               

             if (associated(List%next)) then
             if(.not.isContainChar(List%char,res,.false.).and.&
                .not.(List%char==' ').and..not.(List%char==',').and.&
                .not.(List%char=='.'))then
                tmp => list
                list => tmp%next
                tmp%next => null()

                call  Conjuct(res,tmp)
                call setMaking(List,Res)
             else
                call setMaking(List%next, Res)

             end if
            end if


               
            end subroutine setMaking            

              



            recursive subroutine setDivide(set1,set2,set3)
               type(symbol), pointer :: set1, set2, set3,tmp
               
               if(associated (set2)) then
                  print*, set2%char
               if(.not.(isContainChar(set2%char,set1,.false.))) then
                  tmp => set2
                  set2 => set2%next
                  tmp%next => Null()
                  if (.not.associated(set3)) then
                     set3 => tmp
                  else
                     call conjuct(set3, tmp)
                  end if
               else
                  set2=> set2%next
               end if
            end if

               if (associated(set2%next)) then
                  call setDivide(set1,set2, set3)
               end if


            end subroutine  setDivide
            
            subroutine Control(set1,set2,set3)

            type(symbol), pointer :: set1, set2, set3
               
             if (.not.isContainChar(set2%char, set1,.false.)) then
               call Conjuct(set3,set2)
             end if

             if (isContainChar(set3%char, set1,.false.)) then
                set3 => set3%next
             end if
            end subroutine Control



 recursive subroutine Conjuct(main_list, tail)
    type(symbol) , pointer::main_list, tail,tmp
        if (associated(main_list%next)) then
           call Conjuct(main_list%next, tail)
        else 
           main_list%next => tail

        end if
 
 end subroutine Conjuct

end module List_process


module List_Process
   use Environment
   use List_IO

   implicit none

contains


  
     recursive function Checking(Elem, is_closed, pos, num_bracket,str) result(res)
        class(node), pointer ::           Elem,nxt
        logical , intent(inout) ::       is_closed
        integer,  intent(inout) ::       pos, num_bracket
        character(*)             ::      str
        character(:), allocatable ::           res
  
        res = str
  
  
         if (Associated(Elem%next)) then
            nxt => Elem%next

            select type (Elem)
              type is (variable)
                   
               select type(nxt)

                   type is (variable)
                      res =  "Warning! forbidded character at position.Mostlikely mi    ssing operation at "//pos
                    ! res = pos
                   type is (operand)
 
                      res =  "Warning! forbidded character at position.Mostlikely mi    ssing operation in "//pos
 
                   type is (left_bracket)
                      res =  " Operation is missed in "//pos
                      !res = pos
                end select

              type is (operation)
                select type (nxt)
                   type is (operation)
                      if (nxt%char/='-'.and.nxt%char/='+')&
                        res =  "Extra operation in "//pos
                   type is (right_bracket)
                      res =  " Extra right bracket in "//pos
                end select

              type is (operand)
                select type (nxt)
                     type is (variable)
                        res = "Warning! forbidded character at position.Mostlikely         missing operation in "//pos

                     type is (operand)

                        res = "Warning! forbidded character at position.Mostlikely         missing operation in "//pos

                     type is (left_bracket)
                        res = " Operation is missed in "//pos
                end select

             type is (left_bracket)
               is_closed = .false.
               num_bracket = num_bracket + 1
                 select type (nxt)
                    type is (operation)
                       if (nxt%char/='-')&
                            res =  "Extra operation in"// pos
                    type is (right_bracket)
                         res =  " Empty expression in "//pos
                 end select
  
               type is (right_bracket)
                 num_bracket = num_bracket - 1
                 if (num_bracket == 0) then
                    is_closed = .true.
                 else if (num_bracket < 0) then
                   res =  " Warning! EXTRA RIGHT BRACKET!!!"
                 end if
 !############################################################################ 
               if (res=="norm") then 
                 select type (nxt)
                  
                     type is (variable)
                        res =  "Warning! forbidded character at position.Mostlikely         missing operation in "//pos
 
                     type is (operand)
 
                        res = "Warning! forbidded character at position.Mostlikely         missing operation in "// pos
 
                     type is (left_bracket)
                        res =  " Operation is missed in " // pos
 
                  end select
               end if


  
  
              type is (cringe)
                 if (Elem%char==" ") then
                    res =  " Extra space!"//pos
  
                 else
                   res= " Unknown character!"
  
                 end if
  
           end select
        
           pos = pos + 1
          
           

              
           if (res=="norm") then
                  res =  Checking(Elem%next, is_closed, pos, num_bracket,res)
           else
              end if
           else
              if (.not.is_closed) &
                res =  " There is a lack of bracket"
  
           end if
  
  
  
     end function Checking
  


end module List_process

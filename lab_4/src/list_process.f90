
module List_Process
   use Environment
   use List_IO

   implicit none

contains


   recursive function Check(Elem, is_closed, pos, num_bracket) result(res)
      class(node), pointer ::           Elem
      logical , intent(inout) ::       is_closed
      integer,  intent(inout) ::       pos, num_bracket
      character(:), allocatable ::           res
      !integer              ::       res
     
    !  print *, "kall" // pos
         if (pos==1) then
         res = "norm"
      end if
     print *, res
     

       if (Associated(Elem)) then
         select type (Elem)
            !type is (variable)
            !type is (operation)

            !type is (operand)

            type is (left_bracket)
              print*, "HEl"
              is_closed = .false.
              num_bracket = num_bracket + 1
              
            type is (right_bracket)
               num_bracket = num_bracket - 1
               if (num_bracket == 0) then
                  is_closed = .true.
               else if (num_bracket < 0) then
                 res =  " Warning! EXTRA RIGHT BRACKET!!!"
                 ! res = pos
               end if


            type is (cringe)
               if (Elem%char==" ") then
                  res =  " Extra space!"//pos

                 !res = pos
               else
                 res= " Unknown character!"

                 !res = pos
               end if

         end select

      end if
         pos = pos + 1
         if (Associated(Elem%next)) then
            print*, is_closed, num_bracket
            !res = Check_Next(Elem,pos)
            if (res=="norm") then
               res = Check_Next(Elem,pos)
               if (res=="norm")&
                res =  Check(Elem%next, is_closed, pos, num_bracket)
            else
               print *, res
            end if
         else
            if (.not.is_closed) &
              res =  " There is a lack of bracket"
              ! res = -2

            print *, is_closed, num_bracket
         end if


      
   end function Check 
         
   recursive function Check_Next(Elem,pos) result(res)

      class(node), pointer ::  Elem, nxt
      integer,intent(in)   ::  pos
      character(:), allocatable  :: res
     ! integer::res
      res = "norm"

    if (Associated(Elem%next)) then

      nxt => Elem%next
         select type (Elem)
            type is (variable)
               select type (nxt)
                  type is (variable)
                     res =  "Warning! forbidded character at position.Mostlikely missing operation at "//pos
                   ! res = pos
                  type is (operand)

                     res =  "Warning! forbidded character at position.Mostlikely missing operation in "//pos

                  type is (left_bracket)
                     res =  " Operation is missed in "//pos
                     !res = pos
               end select
            type is (operation)
               print*,("huy")
                 select type (nxt)
                 ! type is (variable)
                 !    print *, "Warning! forbidded character at position.Mostlikely     missing operation"
                  type is (operation)
                     if (nxt%char/='-')&
                       res =  "Extra operation in "//pos
                        !res = pos


                  type is (right_bracket)
                     res =  " Extra right bracket in "//pos

                 !res = pos
               end select

            type is (operand)
                select type (nxt)
                   type is (variable)
                      res = "Warning! forbidded character at position.Mostlikely     missing operation in "//pos

                 !res = pos
                   type is (operand)
 
                     res = "Warning! forbidded character at position.Mostlikely     missing operation in "//pos
 
                 !res = pos
                   type is (left_bracket)
                      res = " Operation is missed in "//pos
                end select
           type is (left_bracket)
            select type (nxt)
                   type is (operation)
                      if (nxt%char/='-')&
                           res =  "Extra operation in"// pos
 
                 !res = pos
 
                   type is (right_bracket)
                        res =  " Empty expression in "//pos
 
                 !res = pos
            end select
            type is (right_bracket)
               select type (nxt)
                  type is (variable)
                   res =  "Warning! forbidded character at position.Mostlikely     missing operation in "//pos
                   
                 !res = pos
                  type is (operand)

                    res = "Warning! forbidded character at position.Mostlikely     missing operation in "// pos

                  type is (left_bracket)
                     res =  " Operation is missed in " // pos

                ! res = pos
               end select

           ! end select




         end select
    end if
        print*, res 

   end function  Check_Next


end module List_process

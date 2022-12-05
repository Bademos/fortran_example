
program lab_1_2
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 3, SURNAME_LEN = 14, NUMS_LEN = 10
   character(:), allocatable        :: input_file, output_file

   character(kind=CH_)  :: Surnames( SURNAME_LEN, STUD_AMOUNT)  = "", &
                           Nums( NUMS_LEN, STUD_AMOUNT) = ""  

   input_file  = "../data/list.txt"
   output_file = "output.txt"
   
   call Read_class_list(input_file, Surnames, Nums)
   call Sort_Ch(Surnames,Nums)
   call Output_class_list(output_file, Surnames, Nums, "rewind","ordered list:")
      
contains
   subroutine Read_class_list(Input_File, Surnames, Nums)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Nums(:, :)
      intent (in)          Input_File
      intent (out)         Surnames, Nums

      integer In, IO, i
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // NUMS_LEN // 'a1, 1x)'
         read (In, format, iostat=IO) (Surnames(:, i), Nums(:,i), i = 1, STUD_AMOUNT)
        
         call Handle_IO_status(IO, "reading class list")
      close (In)
   end subroutine Read_class_list

   subroutine Output_class_list(Output_File, Surnames, Nums, Position,List_name)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Surnames(:, :), Nums(:, :)
      intent (in)          Output_File, Surnames, Nums, Position

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
        write (out,* ) List_name
         
         format = '(' // SURNAME_LEN // 'a1, 1x' // NUMS_LEN // 'a1, 1x)'
         write (Out, format, iostat=IO) &
            (Surnames(:, i),Nums(:,i), i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list


   pure logical function GT(arr1, arr2)
      character(kind=CH_), intent(in) :: arr1(:), arr2(:)

      integer :: i 

      do i = 1, Min(Size(arr1), Size(arr2)) - 1
         if (arr1(i) /= arr2(i)) &
            exit
      end do
      GT = arr1(i) > arr2(i)  
   end function GT
   

  subroutine Sort_ch(Surnames,Nums)
 
         integer, parameter               :: STUD_AMOUNT = 3, SURNAME_LEN = 14, NUMS_LEN = 10
         integer :: i,j,tempnum
         logical :: Swap
         character(kind=CH_), intent(inout)  :: Surnames( SURNAME_LEN, STUD_AMOUNT)
         character(kind=CH_), intent(inout)  :: Nums( NUMS_LEN, STUD_AMOUNT)
         character(kind=CH_ ) :: tmpNums(NUMS_LEN)
         character(kind=Ch_)  :: tmpSurnames(SURNAME_LEN)




         do i = 1, STUD_AMOUNT-1
            tempnum = i
            tmpSurnames = Surnames(:,i)
            tmpNums = Nums(:,i)
 
            do j=i+1, STUD_AMOUNT
 
          swap = (GT(TmpSurnames,Surnames(:,j) ).or.All(Surnames(:,j)==tmpSurnames).and.GT(tmpNums,Nums(:,j)))
               if(Swap) then
                   tempnum = j
                   tmpSurnames = Surnames(:,j)
                   tmpNums = Nums(:,j)
               end if
 
 
 
           end do
           Surnames(:,tempnum) = Surnames(:,i)
           Nums(:,tempnum) = Nums(:,i)
 
 
           Surnames(:,i) = tmpSurnames
           Nums(:,i) = tmpNums
 
        end do
 
 
     end subroutine Sort_ch


   

end program lab_1_2

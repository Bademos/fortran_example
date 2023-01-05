

program lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2

   type(SourceLine), pointer :: Code   => Null(),Res  
   integer                   :: pos 
   character(:,Ch_), allocatable          :: string
   F1 = "../data/mod_source.f90"
   F2 = "source.f90.diff"
   
   Code  => Read_Source_Code(F1)
   ! call Output_Source_Code(F2,Code)
  
!   if (Associated(Code)) then
!        call Process(Code,1,1,21)
!     if (Associated(Code)) &
!      call Output_Source_Code(F2,Code)
!   end if
      pos = 10
      Res =>  Get(Code,pos)
      string =  Res%String
      !string = string(1:4)
      print *, "hui",string
! call Process(Code,1,1,21)
! call InsertionSort(Code,Code) 
 
call Output_Source_Code(F2,Code)




end program lab_2

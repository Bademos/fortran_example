

program reference_lab_2
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, F3
   type(SourceLine), allocatable ::Code, Code_Dict
   


   F1 = "../data/source.f90"
   F2 = "../data/mod_source.f90"

  ! F1 = "../data/f1.txt"
  ! F2 = "../data/f2.txt"
   !F3 = "../data/f3.txt"
   F3 = "source.f90.diff"
   
   Code = Read_Source_Code(F1)
   Code_Dict = Read_Source_Code(F2)
   !call  Delete(Code,Code%Next%Next%Next%String)
   call Process(code,1,1,11)
  ! call Conjuct(Code,Code_Dict)
   call Output_Source_Code(F3, Code,"rewind")
  ! ModdedCode  => Read_Source_Code(F2)
   ! call Output_Source_Code(F3, ModdedCode)
  

end program reference_lab_2

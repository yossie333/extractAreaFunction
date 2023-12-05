subroutine writeArea
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This subroutine output the area function.
! The output directory is defined in param.txt.
!*******************************************************************
        use variables
        implicit none
        integer i,j,iunit
        character(80)filename

        filename = "areafunc.txt"
        filename = trim(rdir) // "/" //filename
        
        write(*,*)"output: ",filename

        iunit=10
        open(iunit,file=filename,status="replace")

        write(iunit,*)"Areafunc for ",fstl
        write(iunit,*)" Distance,   Area"

        do i=1,numplane
           write(iunit,'(2E15.7)')(areafunc(i,j),j=1,2)
        enddo

        close(iunit)
        write(*,*)"End writing areafunc"
        write(*,*)

end subroutine writeArea

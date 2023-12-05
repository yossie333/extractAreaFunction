subroutine writeVTK
!*******************************************************************
! Fortran program for extract area function from STL
! Copyright (c) 2023  Tsukasa Yoshinaga
! 
! This program output the VTK mesh file from the STL file.
! Just for the check of readSTL.
! The output file: result/
! This version only allows to write triangular faces.
!*******************************************************************
        use variables
        implicit none
        integer i,j,k,iunit
        character(80)filename,extention


        extention = ".vtk"

        filename= trim(rdir) // "/surface" //extention
        write(*,*)"output: ",filename

        iunit=10
        open(iunit,file=filename,status="replace")
           write(iunit,'("# vtk DataFile Version 3.0")')
           write(iunit,'("Unstructured grid")')
           write(iunit,'("ASCII")')
           write(iunit,'("DATASET UNSTRUCTURED_GRID")')
           write(iunit,'("POINTS ",I8," float")')nov*3
           do i=1,nov
              do j=1,3
                  write(iunit,'(3E17.5)')sx(j,i),sy(j,i),sz(j,i)
              enddo
           enddo
           write(iunit,'("CELLS ",2I8)')nov,nov*4
           do i=1,nov
                write(iunit,'(4I6)')3,(i-1)*3,(i-1)*3+1,(i-1)*3+2
           enddo
           write(iunit,'("CELL_TYPES ",I8)')nov
           do i=1,nov
                write(iunit,'(I6)')5
           enddo

        close(iunit)

end subroutine writeVTK

subroutine writeVTK3(fnm,n)
!*******************************************************************
! Fortran program for extract area function from STL
! Copyright (c) 2023  Tsukasa Yoshinaga
! 
! This program output the VTK line file from the stored centerline
! and cross-sections xc,yc,zc.
! The lines are stored from odd to even number points.
! Therefore, num should be even number.
! The output filename is input as fnm. n is the number of character.
! The output file: result/
! This version only allows to write lines.
!*******************************************************************
        use variables
        implicit none
        integer i,j,k,iunit,n
        character(80) filename,extention
        character(n) fnm

        if (mod(num,2) .ne. 0) then
                write(*,*)"Error, num is odd.."
                stop
        endif

        extention = ".vtk"

        filename= trim(rdir) //"/"// fnm //extention
        write(*,*)"output: ",filename
        write(*,*)

        iunit=10
        open(iunit,file=filename,status="replace")
           write(iunit,'("# vtk DataFile Version 3.0")')
           write(iunit,'("Unstructured grid")')
           write(iunit,'("ASCII")')
           write(iunit,'("DATASET UNSTRUCTURED_GRID")')
           write(iunit,'("POINTS ",I8," float")')num
           do i=1,num
              write(iunit,'(3E17.5)')xc(i),yc(i),zc(i)
           enddo
           write(iunit,'("CELLS ",2I8)')num/2,num/2*3
           do i=1,num/2
                write(iunit,'(4I6)')2,(i-1)*2,(i-1)*2+1
           enddo
           write(iunit,'("CELL_TYPES ",I8)')num/2
           do i=1,num/2
                write(iunit,'(I6)')3
           enddo

        close(iunit)


end subroutine writeVTK3

subroutine readParam
!*******************************************************************
! Fortran program for extract areafunction 
! Copyright (c) 2023  Tsukasa Yoshinaga
! 
! This program read parameters from param.txt
! List for parameters
!       nx,nyz,nz : number of initial planes (original: 20,20,20)
!       zstart,zend: z-coordinate for horizontal planes
!       ystart,yend: y-coordinate for verical planes
!       xg, yg, zg: coordinate point for glottal start 
!       rrthrsh: parameter to exclude the area far from the centerline
!       numplane: number of planes for output
!       nummax: memory size for xc,yc,zc (original: 50000)
!       idir,rdir: input and output directory name
!       fstl: filename for STL file
!
!*******************************************************************
        use variables
        implicit none
        integer iunit
        character(80)tmp

        pi = 4.d0*datan(1.d0)

        iunit = 10
        write(*,'()')
        write(*,*)"Start reading param.txt"

        open(iunit,file="input/param.txt",status="old")
        read(iunit,'(A)')tmp
        write(*,'(A)')tmp

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)ny,nyz,nz
        write(*,'(3I5)')ny,nyz,nz

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)zstart,zend
        write(*,'(2E17.5)')zstart,zend

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)ystart,yend
        write(*,'(2E17.5)')ystart,yend

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)xg,yg,zg
        write(*,'(3E17.5)')xg,yg,zg

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)rrthrsh
        write(*,'(E17.5)')rrthrsh

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)numplane
        write(*,'(I3)')numplane

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)nummax
        write(*,'(I8)')nummax
        allocate(xc(nummax),yc(nummax),zc(nummax))

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)idir
        write(*,'(A)')idir
        idir=trim(adjustl(idir))

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)rdir
        write(*,'(A)')rdir
        rdir=trim(adjustl(rdir))

        read(iunit,'(A)')tmp
        write(*,'(A)')tmp
        read(iunit,*)fstl
        write(*,'(A)')fstl
        fstl=trim(idir) // "/" // trim(adjustl(fstl))
        

        close(iunit)

        write(*,*)"End reading param.txt"

end subroutine readParam

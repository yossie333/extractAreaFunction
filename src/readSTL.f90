subroutine readSTL
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This program read ASCII STL file.
! The number of triangles is stored as nov
! the coordinate points for each triangle is stored as sx,sy,sz
!*******************************************************************
        use variables
        implicit none
        integer i,j,iunit
        character(80)tmp,vertex

        iunit=10
        write(*,'()')
        write(*,*)"Start reading ",fstl

        !counting line number for stl file
        open(iunit,file=fstl,status='old')

        i=0
        do
           read(iunit,*)tmp
           i = i + 1
           if (tmp(1:8) .eq. "endsolid") exit
        enddo

        close(iunit)

        !number of vertices, nov
        if ( mod((i-2),7) .ne. 0) then
                write(*,*)"Error: broken stl file"
        endif
        nov = (i-2)/7
        write(*,*)"number of vertex, ",nov

        allocate(sx(3,nov),sy(3,nov),sz(3,nov))

        !read vertices
        open(iunit,file=fstl,status='old')
        read(iunit,*)tmp

        do i=1,nov
           read(iunit,*)tmp
           read(iunit,*)tmp

           do j=1,3
              read(iunit,*)vertex,sx(j,i),sy(j,i),sz(j,i)
           enddo

           read(iunit,*)tmp
           read(iunit,*)tmp
        enddo

        close(iunit)
        write(*,*)"End reading ", fstl
        write(*,'()')

end subroutine readSTL

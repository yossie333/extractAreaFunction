subroutine iniPlane(nop)
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This subroutine calculate the initial planes for calculation
! of the centerline of vocal tract. First, the horizontal planes
! are calculated from z = zstart to zend defined in param.txt.
! Then, calculate the planes along circular axis from horizontal
! to vertical. Finally, the vertical planes were calculated
! from y = ystart to yend. On each plane, the cross-section of
! the vocal tract is calculated in subroutine planeCut
! parameters: numc, storing number of points for each plane.
!             x0,y0,z0, the origine of cutting plane
!             p,q,r, vector normal to the plane
!*******************************************************************
        use variables
        implicit none
        integer i,nop
        double precision x0,y0,z0,p,q,r,deg

        allocate(numc(nz+ny+nyz-1))

        write(*,*)"Extract horizontal planes ",nz
        x0 = xg
        y0 = yg
        p = 0.d0
        q = 0.d0
        r = 1.d0
        do i=1,nz
           z0 = zstart + (zend-zstart)/(nz-1)*(i-1)
           call planeCut(x0,y0,z0,p,q,r,i,0)
        enddo
        write(*,*)

        write(*,*)"Extract middle planes ",nyz-1
        x0 = xg
        y0 = ystart
        z0 = zend
        p = 0.d0
        do i=1,nyz-1
           deg = pi/2.d0/nyz
           q = sin(deg*dble(i))
           r = cos(deg*dble(i))
           call planeCut(x0,y0,z0,p,q,r,i+nz,0)
        enddo
        write(*,*)

        write(*,*)"Extract vertical planes ",ny
        p = 0.d0
        q = 1.d0
        r = 0.d0
        do i=1,ny
           y0 = ystart + (yend-ystart)/(ny-1)*(i-1)
           call planeCut(x0,y0,z0,p,q,r,i+nz+nyz-1,0)
        enddo
        write(*,*)

        nop = nz+ny+nyz-1
        write(*,*)"number of extracted planes: nop = ", nop
        write(*,*)

end subroutine iniPlane

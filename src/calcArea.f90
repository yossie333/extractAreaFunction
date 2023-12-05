subroutine calcArea(x0,y0,z0,n,x,y,z,area)
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This subroutine calculate the area from a circle on the plane.
! First, detecting the closest point from the center line x0,y0,z0.
! Then, searching the next line from the previous point to connect
! for one circle. This procedure exclude another circle.
! The area is calculated from the sum of triangles for points at
! the centerline and circuler line.
! Input: x0,y0,z0, coordinates for centerline
!        n, numer of points for the circle
!        x, y, z, coordinates for the circle lines
! Output: area, area calculated from the sum of triangles.
!*******************************************************************
        use variables
        implicit none
        integer i,j,k,n,imin,iflg,ii
        double precision x0,y0,z0
        double precision x(n),y(n),z(n)
        double precision xorg,yorg,zorg,rmin
        double precision a1,a2,a3,b1,b2,b3
        double precision area,r,di

        !initial value for searching maximum
        rmin = 1.0d10
        !find grids with seventh order
        di = 1.d7

        do i=1,n
           r = sqrt((x(i)-x0)**2+(y(i)-y0)**2+(z(i)-z0)**2)
           if (r .lt. rmin)then
                   rmin = r
                   imin = i
           endif
        enddo
        !set the initial point
        i = imin
        xorg = x(i)
        yorg = y(i)
        zorg = z(i)
        !write(*,*)"start point: ",i,xorg,yorg,zorg

        area = 0.d0
        do j=1,n
           !calculate the triangle area
           if ( mod(i,2) .eq. 0 )then
                ii = i - 1
           else
                ii = i + 1
           endif
              a1 = x0 - x(i)
              a2 = y0 - y(i)
              a3 = z0 - z(i)
              b1 = x(ii) - x(i)
              b2 = y(ii) - y(i)
              b3 = z(ii) - z(i)
              area = area + 0.5d0*sqrt((a2*b3-a3*b2)**2+(a3*b1-a1*b3)**2+(a1*b2-a2*b1)**2)
           
              !searching next point
              !if found, iflg -> 1
              iflg = 0
              do k=1,ii-1
                 if (nint(x(ii)*di).eq.nint(x(k)*di).and.nint(y(ii)*di).eq.nint(y(k)*di) &
                         .and.nint(z(ii)*di).eq.nint(z(k)*di)) then
                     i = k
                     iflg = 1
                     exit
                 endif 
              enddo
              do k= ii+1, n
                 if (iflg .eq. 1 ) then
                      exit
                 elseif (nint(x(ii)*di).eq.nint(x(k)*di).and.nint(y(ii)*di).eq.nint(y(k)*di) &
                         .and.nint(z(ii)*di).eq.nint(z(k)*di)) then
                      i = k
                      iflg = 1
                      exit
                 endif
              enddo
              if (iflg .eq. 0)then
                   write(*,*)"Error calculating area: stop"
                   stop
              endif
              if (x(i).eq.xorg .and. y(i).eq.yorg .and. z(i).eq.zorg) then
                   write(*,*)"Area: ", area
                   exit
              endif
        enddo

end subroutine calcArea

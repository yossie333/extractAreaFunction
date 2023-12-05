subroutine rePlane(nop,icstrt)
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This subroutine calculate the cross-section from the b-spline
! centerline. First, the centerline is stored to xctl from xc.
! Then, the b-spline curve is calculated from the centerline.
! 
!*******************************************************************
        use variables
        implicit none
        integer i,j
        integer nop,icstrt,nbmax
        double precision x0,y0,z0,p,q,r
        double precision p1,q1,r1,p2,q2,r2
        double precision tot,dt
        double precision xctl(nop),yctl(nop),zctl(nop)

        ! number of points for b-spline
        nbmax = 1000

        do i=1, nop-1
           xctl(i)=xc(icstrt+(i-1)*2+1)
           yctl(i)=yc(icstrt+(i-1)*2+1)
           zctl(i)=zc(icstrt+(i-1)*2+1)
        enddo
        xctl(nop)=xc(icstrt+(i-1)*2)
        yctl(nop)=yc(icstrt+(i-1)*2)
        zctl(nop)=zc(icstrt+(i-1)*2)

        ! calculate the b-spline (order = 20)
        call bspline(20,xctl,yctl,zctl,nop,xc,yc,zc,nbmax)

        ! calculate the length of the center line, tot
        tot = 0.d0
        do i=1,nbmax-1
           tot = tot + sqrt((xc(i+1)-xc(i))**2+(yc(i+1)-yc(i))**2+(zc(i+1)-zc(i))**2)
        enddo

        ! the centerline is divided by dt.
        ! adjustment to get the number of planes
        dt = tot/dble(numplane+2)

        allocate(areafunc(numplane,2))

        ! find the next point farer than dt
        tot = 0.d0
        do i=2,nbmax-1
           r=sqrt((xc(i)-xc(num))**2+(yc(i)-yc(num))**2+(zc(i)-zc(num))**2)
           if( r .ge. dt ) then
                   if (num .ne. 1)then
                      num=num+1
                      xc(num)=xc(num-1)
                      yc(num)=yc(num-1)
                      zc(num)=zc(num-1)
                   endif
                   num=num+1
                   xc(num)=xc(i)
                   yc(num)=yc(i)
                   zc(num)=zc(i)
                   areafunc(num/2,1)=tot
                   tot = tot + r
           endif
        enddo

        if (numplane .ne. num/2)then
                write(*,*)"Error: adjust the value in rePlane"
                stop
        endif

        tot = tot / dble(numplane)
        write(*,*)"Number of plane for output ", numplane
        write(*,*)"Mean distance between planes ", tot

        ! calculating the cross-section from new centerline
        do i=1,numplane

           !initial plane is horizontal
           if ( i .eq. 1 )then
                   p = 0.d0
                   q = 0.d0
                   r = 1.d0
           else
                   p = xc(i*2)-xc(i*2-1)
                   q = yc(i*2)-yc(i*2-1)
                   r = zc(i*2)-zc(i*2-1)
           endif

           call planeCut(xc(i*2-1),yc(i*2-1),zc(i*2-1),p,q,r,i,1)
        enddo

end subroutine rePlane

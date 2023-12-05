subroutine planeCut(x0,y0,z0,p,q,r,ic,ia)
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This subroutine cut the STL file on the plane
! defined by the origin x0,y0,z0 and the normal
! vector p,q,r. The subroutine at the end of this
! program calculate the area for area function if
! ia = 1.
! Input: 
!       p,q,r angles for cutting plane
!       x0,y0,z0, origin of the cutting plane
!       numo: num of previous plane
!       ic, this is nth plane for calculating the cross-section.
!       ia, swith for calculation
!       rrthrsh, threshold value for the cross-section
!               (if the radius of the cross-section is larger than
!                this, it exclude from the cross-section.)
! Output: 
!       xc,yc,zc, coordinate points for the cross-section
!       num, total number of points for the cross-section
!       numc, number of points for this plane
!*******************************************************************
        use variables
        implicit none
        integer i,j,cnt,ic,ia,numo
        integer,parameter :: idx(3) = (/2, 3, 1/)
        double precision x,y,z,l,m,n
        double precision x0,y0,z0,p,q,r
        double precision xctmp(2),yctmp(2),zctmp(2)
        double precision rr,rrmax,area

        !ia: switch to calculate the area (ini=0,re=1)

        rrmax = 0.d0
        numo = num
        
        do i=1, nov
           !counting for number of clossing points
           cnt = 0

           do j=1,3
              !slopes for line of triangles
              l = sx(idx(j),i)-sx(j,i)
              m = sy(idx(j),i)-sy(j,i)
              n = sz(idx(j),i)-sz(j,i)

              ! if the line of z coordinate was the same
              if (n .eq. 0.d0) then
                  z = sz(j,i)

                  ! if the line of x coordinate was also the same
                  if (l .eq. 0.d0) then
                          x = sx(j,i)
                          y = y0- r*(z-z0)+p*(x-x0)/q
                  else
                          x = (p*x0+q*(y0-sy(j,i))+m*q*sx(j,i)/l-r*(z-z0))/(p+m*q/l)
                          y = sy(j,i) + m*(x - sx(j,i))/l
                  endif

              else
              
                  ! calculating the crossing point of STL line and cutting plane
                  z = (p*(x0-sx(j,i))+q*(y0-sy(j,i))+r*z0+(p*l+q*m)*sz(j,i)/n)/(p*l/n+q*m/n+r)
                  x = sx(j,i) + l*(z - sz(j,i))/n
                  y = sy(j,i) + m*(z - sz(j,i))/n
              
              endif

              ! if the crossing point was inside the STL triangle ...
              if(x .ge. min(sx(j,i),sx(idx(j),i)) .and. x .le. max(sx(j,i),sx(idx(j),i)) )then
                if(y .ge. min(sy(j,i),sy(idx(j),i)) .and. y .le. max(sy(j,i),sy(idx(j),i)))then
                  if(z .ge. min(sz(j,i),sz(idx(j),i)) .and. z .le. max(sz(j,i),sz(idx(j),i)))then
                     !write(*,*)i,x,y,z
                     cnt = cnt + 1
                     xctmp(cnt)=x
                     yctmp(cnt)=y
                     zctmp(cnt)=z
                  endif
                endif
              endif 
            enddo

            !if two points were inside the STL triangle, store the points
            ! rr ; distance of points from x0, y0, z0
            rr = sqrt((xctmp(1)-x0)**2+(yctmp(1)-y0)**2+(zctmp(1)-z0)**2)
            if(cnt .eq. 2 .and. rr .lt. rrthrsh)then
                    
                    ! store for maximum rr value
                    if(rr .gt.rrmax)then
                            rrmax=rr
                    endif

                    num = num + 1
                    xc(num)=xctmp(1)
                    yc(num)=yctmp(1)
                    zc(num)=zctmp(1)
                    num = num + 1
                    xc(num)=xctmp(2)
                    yc(num)=yctmp(2)
                    zc(num)=zctmp(2)
            elseif(cnt .eq. 1 .or. cnt .eq. 3 )then
                    write(*,*)"Warning in planeCut: ",i,cnt
            endif
        enddo

        numc(ic) = num - numo

        write(*,*)ic," number of points on this plane: ",numc(ic)
        write(*,*)" Total points (num): ",num
        write(*,'(A,F7.3)')"Max radius ",rrmax

        if (ia .eq. 1 ) then
                call calcArea(x0,y0,z0,numc(ic),xc(numo+1:num),yc(numo+1:num),zc(numo+1:num),area)
                areafunc(ic,2) = area
        endif

        numo = num
        if ( num .gt. nummax ) then
                write(*,*)"Error: need to increase nummax "
                write(*,*)"nummax = ",nummax," num = ",num
                stop
        endif

end subroutine planeCut

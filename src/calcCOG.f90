subroutine calcCOG(nop,icstrt)
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This subroutine calculate the center of gravity (COG).
! The initial planes were calculated by iniPlane and the
! central points were calculated as COG.
! Input : nop, number of planes defined by iniPlane
! Output: icstrt, storing the num for rePlane.
!         rePlane only uses the information of centerline
!
! subroutine COG: calculating center of gravity from
!                 coordinates x,y,z
!*******************************************************************
        use variables
        implicit none
        integer i,ista,iend
        integer nop,icstrt

        !storing the number of rePlane
        icstrt = num 
        
        !centerline from the current to next plane
        do i = 1,nop-1
           ista = sum(numc(1:i))-numc(i)+1
           iend = sum(numc(1:i))
           num = num + 1
           call COG(xc(ista:iend),yc(ista:iend),zc(ista:iend),numc(i), &
                    xc(num),yc(num),zc(num))

           ista = sum(numc(1:i+1))-numc(i+1)+1
           iend = sum(numc(1:i+1))
           num = num + 1
           call COG(xc(ista:iend),yc(ista:iend),zc(ista:iend),numc(i+1), &
                    xc(num),yc(num),zc(num))
        enddo

        if ( num .gt. nummax ) then
                write(*,*)"Error: need to increase nummax "
                write(*,*)"nummax = ",nummax," num = ",num
                stop
        endif

end subroutine calcCOG
subroutine COG(x,y,z,n,xctr,yctr,zctr)
        implicit none
        integer i,n
        double precision x(n),y(n),z(n)
        double precision xctr,yctr,zctr

        xctr = 0.d0
        yctr = 0.d0
        zctr = 0.d0
        do i=1,n
            xctr = xctr + x(i)
            yctr = yctr + y(i)
            zctr = zctr + z(i)
        enddo

        xctr = xctr / n
        yctr = yctr / n
        zctr = zctr / n

end subroutine COG

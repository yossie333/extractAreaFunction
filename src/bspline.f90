subroutine bspline(no,x,y,z,n,xi,yi,zi,nl)
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This subroutine calculate the b-spline curve
! Input: no, order for spline curve
!        x,y,z, coordinate points for input curve
!        n, number of points for input
!        nl, number of points for spline curve
! Output: xi,yi,zi, output cordinates for the spline curve
!*******************************************************************
        implicit none
        integer i,j,k,n,m,no,nl
        double precision x(n),y(n),z(n)
        double precision xi(nl),yi(nl),zi(nl),a(nl)
        double precision, allocatable :: u(:),bj(:,:,:)

        !m: vector size for u (number of knots)
        !bj: basis function
        m = n + no + 1
        allocate(u(m),bj(nl,m-1,no+1))

        !initializing
        !a: knots changed from 0 to 1
        do i=1,nl
           a(i) = 1.d0/dble(nl-1)*dble(i-1)
           do k=1,no+1
              do j=1,m-1
                 bj(i,j,k)=0.d0
              enddo
           enddo
           xi(i) = 0.d0
           yi(i) = 0.d0
           zi(i) = 0.d0
        enddo

        ! preparing knots u
        do i = 1, m
           if (i .le. no+1) then
                  u(i) = 0.d0
           elseif (i .ge. m-no) then
                  u(i) = 1.d0
           else 
                  u(i) = 1.d0/dble(n-2)*dble(i-no-1)
           endif
        enddo

        ! calculate basis function bj
        do j = 1+no, m-1-no
           do i = 1, nl
              if (a(i) .ge. u(j) .and. a(i) .lt. u(j+1)) then
                      bj(i,j,1) = 1.d0
              elseif(j .eq. m-1-no .and.a(i) .eq. u(m-no))then
                      bj(i,j,1) = 1.d0
              else
                      bj(i,j,1) = 0.d0
              endif
           enddo
        enddo

        do k = 1, no
           do j = 1, m-k-1
              do i = 1, nl
                 if (u(j+k) .ne. u(j)) then
                    bj(i,j,k+1)=(a(i)-u(j))*bj(i,j,k)/(u(j+k)-u(j))
                 endif
                 if (u(j+k+1) .ne. u(j+1)) then
                    bj(i,j,k+1)=bj(i,j,k+1)+(u(j+k+1)-a(i))*bj(i,j+1,k)/(u(j+k+1)-u(j+1))
                 endif
              enddo
           enddo
        enddo

        ! interplate from x,y,z to xi,yi,zi
        do i = 1, nl
           do j = 1, n
              xi(i) = xi(i) + x(j)*bj(i,j,no+1)
              yi(i) = yi(i) + y(j)*bj(i,j,no+1)
              zi(i) = zi(i) + z(j)*bj(i,j,no+1)
           enddo
        enddo

end subroutine bspline

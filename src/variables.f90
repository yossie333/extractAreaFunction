module variables
!*******************************************************************
! Fortran program for extract areafunction
! Copyright (c) 2023 Tsukasa Yoshinaga
! 
! This module defines the variables used in this program
!*******************************************************************
        implicit none

        !readParam
        integer ny,nyz,nz
        double precision pi,zstart,zend,ystart,yend
        double precision xg,yg,zg
        character(80) idir,rdir,fstl

        !readSTL
        integer nov
        double precision, allocatable:: sx(:,:),sy(:,:),sz(:,:)

        !planeCut
        integer num,nummax
        integer,allocatable :: numc(:)
        double precision rrthrsh
        double precision,allocatable ::  xc(:),yc(:),zc(:)

        !rePlane
        integer numplane
        double precision, allocatable :: areafunc(:,:)

end module variables

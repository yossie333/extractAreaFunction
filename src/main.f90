program main
!*******************************************************************
! Fortran program for extracting area function
! Copyright (c) 2023  Tsukasa Yoshinaga
! 
! This program calculate the vocal tract area function from STL file.
! The method is partly based on Arnela et al., (2016) J.Acout.Soc.Am.
! First, the inital planes were extracted from the horizontal planes
! near the glottis and pharynx, oblique planes at pharynx, and vertical
! planes in the oral cavity. Then, the centerline was calculated from
! the initial plane, and the line is smoothed by b-spline.
! Based on the b-spline, the centerline was equally separated and
! final planes were calculated for the areafuction.
! Note that the vocal tract in the STL file should has a direction
! along the z-axis with glottis and the y-axis with the mouth opening.
!
! Input files: Parameter file (param.txt)
!              Vocal Tract geometry (STL file) 
! Output files: initial planes for centerline (ring1.vtk)
!               final planes for cross-sectional area (ring2,vtk)
!               area function of the vocal tract(areafunc.txt)
!   
! Module file: variables
!*******************************************************************
      use variables
      implicit none
      integer d(8),ti,tf,tr,tmax
      integer nop,icstrt
      
      call date_and_time(values=d)
      call system_clock(ti)

      write(*,'("Solver: modeFold start")')
      write(*,'(a,i0,a,i0.2,a,i0.2,a,i0.2,a,i0.2)')&
              'Date: ',d(1),'/',d(2),'/',d(3),', Time:',d(5),':',d(6)

      !initial reading files
      call readParam

      ! read a STL file for areafunction
      call readSTL
      !call writeVTK

      ! calculate the initial planes
      num = 0  !initializing number
      call iniPlane(nop)

      ! calculate center of each initial planes
      call calcCOG(nop,icstrt)

      ! output for the initial planes and a center line
      ! write output file name and number of character
      call writeVTK3("ring1",5)

      ! re-calculate the planes depending on the center line
      num = 1  !initializing number
      call rePlane(nop,icstrt)  

      ! output for the final planes
      ! write output file name and number of character
      call writeVTK3("ring2",5)

      ! output areafunction
      call writeArea  

      !output date and time
      call date_and_time(values=d)
      call system_clock(tf,tr,tmax)

      write(*,*) 'Successfully extractAreaFunction DONE !!'
      write(*,'(a11,i0,a,i0.2,a,i0.2,a,i0.2,a,i0.2)')&
            &' End time: ',d(1),'/',d(2),'/',d(3),'  ',d(5),':',d(6)
      if (tf < ti) then
         write(*,'(a,f10.1)')' Duration (s):',((tmax-ti)+tf+1)/dble(tr)
      else
         write(*,'(a,f10.1)')' Duration (s):',(tf-ti)/dble(tr)
      endif

  end program main

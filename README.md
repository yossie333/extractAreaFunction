# extractAreaFunction
 Fortran program for extracting area function

 Copyright (c) 2023  Tsukasa Yoshinaga
  
 This program calculate the vocal tract area function from STL file.
 The method is partly based on Arnela et al., (2016) J.Acout.Soc.Am.
 First, the inital planes were extracted from the horizontal planes
 near the glottis and pharynx, oblique planes at pharynx, and vertical
 planes in the oral cavity. Then, the centerline was calculated from
 the initial plane, and the line is smoothed by b-spline.
 Based on the b-spline, the centerline was equally separated and
 final planes were calculated for the areafuction.
 Note that the vocal tract in the STL file should has a direction
 along the z-axis with glottis and the y-axis with the mouth opening.

 Input files: 

              Parameter file (param.txt)

              Vocal Tract geometry (STL file) 

 Output files: 
               initial planes for centerline (ring1.vtk)

               final planes for cross-sectional area (ring2,vtk)

               area function of the vocal tract(areafunc.txt)
   
 Module file: variables

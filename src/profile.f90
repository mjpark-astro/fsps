PROGRAM PROFILE

  !set up modules
  USE sps_vars; USE sps_utils
  
  IMPLICIT NONE

  !NB: the various structure types are defined in sps_vars.f90
  !    variables not explicitly defined here are defined in sps_vars.f90
  INTEGER :: i, zlo
  !define variable for SSP spectrum
  REAL(SP), DIMENSION(nspec, ntfull)  :: spec
  !define variables for Mass and Lbol info
  REAL(SP), DIMENSION(ntfull)    :: mass,lbol
  CHARACTER(100) :: outfile=''
  !structure containing all necessary parameters
  TYPE(PARAMS) :: pset
  !define structure for CSP spectrum
  TYPE(COMPSPOUT), DIMENSION(ntfull) :: ocompsp
  REAL(SP) :: zpos

  !---------------------------------------------------------------!
  !---------------------------------------------------------------!
  
  ! Now we're going to show you how to use full  
  ! metallicity-dependent info                   
  
  imf_type = 0    ! Salpeter IMF
  pset%sfh = 0    ! compute SSP
  pset%mag_compute = 0
  add_neb_emission = 1

  zpos = 0.013
  outfile = 'prf.out'

  !here we have to read in all the librarries
  CALL SPS_SETUP(-1)

  !compute bracketing ssps
  !nz and the various *ssp_zz arrays are stored 
  !in the common block set up in sps_vars.f90
  zlo = max(min(locate(log10(zlegend/zsol),zpos),nz-1),1)
  DO i=zlo,zlo+1
     pset%zmet = i
     CALL SSP_GEN(pset,mass_ssp_zz(:,i),&
          lbol_ssp_zz(:,i),spec_ssp_zz(:,:,i))
  ENDDO
  call ztinterp(zpos,spec,lbol,mass)
  call compsp(0,1,outfile,mass,lbol,spec,pset,ocompsp)



END PROGRAM PROFILE

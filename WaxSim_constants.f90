

 module constants
        
            
            !   Used modules
                use,intrinsic       ::  iso_c_binding,only  :   C_bool              !   C/C++ logical
            !   use,intrinsic       ::  iso_c_binding,only  :   rp  =>  C_double    !   C/C++ double precision
            !   use,intrinsic       ::  iso_c_binding,only  :   rp  =>  C_float     !   C/C++ single precision
            !   use,intrinsic       ::  iso_c_binding,only  :   ip  =>  C_int       !   C/C++ integer
                use,intrinsic       ::  iso_c_binding,only  :   C_null_char
                use,intrinsic       ::  iso_fortran_env,only:   ip  =>  int32       !   Fortran integer
            !   use,intrinsic       ::  iso_fortran_env,only:   rp  =>  real32      !   Fortran single precision
                use,intrinsic       ::  iso_fortran_env,only:   rp  =>  real64      !   Fortran double precision
            
            !   All variables must be declared
                implicit none
            
            !   Private subroutines and variables
                private             ::  C_bool
                private             ::  C_null_char
                private             ::  C_inf_num
   
            
            !   Public subroutines and variables
                public
            ! ---------------------------------------------------------------------------------------
            
            !   Numerical constants [-]
                real(rp),parameter              ::  zero = 0._rp
                real(rp),parameter              ::  one = 1._rp
                real(rp),parameter              ::  two = 2._rp
                real(rp),parameter              ::  four = 4._rp
                real(rp),parameter              ::  p001 = 0.001_rp
                real(rp),parameter              ::  p01 = 0.01_rp
                real(rp),parameter              ::  p1 = 0.1_rp
                real(rp),parameter              ::  p25 = 0.25_rp
                real(rp),parameter              ::  p33 = 1._rp / 3._rp
                real(rp),parameter              ::  p5 = 0.5_rp
            
            !   Machine numbers
                real(rp),parameter              ::  dmach(3) = [epsilon(zero), tiny(zero), huge(zero)]
            
            !   Pi number [-]
                real(rp),parameter              ::  pi = 3.14159265358979323846264338327950288419_rp
            
            !   Standart gravity [m/s**2]
                real(rp),parameter              ::  g = 9.80665_rp
            
            !   Dimensionless pipe area [-]
                real(rp),parameter              ::  ap = p25 * pi
            
            !   Degree [deg]
                real(rp),parameter              ::  deg = pi / 180._rp
            
            !   Character constants
                character(*),parameter          ::  no_error = 'no_error'
                character(*),parameter          ::  no_used = 'no_used'
            ! ---------------------------------------------------------------------------------------
            
            !   Data type for tolerances in numerical calculations
                type,private                    ::  tol_type
                    real(rp)                    ::  abs             !	Absolute error      [-]
                    real(rp)                    ::  eps             !	Previous root        [-]
                    real(rp)                    ::  eta             !	Increment           [-]
                    integer(ip)                 ::  max             !	Maximum iterations  [-]
                    real(rp)                    ::  rel             !	Relative error      [-]
                end type
            
            !   Tol_type structure constructor
                type(tol_type),parameter        ::  tol = tol_type ( sqrt(dmach(1)), 1e-6_rp, p01, &
                                                        50, sqrt(dmach(1)) )
            ! ---------------------------------------------------------------------------------------
            
            !   Data type for location 1
                type                            ::  loc1_type
                    integer(ip)                 ::  i               !   Interfacial gas-liq [-]
                    integer(ip)                 ::  j               !   Interfacial liq-liq [-]
                    integer(ip)                 ::  w               !   Wall                [-]
                end type
            
            !   Data type for film region models
                type                            ::  filmm_type
                    integer(ip)                 ::  fe              !   Entrainment model   [-]
                    real(rp)                    ::  fm              !   Maximum entrainment [-]
                    integer(ip)                 ::  wll             !   Liq-Liq wett. model [-]
                    type(loc1_type)             ::  f               !   Film friction model [-]
                end type
            
            !   Data type for slug region models
                type                            ::  slugm_type
                    integer(ip)                 ::  hl              !   Holdup model        [-]
                    integer(ip)                 ::  sm              !   Length model        [-]
                    integer(ip)                 ::  vd              !   Drift velocity model[-]
                    integer(ip)                 ::  vt              !   Tran. velocity model[-]
                end type
            
            !   Data type for slug unit model
                type                            ::  model_type
                    character(22)               ::  name            !   Model name          [-]
                    type(filmm_type)            ::  film            !   Film models data-type
                end type
            ! ---------------------------------------------------------------------------------------
            
            !   Data type for report
                type                            ::  inf_type
                    logical                     ::  er              !   Error               [-]
                    integer(ip)                 ::  it              !   Number of iterations[-]
                    character(20)               ::  nm              !   Numeric report      [-]
                end type
            
            !   Data type for report  (C/C++ link)
                type                            ::  inf_Ctyp
                    logical(c_bool)             ::  er              !   Error               [-]
                    integer(ip)                 ::  it              !   Number of iterations[-]
                    character(21)               ::  nm              !   Numeric report      [-]
                    contains
                        procedure,nopass        ::  constructor     => C_inf_num
                end type
            
            !   Data type for layer report
                type                            ::  layn_type
                    type(inf_type)              ::  f               !   Friction report data-type
                    type(inf_type)              ::  h               !   Holdup report data-type
                    type(inf_type)              ::  w               !   Wettability report data-type
                end type
            
            ! !   Data type for layer report  (C/C++ link)
            !     type                            ::  layn_Ctyp
            !         type(inf_Ctyp)              ::  f               !   Friction report data-type
            !         type(inf_Ctyp)              ::  h               !   Holdup report data-type
            !         type(inf_Ctyp)              ::  w               !   Wettability report data-type
            !         contains
            !             procedure,nopass        ::  constructor     => C_lay_num
            !     end type

            
            
            !   layn_type constructor
                type(inf_type),parameter        ::  inf_ini = inf_type ( .false., 0, no_used )
            

            ! ---------------------------------------------------------------------------------------
            
            !   Data type for fluid properties 1
                type                            ::  f1_type
                    real(rp)                    ::  den             !   Density             [kg/m^3]
                    real(rp)                    ::  stn             !   Surface tension     [N/m]
                    real(rp)                    ::  vel             !   Superficial velocity[m/s]
                    real(rp)                    ::  vis             !   Viscosity           [Pa*s]
                end type
            
            !   Data type for pipe section properties 1
                type                            ::  psp1_type
                    real(rp)                    ::  ang             !   Angle               [rad]
                    real(rp)                    ::  dia             !   Diameter            [m]
                    real(rp)                    ::  rou             !   Relative roughness  [-]
                end type
            ! ---------------------------------------------------------------------------------------
            
            !   Data type for pressure gradient
                type                            ::  prg_type
                    integer(ip)                 ::  fpc             !   Flow pattern code   [-]
                    real(rp)                    ::  hlg             !   Gas-liq holdup      [-]
                    real(rp)                    ::  hll             !   Liq-liq holdup      [-]
                    real(rp)                    ::  ace             !   Acc pressure grad   [Pa/m]
                    real(rp)                    ::  fri             !   Fri pressure grad   [Pa/m]
                    real(rp)                    ::  gra             !   Gra pressure grad   [Pa/m]
                    real(rp)                    ::  tot             !   Tot pressure grad   [Pa/m]
                end type
            
            !   Data type for location 2
                type,private                    ::  loc2_type
                    real(rp)                    ::  i               !   Interfacial         [-]
                    real(rp)                    ::  w               !   Wall                [-]
                end type
            
            !   Data type for fluid layer
                type                            ::  lay_type
                    real(rp)                    ::  h               !   Phase holdup        [-]
                    real(rp)                    ::  v               !   Velocity            [m/s]
                    type(loc2_type)             ::  f               !   Friction data-type
                    type(loc2_type)             ::  s               !   Perimeter data-type
                    type(loc2_type)             ::  t               !   Stress data-type
                end type
            
           
        
            !   prg_type constructor
                type(prg_type),parameter        ::  prg_ini =  prg_type ( 0, zero, zero, &
                                                        zero, zero, zero, zero )
            
            !   lay_type constructor
                type(lay_type),parameter        ::  lay_ini = lay_type ( zero, zero, &
                                                        loc2_type ( zero, zero ), &
                                                        loc2_type ( zero, zero ), &
                                                        loc2_type ( zero, zero ) )
            
          

            
            ! ***************************************************************************************
            
   
            
                ! ***************************************************************************************

            
     
        
        
        ! ---------------------------------------------------------------------------------------
        
        !   Data structure for fluid properties 2
            type,extends(f1_type)           ::  f2_type
                real(rp)                    ::  con             !   Conductivity            [W/(m*K)]
                real(rp)                    ::  enh             !   Enthalpy                [J/kg]
                real(rp)                    ::  epc             !   Expansion coefficient   [1/K]
                real(rp)                    ::  scp             !   Specific heat           [J/(kg*K)]
            end type
        
        !   Data structure for fluid properties 3
            type                            ::  f3_type
                real(rp)                    ::  con             !   Conductivity            [W/(m*K)]
                real(rp)                    ::	den             !   Density                 [kg/m^3]
                real(rp)                    ::  epc             !   Expansion coefficient   [1/K]
                real(rp)                    ::  scp             !   Specific heat           [J/(kg*K)]
                real(rp)                    ::	vis             !   Viscosity               [Pa*s]
            end type
        
        !   Data structure for pipe section properties data-type 2
            type,extends(psp1_type)         ::  psp2_type
                real(rp)                    ::  len             !   Length                  [m]
                real(rp)                    ::  tpo             !   Outer temperature       [K]
                real(rp)                    ::  ypr             !   Ref pressure            [-]
                integer(ip)                 ::  ynm(6)          !   Layers code name        [-]
                integer(ip)                 ::  ymt(6)          !   Layers material code    [-]
                real(rp)                    ::  ydi(6)          !   Layers diamater         [m]
                real(rp)                    ::  ytr(6)          !   Layers Resistance       [K*m/W]
            end type
        ! ---------------------------------------------------------------------------------------
        
        !   Data structure for wax components
            type                            ::  wxc_type
                character(4)                ::  nme             !   Names                   [-]
                real(rp)                    ::  cfr             !   Concentration fraction  [mol_c/mol]
                real(rp)                    ::	den             !   Density                 [kg_c/m^3_c]
                real(rp)                    ::  enh             !   Melting heat            [J_c/kg_c]
                real(rp)                    ::  mol             !   Molecular weigth        [kg_c/kmol_c]
            end type
        
        !   Data structure for wax properties
            type                            ::  wax_type
                real(rp)                    ::	den             !   Density                 [kg/m^3]
                real(rp)                    ::  cfr             !   Wax concentration frac  [mol/mol]
                real(rp)                    ::  enh             !   Enthalpy                [J/kg]
                real(rp)                    ::  mwl             !   Liq molecular weigth    [kg/kmol]
                real(rp)                    ::  mww             !   Wax molecular weigth    [kg/kmol]
                real(rp)                    ::  scp             !   Specific heat           [J/(kg*K)]
                real(rp)                    ::  wfr             !   Wax fraction            [mol/mol]
                real(rp)                    ::  dwf             !   Wax fraction d.w.r. tp  [mol/(mol*K)]
            end type
        
        !   Data structure for PVT data-type
            type                            ::  pvt_type
                integer(ip)                 ::  npt             !   Pressure points         [-]
                integer(ip)                 ::  ntt             !   Temperature points      [-]
                real(rp),allocatable        ::  pr(:)           !   Pressure                [Pa]
                real(rp),allocatable        ::  tp(:)           !   Temperature             [K]
                real(rp),allocatable        ::  enh(:,:)        !   Enthalpy                [J/kg]
                type(f2_type),allocatable   ::  gas(:,:)        !   Gas properties data-type
                type(f2_type),allocatable   ::  liq(:,:)        !   Liq properties data-type
            end type
        
        !   Data structure for THM data-type
            type                            ::  thm_type
                integer(ip)                 ::  npt             !   Pressure points         [-]
                integer(ip)                 ::  ntt             !   Temperature points      [-]
                real(rp)                    ::  vis(3)          !   Oil-wax vis multipliers [-]
                real(rp),allocatable        ::  pr(:)           !   Pressure                [Pa]
                real(rp),allocatable        ::  tp(:)           !   Temperature             [K]
                real(rp),allocatable        ::  tpc(:)          !   Cloud temperature       [K]
                type(wax_type),allocatable  ::  wax(:,:)        !   Wax properties data-type
            end type
        ! ---------------------------------------------------------------------------------------
        
        !   Data structure for pipe section data-type
            type                            ::  sec_type
                real(rp)                    ::  lp              !   Pipeline length         [m]
                real(rp)                    ::  di              !   Inner diameter          [m]
                real(rp)                    ::  dx              !   Section length          [m]
                real(rp)                    ::  fw              !   Wax weigth fraction     [-]
                real(rp)                    ::  pr              !   Pressure                [Pa]
                real(rp)                    ::  th              !   Wax layer thickness     [m]
                real(rp)                    ::  ti              !   Inner diameter temp     [K]
                real(rp)                    ::  tp              !   Bulk temperature        [K]
                real(rp)                    ::  mfa             !   Aging mass flux         [kg/(m**2*s)]
                real(rp)                    ::  mfd             !   Deposition mass flux    [kg/(m**2*s)]
                real(rp)                    ::  mfg             !   Grow mass flux          [kg/(m**2*s)]
                real(rp)                    ::  mfs             !   Shear mass flux         [kg/(m**2*s)]
            end type
        
        !   Data structure for temperature gradient
            type                            ::  tpg_type
                real(rp)                    ::  dtr             !   Radial temp gradient    [K/m]
                real(rp)                    ::  dtx             !   Axial temp gradient     [K/m]
                real(rp)                    ::  htc             !   Convective coefficient  [W/(m**2*K)]
                real(rp)                    ::  qfl             !   Heat                    [W]
                real(rp)                    ::  thf             !   Fluid thermal resistance[K*m/W]
                real(rp)                    ::  thw             !   Wax thermal resistance  [K*m/W]
            end type
        ! ---------------------------------------------------------------------------------------
        
        !   Data structure for pipe wax properties
            type                            ::  ppw_type
                real(rp),allocatable        ::  di(:)           !   Inner diameter          [m]
                real(rp),allocatable        ::  fw(:)           !   Wax weigth fraction     [-]
                real(rp),allocatable        ::  th(:)           !   Wax layer thickness     [m]
            end type
        
        !   Data-type structure for pipeline
            type                            ::  pp_type
                type(sec_type),allocatable  ::  sec(:)          !   Pipe section data-type   
             
                type(f2_type),allocatable   ::  liq(:)          !   Liq phase data-type
             
                type(prg_type),allocatable  ::  prg(:)          !   Pressure gradient data-type
                type(tpg_type),allocatable  ::  tpg(:)          !   Temperature gradient data-type
            end type
        ! ---------------------------------------------------------------------------------------
        
        !	Module subroutines and functions
            contains

            pure function         C_inf_num ( &
                inf_num )

                !	Input variables
                type(inf_type),intent(in)   ::  inf_num         !   Report data type

                !	Output variables
                type(inf_Ctyp)              ::  C_inf_num       !   Report data type

! ***********************************************************************************

!   inf_type transfer inf_Ctyp
C_inf_num = inf_Ctyp ( inf_num%er, inf_num%it, trim(inf_num%nm) // C_null_char )

! ***********************************************************************************
end function
        ! ***************************************************************************************
        
            subroutine              LOG_close ( &
                                        unit_log )

        
            !	Input variables
                integer(ip),intent(in)      ::  unit_log        !   Log file unit           [-]
        
            !   Internal variables
                integer(ip)                 ::	iost            !   File I/O status         [-]
        
            ! ***********************************************************************************
        
            !   Log file closing
                close (unit = unit_log, status = 'keep', iostat = iost)
        
            ! ***********************************************************************************
            end subroutine
        
        ! ***************************************************************************************
        
            subroutine              LOG_in ( &
                                        unit_log, message )
 
        
            !	Input variables
                integer(ip),intent(in)      ::  unit_log        !   Log file unit           [-]
                character(*),intent(in)     ::	message         !	Message                 [-]
                
            !   Internal variables
                integer(ip)                 ::	iost            !   File I/O status         [-]
        
            ! ***********************************************************************************
        
            !   Log file writing
                write (unit = unit_log, fmt = *, iostat = iost) message
        
            ! ***********************************************************************************
            end subroutine
        
        ! ***************************************************************************************
        
            subroutine              LOG_open ( &
                                        file_log, unit_log )
        
            !	Input variables
                integer(ip),intent(in)      ::  unit_log        !   Log file unit           [-]
                character(*),intent(in)     ::	file_log        !	Log files name          [-]
        
            !	Internal variables
                integer                     ::	iost            !   Log file I/O status     [-]
        
            ! ***********************************************************************************
        
            !   Log file opening
                open (unit = unit_log, file = file_log, status = 'unknown', iostat = iost)
        
                if (iost == 0) then
        
            !       Log file header
                    call LOG_in (unit_log, '* * ********************************************* * *')
                    call LOG_in (unit_log, '* * TULSA UNIVERSITY PARAFFIN DEPOSITION PROJECTS * *')
                    call LOG_in (unit_log, '* *                    TU WaxPro                  * *')
                    call LOG_in (unit_log, '* *                   Version  1.2                * *')
                    call LOG_in (unit_log, '* *                     Log file                  * *')
                    call LOG_in (unit_log, '* * ********************************************* * *')                                       
                else
        
            !       Fort20 error file
                    close (unit = unit_log)
                    write (20,*) 'Log file error !'
                end if
        
            ! ***********************************************************************************
            end subroutine
        
        ! ***************************************************************************************
        end module
module Properties

    
    !   Used modules
        use                 ::  constants
    
    !   Variables must be declared
        implicit none
    
    !   Private subroutines and variables
        private
    
    !   Module public subroutines
     
        public              ::  PRO_enh
        public              ::  PRO_fluids
        public              ::  PRO_fluids_w
        public              ::  PRO_t
        public              ::  PRO_wax
    
    !	Module subroutines and functions
        contains
    

    
    ! ***************************************************************************************
    ! Define the PRO_enh subroutine for calculating the mixture enthalpy
        pure subroutine         PRO_enh ( &
                                    pvt, pr, tp, &
                                    enh, inf )

        !   Called subroutines
            use Numerics,only           :   Iblnr_2d
            use Numerics,only           :   Search_1d
    
        !	Input variables
            real(rp),intent(in)         ::  pr              !   Pressure                [Pa]
            real(rp),intent(in)         ::  tp              !   Temperature             [K]
            type(pvt_type),intent(in)   ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]
                                        !   gas                 Gas properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Mass fraction           [-]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   gas%con             Conductivity            [W/(m*K)]
                                        !   gas%enh             Enthalpy                [J/kg]
                                        !   gas%epc             Expansion coefficient   [1/K]
                                        !   gas%scp             Specific heat           [J/(kg*K)]
                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
    
        !   Output variables
            real(rp),intent(out)        ::  enh             !   Total enthalpy          [J/kg]
            type(inf_type),intent(out)  ::	inf             !   Numerical report data-type
                                        !   er                  Error flag              [-]
                                        !   it                  Number of iterations    [-]
                                        !   nm                  Error name              [-]
                                        !                       'p_<_ptab(1)'
                                        !                       'p_>_ptab(npt)'
                                        !                       'p_<_ttab(1)'
                                        !                       't_>_ttab(ntt)'
    
        !   Internal variables
            integer(ip)                 ::  i               !   pr array location       [-]
            integer(ip)                 ::  j               !   tr array location       [-]
            real(rp)                    ::  alpha           !   Weight coeficient       [-]
            real(rp)                    ::  beta            !   Weight coeficient       [-]
    
        ! ***********************************************************************************
    
        !   Numerical report data-type initialization 
            inf = inf_type ( .false., 1, no_error )
    
        !   Pressure boundaries
            if (pr < pvt%pr(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p < p(1)' )
    
        !       Abort PRO_enh subroutine
                return
            end if
            if (pr > pvt%pr(pvt%npt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p > p(npt)' )
    
        !       Abort PRO_enh subroutine
                return
            end if
    
        !   Temperature boundaries
            if (tp < pvt%tp(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't < t(1)' )
    
        !       Abort PRO_enh subroutine
                return
            end if
            if (tp > pvt%tp(pvt%ntt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't > t(ntt)' )
    
        !       Abort PRO_enh subroutine
                return
            end if
    
        !   Index location
            i = Search_1d ( pvt%pr, pr, pvt%npt )
            j = Search_1d ( pvt%tp, tp, pvt%ntt )
    
        !   Upper or lower aproximation
            alpha = (pr - pvt%pr(i)) / (pvt%pr(i+1) - pvt%pr(i))
            beta = (tp - pvt%tp(j)) / (pvt%tp(j+1) - pvt%tp(j))
    
        !   Total enthalpy
            call Iblnr_2d ( &
                alpha, beta, i, j, pvt%enh, enh )
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    ! Define the PRO_fluids subroutine for calculating the wax fraction and viscosity in the bulk 
          subroutine         PRO_fluids ( &
                                    xmf, dia, flow, pr, tp, pvt, &
                                    prg_fri, wax_cfr, thm_vis, &
                                     liq, inf )

            
        !   Called subroutines
            use Numerics,only           :   Iblnr_2d
            use Numerics,only           :   Search_1d
    
        !	Input variables
            logical,intent(in)          ::  xmf             !   Mass or volumetric flow [-]
            real(rp),intent(in)         ::  dia             !   Inner diameter          [m]
            real(rp),intent(in)         ::  flow            !   Flow rate               [kg/s, m**3/s]
            real(rp),intent(in)         ::  pr              !   Pressure                [Pa]
            real(rp),intent(in)         ::  tp              !   Temperature             [K]
            real(rp),intent(in)         ::  prg_fri         !   Fri pressure gradient   [Pa/m]
            real(rp),intent(in)         ::  wax_cfr         !   Wax concentration frac  [mol/mol]
            real(rp),intent(in)         ::  thm_vis(3)      !   Oil-wax vis multipliers [-]
            type(pvt_type),intent(in)   ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]
                                        !   gas                 Gas properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Mass fraction           [-]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   gas%con             Conductivity            [W/(m*K)]
                                        !   gas%enh             Enthalpy                [J/kg]
                                        !   gas%epc             Expansion coefficient   [1/K]
                                        !   gas%scp             Specific heat           [J/(kg*K)]
                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
    
        !   Output variables
 
            type(f2_type),intent(out)   ::  liq             !   Liq phase fluid data
                                        !	liq%den             Density                 [kg/m^3]
                                        !	liq%stn             Mass fraction           [-]
                                        !	liq%vel             Superficial velocity    [m/s]
                                        !	liq%vis             Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
            type(inf_type),intent(out)  ::	inf             !   Numerical report data-type
                                        !   er                  Error flag              [-]
                                        !   it                  Number of iterations    [-]
                                        !   nm                  Error name              [-]
                                        !                       'p_<_p(1)'
                                        !                       'p_>_p(npt)'
                                        !                       'p_<_t(1)'
                                        !                       't_>_t(ntt)'
    
        !   Internal variables
            integer(ip)                 ::  i               !   pr array location       [-]
            integer(ip)                 ::  j               !   tp array location       [-]
            real(rp)                    ::  area            !   Section area            [m**2]
            real(rp)                    :: t_c              !   temperature in C
        ! ***********************************************************************************
        
        t_c = tp - 273.0
        

        !   Numerical report data-type initialization 
            inf = inf_type ( .false., 1, no_error )
           

            
        !   Pressure boundaries
            if (pr < pvt%pr(1)) then
                
        !       Numerical report
                inf = inf_type ( .true., 1, 'p < p(1)' )
    
        !       Abort PRO_fluids subroutine
                return
            end if

            

            if (pr > pvt%pr(pvt%npt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p > p(npt)' )
    
        !       Abort PRO_fluids subroutine
                return
            end if

            
        !   Temperature boundaries
            if (tp < pvt%tp(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't < t(1)' )
    
        !       Abort PRO_fluids subroutine
                return
            end if
            if (tp > pvt%tp(pvt%ntt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't > t(ntt)' )
    
        !       Abort PRO_fluids subroutine
                return
            end if
    
        !   Index location
            i = Search_1d ( pvt%pr, pr, pvt%npt )
            j = Search_1d ( pvt%tp, tp, pvt%ntt )
    
           
    
        !   Liq properties
            liq = PRO_fluid_i1 (pvt%pr, pvt%tp, i, j, pvt%liq, pr, tp)
            !liq%den = (-0.000693*t_c+0.0831)*10000.0
            !liq%vis = -4.0*(10.0**(-10.0))*(t_c**6.0)+(8.0*(10.0**(-8.0)))*(t_c**5.0)&
            ! - 0.000007*(t_c**4.0)&
            ! + 0.0003*(t_c**3.0) - 0.0073*(t_c**2.0) + 0.0814*t_c - 0.2905
        !   Inner pipe area
            area = ap * dia ** 2
    
        !   Superficial velocities
            if (xmf) then
    
        !       Mass flow rate
                ! gas%vel = flow * gas%stn / (area * gas%den)

        ! change here
                liq%vel = flow / (area * liq%den)

                print*, "flow", flow,"area",area,"liq%den",liq%den

              !  liq%vel = flow / (area * 807.0)

            else
    
        !       Volumetric flow rate
                ! gas%vel = flow * gas%stn * liq%den / (area * &
                !           (gas%den * (one - gas%stn) + liq%den * gas%stn))
                liq%vel = flow/area
            end if
    
        !   Wax-oil non-Newtonian viscosity correction
            if (thm_vis(1) /= zero) then
    
                liq%vis = liq%vis * (exp(thm_vis(1) * wax_cfr) + wax_cfr * &
                    (thm_vis(2) / sqrt(abs(prg_fri * dia / liq%vis)) + &
                    thm_vis(3) * wax_cfr ** 3 / abs(prg_fri * dia / liq%vis)))
            end if
    
        !	Internal fuctions or subroutines
            contains
    
        ! ***********************************************************************************
    
            pure function       PRO_fluid_i1 ( pvt_pr, pvt_tp, i, j, pvt_f, pr, tp )
            ! *******************************************************************************
            !                   * *  TULSA UNIVERSITY PARAFFIN DEPOSITION PROJECTS * *
            ! **  FUNCTION:		PRO_fluid_i1
            ! **  VERSION:		2.1 Fortran 2003
            ! **  DESCRIPTION:	Calculates fluid properties at (p,t).
            ! *******************************************************************************
            !   Input variable
                integer(ip),intent(in)      ::	i, j		!	Counters                [-]
                real(rp),intent(in)         ::  pr          !   Pressure                [Pa]
                real(rp),intent(in)         ::  tp          !   Temperature             [K]
                real(rp),intent(in)         ::  pvt_pr(:)   !   PVT pressure array      [Pa]
                real(rp),intent(in)         ::  pvt_tp(:)   !   PVT temperature array   [K]
                type(f2_type),intent(in)    ::  pvt_f(:,:)  !   Fluid data-type
                                            !	den             Density                 [kg/m^3]
                                            !	stn             Surface tension         [N/m]
                                            !	vel             Superficial velocity    [m/s]
                                            !	vis             Viscosity               [Pa*s]
                                            !   con             Conductivity            [W/(m*K)]
                                            !   enh             Enthalpy                [J/kg]
                                            !   epc             Expansion coefficient   [1/K]
                                            !   scp             Specific heat           [J/(kg*K)]
            !   Output variables
                type(f2_type)               ::  PRO_fluid_i1!   Fluid data-type
                                            !	den             Density                 [kg/m^3]
                                            !	stn             Surface tension         [N/m]
                                            !	vel             Superficial velocity    [m/s]
                                            !	vis             Viscosity               [Pa*s]
                                            !   con             Conductivity            [W/(m*K)]
                                            !   enh             Enthalpy                [J/kg]
                                            !   epc             Expansion coefficient   [1/K]
                                            !   scp             Specific heat           [J/(kg*K)]
            !   Internal variables
                real(rp)                    ::  alpha       !   Weight coeficient       [-]
                real(rp)                    ::  beta        !   Weight coeficient       [-]
            ! *******************************************************************************
    
            !   Upper or lower aproximation
                alpha = (pr - pvt_pr(i)) / (pvt_pr(i+1) - pvt_pr(i))
                beta = (tp - pvt_tp(j)) / (pvt_tp(j+1) - pvt_tp(j))
    
            !   Fluid density
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%den, PRO_fluid_i1%den )
    
            !   Fluid enthalpy
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%enh, PRO_fluid_i1%enh )
    
            !   Fluid mass fraction of surface tension
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%stn, PRO_fluid_i1%stn )
    
            !   Fluid specific heat
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%scp, PRO_fluid_i1%scp )
    
            !   Fluid thermal expansion coeficient
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%epc, PRO_fluid_i1%epc )
    
            !   Fluid thermal conductivity
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%con, PRO_fluid_i1%con )
    
            !   Fluid viscosity
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%vis, PRO_fluid_i1%vis )
    
            ! *******************************************************************************
            end function
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    ! Define the PRO_fluids_w subroutine for calculating the wax fraction and viscosity at the wall
        pure subroutine         PRO_fluids_w ( &
                                    dia, pr, tp, pvt, &
                                    prg_fri, wax_cfr, thm_vis, &
                                     liq, inf )

    
        !   Called subroutines
            use Numerics,only           :   Iblnr_2d
            use Numerics,only           :   Search_1d
    
        !	Input variables
            real(rp),intent(in)         ::  dia             !   Inner diameter          [m]
            real(rp),intent(in)         ::  pr              !   Pressure                [Pa]
            real(rp),intent(in)         ::  tp              !   Wall temperature        [K]
            real(rp),intent(in)         ::  prg_fri         !   Fri pressure gradient   [Pa/m]
            real(rp),intent(in)         ::  wax_cfr         !   Wax concentration frac  [mol/mol]
            real(rp),intent(in)         ::  thm_vis(3)      !   Oil-wax vis multipliers [-]
            type(pvt_type),intent(in)   ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]

                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Mass fraction           [-]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]

                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
    
  
            type(f3_type),intent(out)   ::  liq             !   Liq phase fluid data
                                        !	liq%den             Density                 [kg/m^3]
                                        !	liq%stn             Mass fraction           [-]
                                        !	liq%vel             Superficial velocity    [m/s]
                                        !	liq%vis             Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
            type(inf_type),intent(out)  ::	inf             !   Numerical report data-type
                                        !   er                  Error flag              [-]
                                        !   it                  Number of iterations    [-]
                                        !   nm                  Error name              [-]
                                        !                       'p_<_p(1)'
                                        !                       'p_>_p(npt)'
                                        !                       'p_<_t(1)'
                                        !                       't_>_t(ntt)'
    
        !   Internal variables
            integer(ip)                 ::  i               !   pr array location       [-]
            integer(ip)                 ::  j               !   tp array location       [-]
            real(rp)                    ::  t_c             !   temperature in celsius  [C]
        ! ***********************************************************************************
        t_c = tp - 273.0
        !   Numerical report data-type initialization  
            inf = inf_type ( .false., 1, no_error )
    
        !   Pressure boundaries
            if (pr < pvt%pr(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p < p(1)' )
    
        !       Abort PRO_fluids_w subroutine
                return
            end if
            if (pr > pvt%pr(pvt%npt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p > p(npt)' )
    
        !       Abort PRO_fluids_w subroutine
                return
            end if
    
        !   Temperature boundaries
            if (tp < pvt%tp(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't < t(1)' )
    
        !       Abort PRO_fluids_w subroutine
                return
            end if
            if (tp > pvt%tp(pvt%ntt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't > t(ntt)' )
    
        !       Abort PRO_fluids_w subroutine
                return
            end if
    
        !   Index location
            i = Search_1d ( pvt%pr, pr, pvt%npt )
            j = Search_1d ( pvt%tp, tp, pvt%ntt )
    
        ! !   Gas properties
        !     gas = PRO_fluid_i2 ( pvt%pr, pvt%tp, i, j, pvt%gas, pr, tp )
    
        !   Liq properties
            liq = PRO_fluid_i2 ( pvt%pr, pvt%tp, i, j, pvt%liq, pr, tp )
            !liq%den = (-0.000693*t_c+0.0831)*10000.0
            !liq%vis = -4.0*(10.0**(-10.0))*(t_c**6.0)+(8.0*(10.0**(-8.0)))*(t_c**5.0)&
            ! - 0.000007*(t_c**4.0)&
            ! + 0.0003*(t_c**3.0) - 0.0073*(t_c**2.0) + 0.0814*t_c - 0.2905
        !   Wax-oil non-Newtonian viscosity correction
            if (thm_vis(1) /= zero) then
    
                liq%vis = liq%vis * (exp(thm_vis(1) * wax_cfr) + wax_cfr * &
                    (thm_vis(2) / sqrt(abs(prg_fri * dia / liq%vis)) + &
                    thm_vis(3) * wax_cfr ** 3 / abs(prg_fri * dia / liq%vis)))
            end if
    
        !	Internal fuctions or subroutines
            contains
    
        ! ***********************************************************************************
    
            pure function       PRO_fluid_i2 ( pvt_pr, pvt_tp, i, j, pvt_f, pr, tp )
            ! *******************************************************************************
            !                       * *  TULSA UNIVERSITY PARAFFIN DEPOSITION PROJECTS * *
            ! **  FUNCTION:		PRO_fluid_i2
            ! **  VERSION:		2.1 Fortran 2003
            ! **  DESCRIPTION:	Calculates fluid properties at (p,t).
            ! *******************************************************************************
            !   Input variable
                integer(ip),intent(in)      ::	i, j		!	Counters                [-]
                real(rp),intent(in)         ::  pr          !   Pressure                [Pa]
                real(rp),intent(in)         ::  tp          !   Wall temperature        [K]
                real(rp),intent(in)         ::  pvt_pr(:)   !   PVT pressure array      [Pa]
                real(rp),intent(in)         ::  pvt_tp(:)   !   PVT temperature array   [K]
                type(f2_type),intent(in)    ::  pvt_f(:,:)  !   Fluid data-type
                                            !   con             Conductivity            [W/(m*K)]
                                            !   den             Density                 [kg/m^3]
                                            !   epc             Expansion coefficient   [1/K]
                                            !   scp             Specific heat           [J/(kg*K)]
                                            !   vis             Viscosity               [Pa*s]
            !   Output variables
                type(f3_type)               ::  PRO_fluid_i2!   Wall fluid data-type
                                            !   con             Conductivity            [W/(m*K)]
                                            !   den             Density                 [kg/m^3]
                                            !   epc             Expansion coefficient   [1/K]
                                            !   scp             Specific heat           [J/(kg*K)]
                                            !   vis             Viscosity               [Pa*s]
            !   Internal variables
                real(rp)                    ::  alpha       !   Weight coeficient       [-]
                real(rp)                    ::  beta        !   Weight coeficient       [-]
            ! *******************************************************************************
    
            !   Upper or lower aproximation
                alpha = (pr - pvt_pr(i)) / (pvt_pr(i+1) - pvt_pr(i))
                beta = (tp - pvt_tp(j)) / (pvt_tp(j+1) - pvt_tp(j))
    
            !   Fluid density
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%den, PRO_fluid_i2%den )
    
            !   Fluid specific heat 
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%scp, PRO_fluid_i2%scp )
    
            !   Fluid thermal expansion coefficient
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%epc, PRO_fluid_i2%epc )
    
            !   Fluid thermal conductivity
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%con, PRO_fluid_i2%con )
    
            !   Fluid viscosity
                call Iblnr_2d ( &
                    alpha, beta, i, j, pvt_f%vis, PRO_fluid_i2%vis )
    
            ! *******************************************************************************
            end function
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    ! Define the PRO_t subroutine to calculate the temperature of the mixture 
        pure subroutine         PRO_t ( &
                                    enh, pr, pvt, &
                                    tp, inf )
 
        !   Called subroutines
            use Numerics,only           :   Iblnr_1d
            use Numerics,only           :   Search_1d
    
        !	Input variables
            real(rp),intent(in)         ::  enh             !   Total enthalpy          [J/kg]
            real(rp),intent(in)         ::  pr              !   Pressure                [Pa]
            type(pvt_type),intent(in)   ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]
                                        !   gas                 Gas properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Mass fraction           [-]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]

                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
    
        !   Output variables
            real(rp),intent(out)        ::  tp              !   Temperature             [K]
            type(inf_type),intent(out)  ::	inf             !   Numerical report data-type
                                        !   er                  Error flag              [-]
                                        !   it                  Number of iterations    [-]
                                        !   nm                  Error name              [-]
                                        !                       'p_<_p(1)'
                                        !                       'p_>_p(npt)'
                                        !                       'enh_<_enh(1)'
                                        !                       'enh_>_enh(ntt)'
    
        !   Internal variables
            integer(ip)                 ::  i               !   pr array location       [-]
            integer(ip)                 ::  j               !   tp array location       [-]
            real(rp)                    ::  enh_t(pvt%ntt)  !   Total enthalpy array    [-]
    
        ! ***********************************************************************************
    
        !   Numerical report data-type initialization  
            inf = inf_type ( .false., 1, no_error )
    
        !   Pressure boundaries
            if (pr < pvt%pr(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p < p(1)' )
    
        !       Abort PRO_t subroutine
                return
            end if
            if (pr > pvt%pr(pvt%npt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p > p(npt)' )
    
        !       Abort PRO_t subroutine
                return
            end if
    
        !   Index location
            i = Search_1d ( pvt%pr, pr, pvt%npt )
    
            do j = 1, pvt%ntt
    
        !       Total enthalpy
                call Iblnr_1d ( &
                    pvt%pr, i, pvt%enh(:,j), pr, enh_t(j) )
            end do
    
        !   Enthalpy boundaries
            if (enh < enh_t(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'enh < enh(1)' )
    
        !       Abort PRO_t subroutine
                return
            end if
            if (enh > enh_t(pvt%ntt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'enh > enh(ntt)' )
    
        !       Abort PRO_t subroutine
                return
            end if
    
        !   Index location
            j = Search_1d ( enh_t, enh, pvt%ntt )
    
        !   Temperature linear interpolation
            call Iblnr_1d ( &
                enh_t, j, pvt%tp, enh, tp )
    
        ! ***********************************************************************************
        end subroutine
    
    
    ! ***************************************************************************************
    ! Define the PRO_wax subroutine to calculate the wax properties

        ! change here make pure
        subroutine         PRO_wax ( &
                                    pr, tp, thm, &
                                    wax, inf )

        
        !   Called subroutines
            use Numerics,only           :   Iblnr_1d
            use Numerics,only           :   Iblnr_2d
            use Numerics,only           :   Search_1d
    
        !	Input variables
            real(rp),intent(in)         ::  pr              !   Pressure                [Pa]
            real(rp),intent(in)         ::  tp              !   Temperature             [K]
            type(thm_type),intent(in)   ::  thm             !   THM data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   vis                 Oil-wax vis multipliers [-]
                                        !   pr                  Pressure                [Pa]
                                        !   tp                  Temperature             [K]
                                        !   tpc                 Cloud temperature       [K]
                                        !   wax                 Wax properties data-type
                                        !   wax%den             Density                 [kg/m^3]
                                        !   wax%cfr             Wax concentration frac  [mol/mol]
                                        !   wax%enh             Enthalpy                [J/kg]
                                        !   wax%mwl             Liq molecular weigth    [kg/kmol]
                                        !   wax%mww             Wax molecular weigth    [kg/kmol]
                                        !   wax%scp             Specific heat           [J/(kg*K)]
                                        !   wax%wfr             Wax fraction            [mol/mol]
                                        !   wax%dwf             Wax fraction d.w.r. tp  [mol/(mol*K)]
    
        !   Output variables
            type(wax_type),intent(out)  ::  wax             !   Wax pproperties data-type
                                        !   wax%den             Density                 [kg/m^3]
                                        !   wax%cfr             Wax concentration frac  [mol/mol]
                                        !   wax%enh             Enthalpy                [J/kg]
                                        !   wax%mwl             Liq molecular weigth    [kg/kmol]
                                        !   wax%mww             Wax molecular weigth    [kg/kmol]
                                        !   wax%scp             Specific heat           [J/(kg*K)]
                                        !   wax%wfr             Wax fraction            [mol/mol]
                                        !   wax%dwf             Wax fraction d.w.r. tp  [mol/(mol*K)]
            type(inf_type),intent(out)  ::	inf             !   Numerical report data-type
                                        !   er                  Error flag              [-]
                                        !   it                  Number of iterations    [-]
                                        !   nm                  Error name              [-]
                                        !                       'p_<_p(1)'
                                        !                       'p_>_p(npt)'
                                        !                       'p_<_t(1)'
                                        !                       't_>_t(ntt)'
    
        !   Internal variables
            integer(ip)                 ::  i               !   pr array location       [-]
            integer(ip)                 ::  j               !   tp array location       [-]
            real(rp)                    ::  alpha           !   Weight coeficient       [-]
            real(rp)                    ::  beta            !   Weight coeficient       [-]
    
        ! ***********************************************************************************
    
        !   Numerical report data-type initialization  
            inf = inf_type ( .false., 1, no_error )
    
        !   Pressure boundaries
            if (pr < thm%pr(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p < p(1)' )
    
        !       Abort PRO_wax subroutine
                return
            end if
            if (pr > thm%pr(thm%npt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 'p > p(npt)' )
    
        !       Abort PRO_wax subroutine
                return
            end if
    
        !   Temperature boundaries
            if (tp < thm%tp(1)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't < t(1)' )
    
        !       Abort PRO_wax subroutine
                return
            end if
            if (tp > thm%tp(thm%ntt)) then
    
        !       Numerical report
                inf = inf_type ( .true., 1, 't > t(ntt)' )
    
        !       Abort PRO_wax subroutine
                return
            end if
    
        !   Index location
            i = Search_1d ( thm%pr, pr, thm%npt )
            j = Search_1d ( thm%tp, tp, thm%ntt )
    
        !   Upper or lower aproximation
            alpha = (pr - thm%pr(i)) / (thm%pr(i+1) - thm%pr(i))
            beta = (tp - thm%tp(j)) / (thm%tp(j+1) - thm%tp(j))
    
        !   Wax density
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%den, wax%den )
    
        !   Wax concentration fraction
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%cfr, wax%cfr )


    
        !   Wax enthalpy
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%enh, wax%enh )
    
        !   Liq molecular weigth
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%mwl, wax%mwl )
    
        !   Wax molecular weigth
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%mww, wax%mww )
    
        !   Wax specific heat
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%scp, wax%scp )
    
        !   Wax solid fraction
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%wfr, wax%wfr )
    
        !   Wax solid fraction d.w.r. tp 
            call Iblnr_2d ( &
                alpha, beta, i, j, thm%wax%dwf, wax%dwf )


                ! pulled from the DSC curve for the oil.. fitted with a polynomial

            wax%wfr = 1.5 * ((7.79096*(10.0**-8.0)*((tp-273.15)**4.0))+(-7.161*(10.0**-6.0)*((tp-273.15)**3.0)) &
            
                + (0.00016485*((tp-273.15)**2.0)) + (-0.001749*((tp-273.15)**1.0)) + 0.08196859)

            print*, "wax temperature", (tp-273.15)
            print*, "wax fraction", wax%wfr

            wax%cfr = 1.5 * ((-1.05116*(10.0**-7.0)*((tp-273.15)**4.0)) + (9.9132*(10.0**-6.0)*((tp-273.15)**3.0)) &
            
                + (-0.0002409*((tp-273.15)**2.0)) + (0.00172366*((tp-273.15)**1.0)) -0.0011797)

            wax%dwf = 1.5 * ((3.10702*(10.0**-7.0)*((tp-273.15)**3.0)) + (-2.2290*(10.0**-5.0)*((tp-273.15)**2.0)) + &

                 (0.00036983*((tp-273.15)**1.0))-0.002067)


            !wax%wfr = (3.0*(10.0**-6.0)*((tp-273.15)**3.0))-(3.0*(10.0**-5.0)*((tp-273.15)**2.0))+(0.0088*(tp-273.15))-0.0017

            !print*, "wax temperature", (tp-273.15)
            !print*, "wax fraction", wax%wfr

            !wax%cfr = -(3.0*(10.0**-6.0)*((tp-273.15)**3.0))+(3.0*(10.0**-4.0)*((tp-273.15)**2.0))-(0.0088*(tp-273.15))+0.0707

            !wax%dwf = (1.0*(10.0**-5.0)*((tp-273.15)**2.0))-(7.0*(10.0**-4.0)*((tp-273.15)**1.0))+0.0103
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    end module
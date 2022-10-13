module Mass_transfer

    
    !   Used modules
        use                 ::  constants
    
    !   Variables must be declared
        implicit none
    
    !   Private subroutines and variables
        private
    
    !   Module public subroutines
        public              ::  MT_deposition 
    
    !	Module subroutines and functions
        contains
    
    ! ***************************************************************************************
 

        subroutine         MT_deposition ( &
                                    flag, xwd, xwm, c_finit, alfa, cdif, dt, old_di, old_fw, old_th, vk, &
                                     pr, tp, ti, gas_i,liq, liq_i, wax, wax_i, pvt, thm, &
                                    psp_dia, prg, tpg, &
                                    dfw, dth, mfa, mfd, mfg, mfs, fluxes, inf )
    

    
        !   Called subroutines
            use Properties,only         :   PRO_wax
    
        !	Input variables
            logical,intent(in)          ::  flag            !   Predictor/corrector flag[-]
            integer(ip),intent(in)		::	xwd(2)	        !	Wax diffusivity selector[-]
            integer(ip),intent(in)		::	xwm		        !	Wax model 
            real(rp),intent(in)         ::  c_finit         !   Initial wax fraction multiplier [-]
            real(rp),intent(in)	        ::  alfa            !	Aspect ratio            [-]
            real(rp),intent(in)         ::  cdif            !   Multiplier coefficient  [-]
            real(rp),intent(in)         ::  dt               !   Time step               [s]
            real(rp),intent(in)         ::  old_di          !   Wax pipe diameter       [m]
            real(rp),intent(in)	        ::  old_fw          !   Wax weigth fraction     [kg/kg]
            real(rp),intent(in)         ::  old_th          !   Wax layer thickness     [m]
            real(rp),intent(in)         ::  pr              !   Section pressure        [Pa]
            real(rp),intent(in)         ::  tp              !   Section temperature     [K]
            real(rp),intent(in)         ::  ti              !   Inner diameter temp     [K]
            real(rp),intent(in)         ::  psp_dia         !   Inner pipe diameter     [m]
            real(rp),intent(in)         ::  vk(2)           !   Venkatesan paramaters   [-]
            type(f3_type),intent(in)    ::  gas_i           !   Inner gas fluid-wall data-type
                                        !   gas_i%con           Conductivity            [W/(m*K)]
                                        !   gas_i%den           Density                 [kg/m^3]
                                        !   gas_i%epc           Expansion coefficient   [1/K]
                                        !   gas_i%scp           Specific heat           [J/(kg*K)]
                                        !	gas_i%vis           Viscosity               [Pa*s]
            type(f2_type),intent(in)    ::  liq             !   Bulk liq fluid data-type
                                        !	liq%den             Density                 [kg/m^3]
                                        !	liq%stn             Mass fraction           [-]
                                        !	liq%vel             Superficial velocity    [m/s]
                                        !	liq%vis             Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
            type(f3_type),intent(in)    ::  liq_i           !   Inner liq fluid-wall data-type
                                        !   liq_i%con           Conductivity            [W/(m*K)]
                                        !   liq_i%den           Density                 [kg/m^3]
                                        !   liq_i%epc           Expansion coefficient   [1/K]
                                        !   liq_i%scp           Specific heat           [J/(kg*K)]
                                        !	liq_i%vis           Viscosity               [Pa*s]
            type(prg_type),intent(in)   ::  prg             !   Pressure gradient data-type
                                        !   prg%fpc             Flow pattern code       [-]
                                        !   prg%hlg             Gas-liq holdup          [-]
                                        !   prg%hll             Liq-liq holdup          [-]
                                        !   prg%ace             Acc pressure gradient   [Pa/m]
                                        !   prg%fri             Fri pressure gradient   [Pa/m]
                                        !   prg%gra             Gra pressure gradient   [Pa/m]
                                        !   prg%tot             Tot pressure gradient   [Pa/m]
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
            type(tpg_type),intent(in)   ::  tpg             !   Temperature gradient data-type
                                        !   tpg%dtr             Radial temp gradient    [K/m]
                                        !   tpg%dtx             Axial temp gradient     [K/m]
                                        !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                                        !   tpg%qfl             Heat                    [W]
                                        !   tpg%thf             Fluid thermal resistance[K*m/W]
                                        !   tpg%thw             Wax thermal resistance  [K*m/W]
            type(wax_type),intent(in)   ::  wax             !   Wax properties data-type
                                        !   wax%den             Density                 [kg/m^3]
                                        !   wax%cfr             Wax concentration frac  [mol/mol]
                                        !   wax%enh             Enthalpy                [J/kg]
                                        !   wax%mwl             Liq molecular weigth    [kg/kmol]
                                        !   wax%mww             Wax molecular weigth    [kg/kmol]
                                        !   wax%scp             Specific heat           [J/(kg*K)]
                                        !   wax%wfr             Wax fraction            [mol/mol]
                                        !   wax%dwf             Wax fraction d.w.r. tp  [mol/(mol*K)]
            type(wax_type),intent(in)   ::  wax_i           !   Inner wax properties data-type
                                        !   wax_i%den           Density                 [kg/m^3]
                                        !   wax_i%cfr           Wax concentration frac  [mol/mol]
                                        !   wax_i%enh           Enthalpy                [J/kg]
                                        !   wax_i%mwl           Liq molecular weigth    [kg/kmol]
                                        !   wax_i%mww           Wax molecular weigth    [kg/kmol]
                                        !   wax_i%scp           Specific heat           [J/(kg*K)]
                                        !   wax_i%wfr           Wax fraction            [mol/mol]
                                        !   wax_i%dwf           Wax fraction d.w.r. tp  [mol/(mol*K)]
    
        !   Output variables
            real(rp),intent(out)        ::  dfw             !   Delta wax weigth frac   [kg/kg]
            real(rp),intent(out)        ::  dth             !   Delta wax layer thick   [m]
            real(rp),intent(out)        ::  mfa             !   Aging mass flux         [kg/(m**2*s)]
            real(rp),intent(out)        ::  mfd             !   Deposition mass flux    [kg/(m**2*s)]
            real(rp),intent(out)        ::  mfg             !   Grow mass flux          [kg/(m**2*s)]
            real(rp),intent(out)        ::  mfs             !   Shear mass flux         [kg/(m**2*s)]
            real(rp),intent(inout)      ::  fluxes(8)       !   ODE integration fluxes  [-]
            type(inf_type),intent(out)  ::  inf             !   Numerical report data-type
                                        !   inf%er              Error flag              [-]
                                        !   inf%it              Number of iterations    [-]
                                        !   inf%nm              Error name              [-]
    
        ! ***********************************************************************************
    
        !   Pipe section numerical initialization 
            inf = inf_type ( .false., 1, no_error )
    
        !   ODE system semi-implicit trapezoidal method
            if (flag) then
    
        !       Corrector evaluation
                call ODE_fun ( &
                    old_di, old_fw, old_th, &
                    fluxes(2), fluxes(4), mfa, mfd, mfg, mfs )
    
        !       Wax layer weigth fraction change
                dfw = p5 * dt * (fluxes(1) + fluxes(2))
    
        !       Wax layer growth
                dth = p5 * dt * (fluxes(3) + fluxes(4))
        !       Wax mass fluxes
                mfa = p5 * (mfa + fluxes(5))
                mfd = p5 * (mfd + fluxes(6))
                mfg = p5 * (mfg + fluxes(7))
                mfs = p5 * (mfs + fluxes(8))
            else
    
        !       Predictor evaluation
                call ODE_fun ( &
                    old_di, old_fw, old_th, &
                    fluxes(1), fluxes(3), fluxes(5), fluxes(6), fluxes(7), fluxes(8) )
    
        !       Wax layer weigth fraction change
                dfw = dt * fluxes(1)
    
        !       Wax layer growth
                dth = dt * fluxes(3)


            end if
            
    
        !	Internal functions or subroutines
            contains
    
        ! ***********************************************************************************
    
            subroutine     ODE_fun  ( di, fw, th, dfw, dth, mfa, mfd, mfg, mfs )
            ! *******************************************************************************
            !                   * *  TULSA UNIVERSITY FLUID FLOW PROJECTS  * *
            ! **  SUBROUTINE:   ODE_fun
            ! **  VERSION:      2.1 Fortran 2003
            ! **  DESCRIPTION:  Mass transfer ODE system functions
            ! *******************************************************************************
            !	Input variables
                real(rp),intent(in)         ::  di          !   Wax pipe diameter       [m]
                real(rp),intent(in)	        ::  fw          !   Wax weigth fraction     [kg/kg]
                real(rp),intent(in)         ::  th          !   Wax layer thickness     [m]
            !	Output variables
                real(rp),intent(out)        ::  dfw         !   Weigth frac growth rate [1/s]
                real(rp),intent(out)        ::  dth         !   Layer growth rate       [m/s]
                real(rp),intent(out)        ::  mfa         !   Aging mass flux         [kg/(m**2*s)]
                real(rp),intent(out)        ::  mfd         !   Deposition mass flux    [kg/(m**2*s)]
                real(rp),intent(out)        ::  mfg         !   Grow mass flux          [kg/(m**2*s)]
                real(rp),intent(out)        ::  mfs         !   Shear mass flux         [kg/(m**2*s)]
            !	Internal variables
                real(rp)                    ::  den         !   Gel density             [kg/m**3]
                real(rp)                    ::  dwo         !   Wax in oil mol diffusion[m**2/s]
                real(rp)                    ::  fwm         !   Matzain wax weigth frac [kg/kg]
                real(rp)                    ::  pi3         !   Diffusion multiplier    [-]
                real(rp)                    ::  wvf         !   Weigth volume fraction  [-]
            ! *******************************************************************************
    
            !   Wax model selection
                select case (xwm)
            !		-------------------------------------------------------------------------
                    case (0)            !	Matzain model
    

                        print *, "matzain model"

            !           Matzain diffusion multiplier
                        call D_Matzain ( &
                            di, th, &
                            gas_i, liq, liq_i, prg, &
                            fwm, pi3 )
    
            !           Deposition mass flux (equilibrium model)
                        call MF_equi ( &
                            ti, xwd(1), cdif, liq_i, wax_i, tpg, &
                            mfd, dwo )
    
            !           Aging mass flux
                        mfa = zero
    
            !           Shear mass flux
                        mfs = (one - pi3) * mfd
            !		-------------------------------------------------------------------------
                    case (1)            !	Venkatesan model
    
            !           Deposition mass flux (equilibrium model)
                        call MF_equi ( &
                            ti, xwd(1), cdif,liq_i, wax_i, tpg, &
                            mfd, dwo )
    
            !           Aging mass flux (diffusion model)
                        call MF_aging ( &
                            xwd(2), alfa, dwo, fw, liq_i, wax_i, tpg, &
                            mfa )
    
            !           Shear mass flux
                        mfs = MF_shear ( di, th, vk, prg, liq_i )

                     
            !		-------------------------------------------------------------------------
                    case (2)            !	Singh model
    
                        print *, " Singh model"
            !           Deposition mass flux (fmt model)
                        call MF_film ( &
                            pr, p5*(tp+ti), &
                            xwd(1), di, cdif, prg%fri, wax, wax_i, pvt, thm,  tpg, &
                            mfd, dwo )

            !           Aging mass flux (diffusion model)
                        call MF_aging ( &
                            xwd(2), alfa, dwo, fw, liq_i, wax_i, tpg, &
                            mfa )
    
            !           Shear mass flux
                        mfs = MF_shear ( di, th, vk, prg, liq_i )

                        
            !		-------------------------------------------------------------------------
                end select
    
            !   Grow mass flux 
                mfg = mfd - (mfa + mfs)

                print*, "growth mass flux", mfg
            !   Grow mass flux test
                if(mfg < zero) then
    
            !       Wax layer growth
                    dth = zero
    
            !       Wax layer weigth fraction change
                    dfw = zero
                    return
                end if
     
            !   Wax model selection
                if (xwm == 0) then
    
            !       Weigth volume fraction
                    wvf = fwm * liq_i%den / ((one - fwm) * wax_i%den + fwm * liq_i%den)
    
            !       Gel density
                    den = (one - wvf) * liq_i%den + wvf * wax_i%den

                    print*, "wax density 2", den
    
            !       Wax layer growth

                    
                    dth = mfg / (den * fwm)
    
            !       Wax layer weigth fraction change
                    dfw = fwm - fw
                else
    
            !       Weigth volume fraction
                    wvf = fw * liq_i%den / ((one - fw) * wax_i%den + fw * liq_i%den)
    
            !       Gel density
                    den = (one - wvf) * liq_i%den + wvf * wax_i%den

                    print*, "wax density 3", den
    
            !       Wax layer growth
                    dth = mfg / (den * fw)

                    print*, "growth mf", mfg

                    print*, "dth wax fractin", fw

                    print*, "wax layer growth", dth

                    print*, "wax fraction 3", dfw
            !       Wax layer weigth fraction change
                    dfw = mfa * (psp_dia - two * th) / (den * th * (psp_dia - th))

                end if
    
            ! *******************************************************************************
            end subroutine
    
        ! ***********************************************************************************
    
        end subroutine
    
    ! ***************************************************************************************
    
        subroutine         MF_aging ( &
                                    xwd, alfa, dwo, fw, liq_i, wax_i, tpg, &
                                    mflux )

        !   Called subroutines
            use Heat_transfer,only      :   K_wax
    
        !	Input variables
            integer(ip),intent(in)		::	xwd		        !	Wax diffusivity selector[-]
            real(rp),intent(in)	        ::  alfa            !	Aspect ratio            [-]
            real(rp),intent(in)         ::  dwo		        !	Wax in oil mol diffusion[m**2/s]
            real(rp),intent(in)	        ::  fw              !   Wax weigth fraction     [-]
            type(f3_type),intent(in)    ::  liq_i           !   Inner liq fluid-wall data-type
                                        !   liq_i%con           Conductivity            [W/(m*K)]
                                        !   liq_i%den           Density                 [kg/m^3]
                                        !   liq_i%epc           Expansion coefficient   [1/K]
                                        !   liq_i%scp           Specific heat           [J/(kg*K)]
                                        !	liq_i%vis           Viscosity               [Pa*s]
            type(wax_type),intent(in)   ::  wax_i           !   Wax pproperties data-type
                                        !   wax_i%den           Density                 [kg/m^3]
                                        !   wax_i%cfr           Wax concentration frac  [mol/mol]
                                        !   wax_i%enh           Enthalpy                [J/kg]
                                        !   wax_i%mwl           Liq molecular weigth    [kg/kmol]
                                        !   wax_i%mww           Wax molecular weigth    [kg/kmol]
                                        !   wax_i%scp           Specific heat           [J/(kg*K)]
                                        !   wax_i%wfr           Wax fraction            [mol/mol]
                                        !   wax_i%dwf           Wax fraction d.w.r. tp  [mol/(mol*K)]
            type(tpg_type),intent(in)   ::  tpg             !   Temperature gradient data-type
                                        !   tpg%dtr             Radial temp gradient    [K/m]
                                        !   tpg%dtx             Axial temp gradient     [K/m]
                                        !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                                        !   tpg%qfl             Heat                    [W]
                                        !   tpg%thf             Fluid thermal resistance[K*m/W]
                                        !   tpg%thw             Wax thermal resistance  [K*m/W]
    
        !   Output variables
            real(rp),intent(out)        ::  mflux           !   Wax aging mass flux     [kg/(m**2*s)]
    
        !   Internal variables
            real(rp)                    ::  dif		        !	Efective diffusion      [m**2/s]
    
        ! ***********************************************************************************
    
        !   Effective diffusion
            dif = D_Cussler ( alfa, dwo, fw )
    
        !   Mass flux
            if (wax_i%dwf < zero) then
    
                mflux = liq_i%den * dif * liq_i%con * tpg%dtr * wax_i%dwf * wax_i%mww  / &
                    (wax_i%mwl * K_wax (fw, liq_i%con))

               ! mflux = liq_i%den * dif * tpg%dtr * wax_i%dwf 

                print*, "aging mass flux", mflux
            else
    
                mflux = zero
            end if
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    
          subroutine         MF_equi ( &
                                    ti, &
                                    xwd, cdif, liq_i, wax_i, tpg, &
                                    mflux, dwo )
    
        !	Input variables
            integer(ip),intent(in)		::	xwd		        !	Wax diffusivity selector[-]
            real(rp),intent(in)         ::  cdif            !   Multiplier coefficient  [-]
            real(rp),intent(in)         ::  ti              !   Inner diameter temp     [K]
            type(f3_type),intent(in)    ::  liq_i           !   Inner liq fluid-wall data-type
                                        !   liq_i%con           Conductivity            [W/(m*K)]
                                        !   liq_i%den           Density                 [kg/m^3]
                                        !   liq_i%epc           Expansion coefficient   [1/K]
                                        !   liq_i%scp           Specific heat           [J/(kg*K)]
                                        !	liq_i%vis           Viscosity               [Pa*s]
            type(wax_type),intent(in)   ::  wax_i           !   Inner wax properties data-type
                                        !   wax_i%den           Density                 [kg/m^3]
                                        !   wax_i%cfr           Wax concentration frac  [mol/mol]
                                        !   wax_i%enh           Enthalpy                [J/kg]
                                        !   wax_i%mwl           Liq molecular weigth    [kg/kmol]
                                        !   wax_i%mww           Wax molecular weigth    [kg/kmol]
                                        !   wax_i%scp           Specific heat           [J/(kg*K)]
                                        !   wax_i%wfr           Wax fraction            [mol/mol]
                                        !   wax_i%dwf           Wax fraction d.w.r. tp  [mol/(mol*K)]
            type(tpg_type),intent(in)   ::  tpg             !   Temperature gradient data-type
                                        !   tpg%dtr             Radial temp gradient    [K/m]
                                        !   tpg%dtx             Axial temp gradient     [K/m]
                                        !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                                        !   tpg%qfl             Heat                    [W]
                                        !   tpg%thf             Fluid thermal resistance[K*m/W]
                                        !   tpg%thw             Wax thermal resistance  [K*m/W]
    
        !   Output variables
            real(rp),intent(out)        ::  mflux           !   Wax deposition mass flux[kg/(m**2*s)]
            real(rp),intent(out)        ::  dwo		        !	Wax in oil mol diffusion[m**2/s]
    
        ! ***********************************************************************************
    
        !   Molecular diffusivity of wax in oil
            select case (xwd)
        !		-----------------------------------------------------------------------------
                case (0)    !   Hayduk & Minhas correlation
    
                    dwo = D_Hayduk_Minhas ( ti, wax_i%den, wax_i%mww, liq_i%vis, cdif )

                    print*, "dwo =", dwo
        !		-----------------------------------------------------------------------------
                case (1)    !   Wilke & Chang correlation
    
                    dwo = D_Wilke_Chang ( ti, wax_i%den, wax_i%mwl, wax_i%mww, liq_i%vis, cdif )
        !		-----------------------------------------------------------------------------
            end select
    
        !   Mass flux
            if (wax_i%dwf < zero) then
    
                mflux = liq_i%den * dwo * wax_i%dwf * tpg%dtr * wax_i%mww / wax_i%mwl

                print*, "wax mass flux", mflux

            else
    
                mflux = zero
            end if
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    
        subroutine         MF_film ( &
                                    pr, tb, &
                                    xwd, dia, cdif, prg_fri, wax, wax_i, pvt, thm, tpg, &
                                    mflux, dwo )
   
    
        !   Called subroutines
            use Properties,only         :   PRO_fluids
            use Properties,only         :   PRO_wax
    
        !	Input variables
            integer(ip),intent(in)		::	xwd		        !	Wax diffusivity selector[-]
            real(rp),intent(in)         ::  cdif            !   Multiplier coefficient  [-]
            real(rp),intent(in)         ::  dia             !   Inner pipe diameter     [m]
            real(rp),intent(in)         ::  pr              !   Section pressure        [Pa]
            real(rp),intent(in)         ::  tb              !   Boundary layer temp     [K]
            real(rp),intent(in)         ::  prg_fri         !   Fri pressure gradient   [Pa/m]
            type(wax_type),intent(in)   ::  wax             !   Bulk wax properties data-type
                                        !   wax%den             Density                 [kg/m^3]
                                        !   wax%cfr             Wax concentration frac  [mol/mol]
                                        !   wax%enh             Enthalpy                [J/kg]
                                        !   wax%mwl             Liq molecular weigth    [kg/kmol]
                                        !   wax%mww             Wax molecular weigth    [kg/kmol]
                                        !   wax%scp             Specific heat           [J/(kg*K)]
                                        !   wax%wfr             Wax fraction            [mol/mol]
                                        !   wax%dwf             Wax fraction d.w.r. tp  [mol/(mol*K)]
            type(wax_type),intent(in)	::  wax_i           !   Inner wax properties data-type
                                        !   wax_i%den           Density                 [kg/m^3]
                                        !   wax_i%cfr           Wax concentration frac  [mol/mol]
                                        !   wax_i%enh           Enthalpy                [J/kg]
                                        !   wax_i%mwl           Liq molecular weigth    [kg/kmol]
                                        !   wax_i%mww           Wax molecular weigth    [kg/kmol]
                                        !   wax_i%scp           Specific heat           [J/(kg*K)]
                                        !   wax_i%wfr           Wax fraction            [mol/mol]
                                        !   wax_i%dwf           Wax fraction d.w.r. tp  [mol/(mol*K)]
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
            type(tpg_type),intent(in)   ::  tpg             !   Temperature gradient data-type
                                        !   tpg%dtr             Radial temp gradient    [K/m]
                                        !   tpg%dtx             Axial temp gradient     [K/m]
                                        !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                                        !   tpg%qfl             Heat                    [W]
                                        !   tpg%thf             Fluid thermal resistance[K*m/W]
                                        !   tpg%thw             Wax thermal resistance  [K*m/W]
    
        !   Output variables
            real(rp),intent(out)        ::  mflux           !   Wax deposition mass flux[kg/(m**2*s)]
            real(rp),intent(out)        ::  dwo		        !	Wax in oil mol diffusion[m**2/s]
    
        !   Internal variables
            real(rp)                    ::  cb              !   Bulk concentration      []
            real(rp)                    ::  ci              !   Inner dia concentration []
            real(rp)                    ::  le              !   Lewis number            [-]
            type(f2_type)               ::  gas_l           !   Layer gas fluid-wall data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Mass fraction           [-]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   con                 Conductivity            [W/(m*K)]
                                        !   enh                 Enthalpy                [J/kg]
                                        !   epc                 Expansion coefficient   [1/K]
                                        !   scp                 Specific heat           [J/(kg*K)]
            type(f2_type)               ::  liq_l           !   Layer liq fluid-wall data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Mass fraction           [-]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   con                 Conductivity            [W/(m*K)]
                                        !   enh                 Enthalpy                [J/kg]
                                        !   epc                 Expansion coefficient   [1/K]
                                        !   scp                 Specific heat           [J/(kg*K)]
            type(inf_type)              ::	inf(2)          !   Numerical report data-type
            type(wax_type)              ::  wax_l           !   Layer wax properties data-type
                                        !   den                 Density                 [kg/m^3]
                                        !   cfr                 Wax concentration frac  [mol/mol]
                                        !   enh                 Enthalpy                [J/kg]
                                        !   mwl                 Liq molecular weigth    [kg/kmol]
                                        !   mww                 Wax molecular weigth    [kg/kmol]
                                        !   scp                 Specific heat           [J/(kg*K)]
                                        !   wfr                 Wax fraction            [mol/mol]
                                        !   dwf                 Wax fraction d.w.r. tp  [mol/(mol*K)]
    
        ! ***********************************************************************************
    
        !   Boundary layer THM properties
            call PRO_wax ( &
                pr, tb, thm, &
                wax_l, inf(1) )
    
        !   Boundary layer PVT properties
            call PRO_fluids ( &
                .true., dia, zero, pr, tb, pvt, &
                prg_fri, wax_l%cfr, thm%vis, &
                liq_l, inf(2) )
    
        !   Bulk concentration
            cb = wax%wfr / (one - wax%wfr)
            
        !   Inner diameter concentration
            ci = wax_i%wfr / (one - wax_i%wfr)
    
        !   Molecular diffusivity of wax in oil
            select case (xwd)
        !		-----------------------------------------------------------------------------
                case (0)    !   Hayduk & Minhas correlation
    
                    dwo = D_Hayduk_Minhas ( tb, wax_l%den, wax_l%mww, liq_l%vis, cdif )
                    
                    print*, "dwo =", dwo
        !		-----------------------------------------------------------------------------
                case (1)    !   Wilke & Chang correlation
    
                    dwo = D_Wilke_Chang ( tb, wax_l%den, wax_l%mwl, wax_l%mww, liq_l%vis, cdif )
        !		-----------------------------------------------------------------------------
            end select

        !   Lewis number
            
            le = liq_l%con / (liq_l%scp * liq_l%den * dwo)

           
        !   Mass flux
            if (wax_l%dwf < zero) then
    
                mflux = dwo * liq_l%den * tpg%htc * 0.876 *(ci - cb) * le ** p33 * wax_l%mww / &
                        (wax_l%mwl * liq_l%con)

                

                print*,"important numbers", dwo, liq_l%den, tpg%htc, ci, cb, le**p33, wax_l%mww, wax_l%mwl

                print*, "film mass flux", mflux
            else
    
                mflux = zero
            end if
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    
        pure function           MF_shear ( &
                                    dia, th, vk, &
                                    prg, liq_i )

        !	Input variables
            real(rp),intent(in)         ::  dia             !   Inner pipe diameter     [m]
            real(rp),intent(in)         ::  th              !   Wax layer thickness     [m]
            real(rp),intent(in)         ::  vk(2)           !   Venkatesan parameters   [-]
            type(f3_type),intent(in)    ::  liq_i           !   Inner liq fluid-wall data-type
                                        !   liq_i%con           Conductivity            [W/(m*K)]
                                        !   liq_i%den           Density                 [kg/m^3]
                                        !   liq_i%epc           Expansion coefficient   [1/K]
                                        !   liq_i%scp           Specific heat           [J/(kg*K)]
                                        !	liq_i%vis           Viscosity               [Pa*s]
            type(prg_type),intent(in)   ::  prg             !   Pressure gradient data-type
                                        !   prg%fpc             Flow pattern code       [-]
                                        !   prg%hlg             Gas-liq holdup          [-]
                                        !   prg%hll             Liq-liq holdup          [-]
                                        !   prg%ace             Acc pressure gradient   [Pa/m]
                                        !   prg%fri             Fri pressure gradient   [Pa/m]
                                        !   prg%gra             Gra pressure gradient   [Pa/m]
                                        !   prg%tot             Tot pressure gradient   [Pa/m]
    
    
        !   Output variables
            real(rp)                    ::  MF_shear        !   Wax shear mass flux     [kg/(m**2*s)]
    
        !   Internal variables
            real(rp)                    ::  stress          !   Bulk concentration      [Pa]
    
        ! ***********************************************************************************
    
        !   Shear stress
            stress = p25 * prg%fri * dia
    
        !   Shear mass flux
            MF_shear = vk(1) * stress ** vk(2) * liq_i%den / (pi * (dia - two * th))
    
        ! ***********************************************************************************
        end function
    
    ! ***************************************************************************************
    
        pure function           D_Cussler ( &
                                    alfa, dwo, fw )

    
        !	Input variables
            real(rp),intent(in)	        ::  alfa            !	Aspect ratio            [-]
            real(rp),intent(in)         ::  dwo		        !	Wax in oil mol diffusion[m**2/s]
            real(rp),intent(in)	        ::  fw              !   Wax weigth fraction     [-]
    
        !   Output variables
            real(rp)                    ::  D_Cussler       !	Wax in oil mol diffusion[m**2/s]
    
        ! ***********************************************************************************
    
        !   Molecular diffusivity
            D_Cussler = dwo / (one + alfa ** 2 * fw ** 2 / (one - fw))
    
        ! ***********************************************************************************
        end function
    
    ! ***************************************************************************************
    
        pure function           D_Hayduk_Minhas ( &
                                    tp, den, mww, vis, cdif )

        !	Input variables
            real(rp),intent(in)	        ::  tp		        !	Temperature             [K]
            real(rp),intent(in)	        ::  den             !   Wax density             [kg/m**3]
            real(rp),intent(in)	        ::  mww             !   Wax molecular weight    [kg/kmol]
            real(rp),intent(in)	        ::  vis             !	Oil viscosity           [Pa*s]
            real(rp),intent(in)         ::  cdif            !   Multiplier coefficient  [-]
    
        !   Output variables
            real(rp)                    ::  D_Hayduk_Minhas !	Wax in oil mol diffusion[m**2/s]
    
        !	Internal variables
            real(rp)                    ::  mvw             !   Wax molecular volume    [cm**3/mol]
    
        ! ***********************************************************************************
    
        !   Wax molecular volume
            mvw = 1.e3_rp * mww / den
    
        !   Molecular diffusivity of wax in oil
            D_Hayduk_Minhas = cdif * 13.2e-12_rp * tp ** 1.47_rp * &
                              (vis * 1.e3_rp) ** (10.2 / mvw - 0.791_rp) / mvw ** 0.71_rp
    
        ! ***********************************************************************************
        end function
    
    ! ***************************************************************************************
    
        pure subroutine         D_Matzain ( &
                                    dia, th, &
                                    gas_i, liq, liq_i, prg, &
                                    fw, pi3 )

    
        !   Parameters
            real(rp),parameter          ::  c1 = 15._rp
            real(rp),parameter          ::  c2 = 0.055_rp
            real(rp),parameter          ::  c3 = 1.4_rp
    
        !	Input variables
            real(rp),intent(in)         ::  dia             !   Inner pipe diameter     [m]
            real(rp),intent(in)         ::  th              !   Wax layer thickness     [m]
            type(f3_type),intent(in)    ::  gas_i           !   Inner gas fluid-wall data-type
                                        !   gas_i%con           Conductivity            [W/(m*K)]
                                        !   gas_i%den           Density                 [kg/m^3]
                                        !   gas_i%epc           Expansion coefficient   [1/K]
                                        !   gas_i%scp           Specific heat           [J/(kg*K)]
                                        !	gas_i%vis           Viscosity               [Pa*s]
            type(f2_type),intent(in)    ::  liq             !   Bulk liq fluid data-type
                                        !	liq%den             Density                 [kg/m^3]
                                        !	liq%stn             Mass fraction           [-]
                                        !	liq%vel             Superficial velocity    [m/s]
                                        !	liq%vis             Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]
            type(f3_type),intent(in)    ::  liq_i           !   Inner liq fluid-wall data-type
                                        !   liq_i%con           Conductivity            [W/(m*K)]
                                        !   liq_i%den           Density                 [kg/m^3]
                                        !   liq_i%epc           Expansion coefficient   [1/K]
                                        !   liq_i%scp           Specific heat           [J/(kg*K)]
                                        !	liq_i%vis           Viscosity               [Pa*s]]
            type(prg_type),intent(in)   ::  prg             !   Pressure gradient data-type
                                        !   prg%fpc             Flow pattern code       [-]
                                        !   prg%hlg             Gas-liq holdup          [-]
                                        !   prg%hll             Liq-liq holdup          [-]
                                        !   prg%ace             Acc pressure gradient   [Pa/m]
                                        !   prg%fri             Fri pressure gradient   [Pa/m]
                                        !   prg%gra             Gra pressure gradient   [Pa/m]
                                        !   prg%tot             Tot pressure gradient   [Pa/m]
    
        !   Output variables
            real(rp),intent(out)        ::  fw              !   Wax weigth fraction     [-]
            real(rp),intent(out)        ::  pi3             !   Diffusion multiplier    [-]
    
        !	Internal variables
            real(rp)                    ::  denm            !   Mix density             [kg/m**3]
            real(rp)                    ::  re              !   Reynolds number         [-]
            real(rp)                    ::  reth            !   Thick Reynolds number   [-]
    
        ! ***********************************************************************************
    
        !   Mixture density
            denm = gas_i%den * (one - prg%hlg) + liq_i%den * prg%hlg
    
        !   Flow pattern selection
            select case (prg%fpc)
        !		-----------------------------------------------------------------------------
                case (2, 3, 4)          !	Single-phase liq flow
    
        !           Thick Reynolds number
                    reth = liq_i%den * liq%vel * th / liq_i%vis
        !		-----------------------------------------------------------------------------
                case (10, 11, 12, 13)   !	Dispersed bubble flow, Bubble flow or slug flow
    
        !           Thick Reynolds number
                    reth = denm * liq%vel * th / (prg%hlg * liq_i%vis)
        !		-----------------------------------------------------------------------------
                case (14, 15)           !	Stratified flow
    
        !           Thick Reynolds number
                    reth = liq_i%den * liq%vel * th / (prg%hlg * liq_i%vis)
        !		-----------------------------------------------------------------------------
                case (16)               !	Annular flow
    
        !           Thick Reynolds number
                    reth = sqrt(liq_i%den * denm) * liq%vel * th / (prg%hlg * liq_i%vis)
        !		-----------------------------------------------------------------------------
                case default            !   Flow pattern not defined
    
        !           Thick Reynolds number
                    reth  = zero
        !		-----------------------------------------------------------------------------
            end select
    
        !   Reynolds number
            re = liq_i%den * liq%vel * dia / (prg%hlg * liq_i%vis)
        
        !   Wax layer content 
            fw  = 0.125_rp * re ** 0.15_rp
    
        !   Diffusion multiplier
            pi3 = c1 / (one + c2 * reth ** c3)
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    
        pure function           D_Wilke_Chang ( &
                                    tp, den, mwl, mww, vis, cdif )

    
        !	Input variables
            real(rp),intent(in)         ::  cdif            !   Multiplier coefficient  [-]
            real(rp),intent(in)	        ::  den             !   Wax density             [kg/m**3]
            real(rp),intent(in)         ::  mwl             !   Oil molecular weight    [kg/kmol]
            real(rp),intent(in)	        ::  mww             !   Wax molecular weight    [kg/kmol]
            real(rp),intent(in)	        ::  tp		        !	Temperature             [K]
            real(rp),intent(in)	        ::  vis             !	Oil viscosity           [Pa*s]
    
        !   Output variables
            real(rp)                    ::  D_Wilke_Chang   !	Wax in oil mol diffusion[m**2/s]
    
        !	Internal variables
            real(rp)                    ::  mvw             !   Wax molecular volume    [cm**3/mol]
    
        ! ***********************************************************************************
    
        !   Wax molecular volume
            mvw = 1.e3_rp * mww / den
    
        !   Molecular diffusivity of wax in oil
            D_Wilke_Chang  = cdif * 7.4e-15_rp * tp * sqrt(mwl) / (vis * mvw ** 0.6_rp)
    
        ! ***********************************************************************************
        end function
    
    ! ***************************************************************************************
    end module
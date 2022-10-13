 ! from test_wax_sim.py 
 ! Inputs (C_diff, MW_liq, liq_vis, mvw, liq_con )
 ! Internal(nul)
 ! Outputs ( tpg_htc, tpg_thf, thg, ntu, t_ini, t_ds, tm, tpg_qfl, tpg_dtx, ti, tpg_dtr, Le )

 module Heat_transfer 

!   Used Modules 
    use                 :: constants

!  variables must be declared 
        implicit none 

!   Private subroutines and variales 
        private 

!   Module public subroutines
        public          :: HT_gl_unified
        public          :: K_wax




! Module subroutines and functions
    Contains 


! ***************************************************************************************
! Declare subroutine HT_gl_unified to calculate downstream temperature

    ! change here: make pure
    subroutine          HT_gl_unified ( &
                                    enh_us, p_us, t_us, dx, lp, fw, dia, &
                                    p_a, t_a, t_ds, &
                                    liq, liq_i, pvt, &
                                    psp, prg, &
                                    enh_ds, ti, tm, tpg, inf )


    !   Called subroutines
        use Model_single,only       :   PG_single

    !	Input variables
            
            real(rp),intent(in)         ::  dia             !   Inner pipe diameter     [m]
            real(rp),intent(in)         ::  dx              !   Section length          [m]
            real(rp),intent(in)         ::  enh_us          !   Upstream total enh      [J/kg]
            real(rp),intent(in)	        ::  fw              !   Wax layer content       [-]
            real(rp),intent(in)         ::  lp              !   Pipeline length         [m]
            real(rp),intent(in)         ::  p_us            !   Upstream pressure       [Pa]
            real(rp),intent(in)         ::  t_us            !   Upstream temperature    [K]

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
            type(psp2_type),intent(in)  ::  psp             !   Pipe section properties data-type
                                        !   psp%ang             Angle                   [rad]
                                        !   psp%dia             Inner diameter          [m]
                                        !   psp%len             Length                  [m]
                                        !   psp%rou             Relative roughness      [-]
                                        !   psp%tpo             Outer temperature       [K]
                                        !   psp%ypr             Reference pressure      [Pa]
                                        !   psp%ynm             Layer code name         [-]
                                        !   psp%ymt             Layer material code     [-]
                                        !   psp%ydi             Layer diamater          [m]
                                        !   psp%ytr             Layer resistance        [K*m/W]
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
    !	Input / Output variables
        real(rp),intent(inout)      ::  p_a(2)          !   Annulus section pressure[Pa]
        real(rp),intent(inout)      ::  t_a(2)          !   Annulus wall temperature[K]
        real(rp),intent(inout)      ::  t_ds            !   Downstream temperature  [K]
    


    !   Output variables
            real(rp),intent(out)        ::  enh_ds          !   Downstream total enh    [J/kg]
            real(rp),intent(out)        ::  ti              !   Inner wall temperature  [K]
            real(rp),intent(out)        ::  tm              !   Medium temperature      [K]
            type(tpg_type),intent(out)  ::  tpg             !   Temperature gradient data-type
                                        !   tpg%dtr             Radial temp gradient    [K/m]
                                        !   tpg%dtx             Axial temp gradient     [K/m]
                                        !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                                        !   tpg%qfl             Heat                    [W]
                                        !   tpg%thf             Fluid thermal resistance[K*m/W]
                                        !   tpg%thw             Wax thermal resistance  [K*m/W]
            type(inf_type),intent(out)  ::  inf             !   Numerical report data-type
                                        !   inf%er              Error flag              [-]
                                        !   inf%it              Number of iterations    [-]
                                        !   inf%nm              Error name              [-]
    
    !   Internal variables
            real(rp)                    ::  htc             !   Convective coefficient  [W/(m**2*K)]
            real(rp)                    ::  phil            !   Dim.less pressure L&M   [-]
            type(prg_type)              ::  prg_s           !   Pressure gradient data-type
                                        !   prg_s%fpc           Flow pattern code       [-]
                                        !   prg_s%hlg           Gas-liq holdup          [-]
                                        !   prg_s%hll           Liq-liq holdup          [-]
                                        !   prg_s%ace           Acc pressure gradient   [Pa/m]
                                        !   prg_s%fri           Fri pressure gradient   [Pa/m]
                                        !   prg_s%gra           Gra pressure gradient   [Pa/m]
                                        !   prg_s%tot           Tot pressure gradient   [Pa/m]
            type(lay_type)              ::  single          !   Layer data-type
                                        !   h                   Holdup                  [-]
                                        !   v                   Velocity                [m/s]
                                        !   f%i                 Int friction            [-]
                                        !   f%w                 Wal friction            [-]
                                        !   s%i                 Int perimeter           [-]
                                        !   s%w                 Wal perimeter           [-]
                                        !   t%i                 Int stress              [Pa]
                                        !   t%w                 Wal stress              [Pa]
            type(layn_type)             ::  single_n        !   Slug region numerical data-type
                                        !   f%er                Friction error          [-]
                                        !   f%it                Friction iterations     [-]
                                        !   f%nm                Friction report         [-]
                                        !   h%er                Holdup eror             [-]
                                        !   h%it                Holdup iterations       [-]
                                        !   h%nm                Holdup report           [-]
                                        !   w%er                Wettability error       [-]
                                        !   w%it                Wettability iterations  [-]
                                        !   w%nm                Wettability report      [-]
    
        ! ***********************************************************************************
! Call CHT_liq for the convective heat transfer coefficient
        call CHT_liq (& 
            psp, liq, lp, liq_i%vis, htc)

! Call EB_single for the downstream temperature
        call EB_single ( & 
                        dia, dx, htc, fw, t_us, &
                        p_a, t_a, &
                        prg%fri, psp, liq, liq_i, pvt, &
                        t_ds, ti, tm, tpg, inf )

            enh_ds = zero 
    
    end subroutine
! Define EB_single to calculate the downstream temperature

    ! change here : make pure
    subroutine         EB_single ( & 
                                    dia, dx, htc, fw, t_us, &
                                    p_a, t_a, &
                                    prg_fri, psp, fluid, fluid_i, pvt, &
                                    t_ds, ti, tm, tpg, inf )

    ! ***************************************************************************************
        !	Input variables
            real(rp),intent(in)         ::  dia             !   Inner pipe diameter     [m]
            real(rp),intent(in)         ::  dx              !   Section length          [m]
            real(rp),intent(in)         ::  htc             !   Convective coefficient  [W/(m**2*K)]
            real(rp),intent(in)	        ::  fw              !   Wax layer content       [-]
            real(rp),intent(in)         ::  prg_fri         !   Fri pressure gradient   [Pa/m]
            real(rp),intent(in)         ::  t_us            !   Upstream temperature    [K]
            type(f2_type),intent(in)    ::  fluid           !   Fluid data-type
                                        !	fluid%den           Density                 [kg/m^3]
                                        !	fluid%stn           Mass fraction           [-]
                                        !	fluid%vel           Superficial velocity    [m/s]
                                        !	fluid%vis           Viscosity               [Pa*s]
                                        !   fluid%con           Conductivity            [W/(m*K)]
                                        !   fluid%enh           Enthalpy                [J/kg]
                                        !   fluid%epc           Expansion coefficient   [1/K]
                                        !   fluid%scp           Specific heat           [J/(kg*K)]]
            type(f3_type),intent(in)    ::  fluid_i         !   Fluid-wall data-type
                                        !   fluid_i%con         Conductivity            [W/(m*K)]
                                        !   fluid_i%den         Density                 [kg/m^3]
                                        !   fluid_i%epc         Expansion coefficient   [1/K]
                                        !   fluid_i%scp         Specific heat           [J/(kg*K)]
                                        !	fluid_i%vis         Viscosity               [Pa*s]
            type(psp2_type),intent(in)  ::  psp             !   Pipe section properties data-type
                                        !   psp%ang             Angle                   [rad]
                                        !   psp%dia             Inner diameter          [m]
                                        !   psp%len             Length                  [m]
                                        !   psp%rou             Relative roughness      [-]
                                        !   psp%tpo             Outer temperature       [K]
                                        !   psp%ypr             Reference pressure      [Pa]
                                        !   psp%ynm             Layer code name         [-]
                                        !   psp%ymt             Layer material code     [-]
                                        !   psp%ydi             Layer diamater          [m]
                                        !   psp%ytr             Layer resistance        [K*m/W]
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

        !	Input / Output variables
            real(rp),intent(inout)      ::  p_a(2)          !   Annulus section pressure[Pa]
            real(rp),intent(inout)      ::  t_a(2)          !   Annulus wall temperature[K]

        !   Output variables
            real(rp),intent(out)        ::  t_ds            !   Downstream temperature  [K]
            real(rp),intent(out)        ::  ti              !   Inner wall temperature  [K]
            real(rp),intent(out)        ::  tm              !   Medium temperature      [K]
            type(tpg_type),intent(out)  ::  tpg             !   Temperature gradient data-type
                                        !   tpg%dtr             Radial temp gradient    [K/m]
                                        !   tpg%dtx             Axial temp gradient     [K/m]
                                        !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                                        !   tpg%qfl             Heat                    [W]
                                        !   tpg%thf             Fluid thermal resistance[K*m/W]
                                        !   tpg%thw             Wax thermal resistance  [K*m/W]
            type(inf_type),intent(out)  ::  inf             !   Numerical report data-type
                                        !   inf%er              Error flag              [-]
                                        !   inf%it              Number of iterations    [-]
                                        !   inf%nm              Error name              [-]
    
        !   Internal variables
            real(rp)                    ::  t_ini           !   Initial corr temperature[K]
            real(rp)                    ::  mflow           !   Mass flow rate          [kg/s]
            real(rp)                    ::  ntu             !   Number of traner units[1/m]
            real(rp)                    ::  tha             !   Annulus thermal res     [K*m/W]
            real(rp)                    ::  thg             !   Global thermal res 

    !   ! *************************************************************************************** 
        !   Pipe section numerical initialization 
            inf = inf_type ( .false., 1, no_error )

        !   Mass flow rate

        ! change here
            mflow = ap * dia ** 2 * fluid%den * fluid%vel

          !  mflow = ap * dia ** 2 * 807.0 * fluid%vel

        !   Convective heat transfer coefficient
            tpg%htc = htc

        !   Fluid thermal resistance
            tpg%thf = one / (dia * tpg%htc)

        !   Wax thermal resistance
            tpg%thw = 0.5 * log(psp%dia / dia) / (two * K_wax (fw, fluid_i%con))

        !   Annular space thermal resistance
            if (psp%ynm(2) /= 6) then

                !print*,"ynm(2)=",psp%ynm(0

                tha = 0.0
            else

                call THR_annulus ( &
                    p_a, t_a, dx, psp, pvt, &
                    tha, inf )
            end if

            tha = 0.05

            print*, "fluid thermal res", tpg%thf 
            print*, "pipe layer thermal res", psp%ytr
            print*, "annular thermal res", tha
        !   Global thermal resistance
            thg = sum(psp%ytr) + tpg%thf + tpg%thw + tha

        
        !   Number of transfer units
            ntu = (pi * dx) / (thg * mflow * fluid%scp)

        !   Initial corrected temperature
            t_ini = psp%tpo + prg_fri * dx / (fluid%den * fluid%scp * ntu)

        !   Pipe section, downstream temperature
            t_ds = t_ini + (t_us -t_ini) * exp(-ntu)

        !   Pipe section, medium temperature
            tm = t_ini + (t_us - t_ini) * exp(-p5 * ntu)  

        !   Pipe section, heat
            tpg%qfl = mflow * fluid%scp * (t_ds - t_us)

        !   Pipe section, axial temperature gradient
            tpg%dtx = (t_ds - t_us) / dx

            if (psp%ynm(2) == 6) then

        !       Pipe section, outher tubing wall temperature
                t_a(1) = tm + tpg%qfl * (tpg%thf + tpg%thw + psp%ytr(1)) / (pi * dx)

        !       Pipe section, inner casing wall temperature
                t_a(2) = tm + tpg%qfl * (tpg%thf + tpg%thw + psp%ytr(1) + tha) / (pi * dx)
            end if

        !   Pipe section, inner wall temperature
            ti = tm + tpg%qfl * tpg%thf / (pi * dx)

        !   Pipe section, radial temperature gradient
            tpg%dtr = tpg%qfl / (fluid_i%con * dia * pi * dx)

        end subroutine
    ! ***************************************************************************************
! Define CHT_liq to calculate the liquid heat transfer coefficient
  
    pure subroutine         CHT_liq ( &
        psp, liq, lp, liq_vis_i, &
        htc )

    ! ***************************************************************************************
        !	Input variables
            real(rp),intent(in)         ::  lp              !   Pipeline length         [m]
            real(rp),intent(in)         ::  liq_vis_i       !   Wall temp liq viscosity [Pa*s]
            type(f2_type),intent(in)    ::  liq             !   Bulk liq fluid data-type
                    !	liq%den             Density                 [kg/m^3]
                    !	liq%stn             Mass fraction           [-]
                    !	liq%vel             Superficial velocity    [m/s]
                    !	liq%vis             Viscosity               [Pa*s]
                    !   liq%con             Conductivity            [W/(m*K)]
                    !   liq%enh             Enthalpy                [J/kg]
                    !   liq%epc             Expansion coefficient   [1/K]
                    !   liq%scp             Specific heat           [J/(kg*K)]
            type(psp2_type),intent(in)  ::  psp             !   Pipe section properties data-type
                    !   psp%ang             Angle                   [rad]
                    !   psp%dia             Inner diameter          [m]
                    !   psp%len             Length                  [m]
                    !   psp%rou             Relative roughness      [-]
                    !   psp%tpo             Outer temperature       [K]
                    !   psp%ypr             Reference pressure      [Pa]
                    !   psp%ynm             Layer code name         [-]
                    !   psp%ymt             Layer material code     [-]
                    !   psp%ydi             Layer diamater          [m]
                    !   psp%ytr             Layer resistance        [K*m/W]

        !	Output variables
            real(rp),intent(out)         ::  htc            !   Convective coefficient  [W/(m**2*K)]

            !	Internal variables
            real(rp)			        ::  nul             !   Liq Nusselt number      [-]
            real(rp)                    ::  rel             !	Liq Reynolds number     [-]
            real(rp)			        ::  prl             !   Liq Prandtl number      [-]

            ! ***********************************************************************************

        !   Liq Prandtl number
            prl = liq%scp * liq%vis / liq%con

        !   Liq Reynolds number
            rel = psp%dia * liq%den * liq%vel / liq%vis

      
        !   Liq Nusselt number
            if (rel < 2100._rp) then

        !   Laminar flow
            call C_SiederTate ( &
            psp%dia, lp, liq%vis, liq_vis_i, prl, rel, nul )
            else

        !   Turbulent flow
            call C_Petukhov ( &
            psp%rou, liq%vis, liq_vis_i, prl, rel, nul )
            end if

        !   Convective heat transfer coefficient
            htc = nul * liq%con / psp%dia

            ! ***********************************************************************************
            end subroutine


! ***********************************************************************************          
            pure function           K_wax ( &
            fw, thk )
        ! ***********************************************************************************

        !   Parameters
        real(rp),parameter	        ::	kw = 0.25_rp    !   Wax thermal conductivity[W/(m*K)]

        !	Input variables
        real(rp),intent(in)	        ::  fw              !   Wax layer content       [-]
        real(rp),intent(in)	        ::  thk             !   Liq thermal conductivity[W/(m*K)]

        !   Output variables
        real(rp)                    ::  K_wax           !   Wax thermal conductivity[W/(m*K)]

        ! ***********************************************************************************

        !   Wax thermal conductivity
        K_wax = 0.07*((two * kw + thk) + fw * (kw - thk)) * thk / &
        ((two * kw + thk) - two * fw * (kw - thk))

        ! ***********************************************************************************
        end function

        pure subroutine         C_SiederTate ( &
        dia, lp, vis, visw, &
        pr, re, &
        nu )

        ! ***********************************************************************************

        !	Input variables
        real(rp),intent(in)         ::  dia             !   Pipe diameter           [m]
        real(rp),intent(in)         ::  lp              !   Pipeline length         [m]
        real(rp),intent(in)	        ::  pr		        !	Prandtl number          [-]
        real(rp),intent(in)	        ::  re		        !	Reynolds number         [-]
        real(rp),intent(in)	        ::  vis             !   Viscosity               [Pa*s]
        real(rp),intent(in)	        ::  visw            !   Wall temp viscosity     [Pa*s]

        !   Output variables
        real(rp),intent(out)        ::  nu		        !	Nusselt number          [-]

        ! ***********************************************************************************

        !   Nusselt number
        nu = 1.86_rp * (re * pr * dia / lp) ** p33 * (vis / visw) ** 0.14_rp

        ! ***********************************************************************************
        end subroutine

        pure subroutine         C_Petukhov ( &
        rou, vis, visw, &
        pr, re, &
        nu )



        !	Input variables
        real(rp),intent(in)	        ::  pr		        !	Prandtl number          [-]
        real(rp),intent(in)	        ::  re		        !	Reynolds number         [-]
        real(rp),intent(in)         ::  rou             !	Pipe relative roughness [-]
        real(rp),intent(in)	        ::  vis             !   Viscosity               [Pa*s]
        real(rp),intent(in)	        ::  visw            !   Wall temp viscosity     [Pa*s]

        !   Output variables
        real(rp),intent(out)        ::  nu		        !	Nusselt number          [-]

        !	Internal variables
        real(rp)                    ::  fric            !   Friction factor         [-]

        ! ***********************************************************************************

        !   Friction factor
        fric = one / (1.82_rp * log10(re) - 1.64_rp) ** 2

        !   Nusselt number
        nu = 0.125_rp * fric  * re * pr / &
        (1.07_rp + 12.7_rp * sqrt(0.125_rp * fric) * (pr ** (two * p33) - one)) * &
        (vis / visw) ** p25

        ! ***********************************************************************************
        end subroutine

        ! change here make pure
          subroutine         THR_annulus ( &
            p_a, t_a, dx, psp, pvt, &
            tha, inf )
        ! **********************************************************************************

        !   Called subroutines
        
        use Properties,only         :   PRO_fluids_w
        

        !   Parameters
        real(rp),parameter          ::  aux0(3) = [zero,zero,zero]

        !	Input variables
        real(rp),intent(in)         ::  dx              !   Section length          [m]
        type(psp2_type),intent(in)  ::  psp             !   Pipe section properties data-type
                !   psp%ang             Angle                   [rad]
                !   psp%dia             Inner diameter          [m]
                !   psp%len             Length                  [m]
                !   psp%rou             Relative roughness      [-]
                !   psp%tpo             Outer temperature       [K]
                !   psp%ypr             Reference pressure      [Pa]
                !   psp%ynm             Layer code name         [-]
                !   psp%ymt             Layer material code     [-]
                !   psp%ydi             Layer diamater          [m]
                !   psp%ytr             Layer resistance        [K*m/W]
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

        !	Input / Output variables
        real(rp),intent(inout)      ::  p_a(2)          !   Annulus section pressure[Pa]
        real(rp),intent(inout)      ::  t_a(2)          !   Annulus wall temperature[K]

        !	Output variables
        real(rp),intent(out)        ::  tha             !   Annulus thermal res     [K*m/W]
        type(inf_type),intent(out)  ::  inf             !   Numerical report data-type
                !   inf%er              Error flag              [-]
                !   inf%it              Number of iterations    [-]
                !   inf%nm              Error name              [-]

        !   Internal variables
        real(rp)                    ::  gr		        !	Grashof number          [-]
        real(rp)                    ::  nu		        !	Nusselt number          [-]
        real(rp)                    ::  pr		        !	Prandtl number          [-]
        type(f3_type)               ::  fluid(2)        !   Fluid-wall data-type
                !   con                 Conductivity            [W/(m*K)]
                !   den                 Density                 [kg/m^3]
                !   epc                 Expansion coefficient   [1/K]
                !   scp                 Specific heat           [J/(kg*K)]
                !	vis                 Viscosity               [Pa*s]

        ! ***********************************************************************************

        !   Pipe section numerical initialization 
        inf = inf_type ( .false., 1, no_error )


        call PRO_fluids_w ( &
        zero, p5*(p_a(1)+p_a(2)), p5*(t_a(1)+t_a(2)), pvt, &
        zero, zero, aux0, &
        fluid(1), inf )
        !       -----------------------------------------------------------------------------
        

        if (inf%er) then

        !       Annulus thermal resistance
        tha = zero
        else

        print*, "calculate annular thermal res"
        !	    Prandtl number
        pr = fluid(1)%scp * fluid(1)%vis / fluid(1)%con

        !	    Grashof number
        gr = g * fluid(1)%epc * fluid(1)%den ** 2 * (t_a(1) - t_a(2)) * &
        (psp%ydi(3) - psp%ydi(2)) ** 3 / (8._rp * fluid(1)%vis)

        !	    Nusselt number 
        call C_HasanKabir ( &
        gr, pr, nu)

        !       Annulus thermal resistance
        tha = p5 * nu * fluid(1)%con / log(psp%ydi(3) / psp%ydi(2))

        !       Annulus pressure
        p_a(2) = p_a(1) - g * fluid(1)%den * dx * sin(psp%ang)
        end if

        ! ***********************************************************************************
        end subroutine

        
            pure subroutine         C_HasanKabir ( &
            gr, pr, &
            nu )
        ! ***********************************************************************************

        !	Input variables
        real(rp),intent(in)	        ::  gr		        !	Grashof number          [-]
        real(rp),intent(in)	        ::  pr		        !	Prandtl number          [-]

        !   Output variables
        real(rp),intent(out)        ::  nu		        !	Nusselt number          [-]

        ! ***********************************************************************************

        !   Nusselt number
        nu = 0.049_rp * (gr * pr) ** p33 * pr ** 0.074_rp

        ! ***********************************************************************************
        end subroutine

    end module
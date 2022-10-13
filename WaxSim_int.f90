module Integrator  

    ! used modules

        use              constants
        use              reader
        use              Heat_transfer
        use              Model_single


    ! all variables must be declared within the subroutine
        implicit none

    ! module public subroutines

        public              :: int_pss
        public              :: int_ss


    ! subroutines contained within the integrator module
        contains 

    ! ****************************************************************************************************
    ! Define the int_mh subroutine for heat and momentum
        subroutine      int_mh ( &
            massflow, unit_log, &
             np, ns, psp, pvt, flow, pre_bc, tmp_bc, &
            pipe, error_int )
    
        
            !   Called subroutines
                !use Model_single,only      :   PG_single
                use Heat_transfer,only      :   HT_gl_unified
                use Properties,only         :   PRO_enh
                use Properties,only         :   PRO_fluids
                use Properties,only         :  PRO_fluids_w
        
            !   Parameters
                real(rp),parameter          ::  aux0(3) = [zero, zero, zero]
        
            !	Input variables
                logical,intent(in)          ::  massflow        !   Mass or volumetric flow [-]
                integer(ip),intent(in)	    ::	unit_log        !   Log file unit           [-]
                integer(ip),intent(in)	    ::  np              !   Number of pipes         [-]
                integer(ip),intent(in)	    ::  ns(:)           !   Number of segments      [-]
                real(rp),intent(in)         ::  flow            !   Flow rate               [kg/s, m**3/s]
                real(rp),intent(in)         ::  pre_bc          !   Pressure bound cond     [Pa]
                real(rp),intent(in)         ::  tmp_bc          !   Temperature bound cond  [K]
            !    type(model_type),intent(in) ::  model           !   Model data-type 
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
                type(psp2_type),intent(in)  ::  psp(:)          !   Pipe section properties data-type
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
        
            !   Input / Output variables
                logical,intent(out)         ::  error_int       !   ODE solver error status [-]
                type(pp_type),intent(inout) ::  pipe(:)         !   Pipe data-type
                                            !   sec                 Pipe section data-type
                                            !   sec%lp              Pipeline length         [m]
                                            !   sec%di              Inner diameter          [m]
                                            !   sec%dx              Section length          [m]
                                            !   sec%fw              Wax weigth fraction     [-]
                                            !   sec%pr              Pressure                [Pa]
                                            !   sec%th              Wax layer thickness     [m]
                                            !   sec%tp              Bulk temperature        [K]
                                            !   sec%ti              Inner diameter temp     [K] 

                                            !   liq                 Liq properties data-type
                                            !	liq%den             Density                 [kg/m^3]
                                            !	liq%stn             Surface tension         [N/m]
                                            !	liq%vel             Superficial velocity    [m/s]
                                            !	liq%vis             Viscosity               [Pa*s]
                                            !   liq%enh             Enthalpy                [J/kg]
                                            !   liq%scp             Specific heat           [J/(kg*K)]
                                            !   liq%con             Conductivity            [W/(m*K)]
                                            !   film                Film data-type
                                            !   film%len            Region length           [m]
                                            !   film%ent            Liq entrainment fraction[-]
    
                                            !   film%liq%h          Liq holdup              [-]
                                            !   film%liq%v          Liq velocity            [m/s]
                                            !   film%liq%f%j        Liq int friction        [-]
                                            !   film%liq%f%w        Liq wal friction        [-]
                                            !   film%liq%s%j        Liq int perimeter       [-]
                                            !   film%liq%s%w        Liq wal perimeter       [-]
                                            !   film%liq%t%j        Liq int stress          [Pa]
                                            !   film%liq%t%w        Liq wal stress          [Pa]

                                            !   prg                 Pressure gradient data-type
                                            !   prg%fpc             Flow pattern code       [-]
                                            !   prg%hlg             Gas-liq holdup          [-]
                                            !   prg%hll             Liq-liq holdup          [-]
                                            !   prg%ace             Acc pressure gradient   [Pa/m]
                                            !   prg%fri             Fri pressure gradient   [Pa/m]
                                            !   prg%gra             Gra pressure gradient   [Pa/m]
                                            !   prg%tot             Tot pressure gradient   [Pa/m]
                                            !   tpg                 Temperature gradient data-type
                                            !   tpg%dtr             Radial temp gradient    [K/m]
                                            !   tpg%dtx             Axial temp gradient     [K/m]
                                            !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                                            !   tpg%qfl             Heat                    [W]
                                            !   tpg%thf             Fluid thermal resistance[K*m/W]
                                            !   tpg%thw             Wax thermal resistance  [K*m/W]
        
            !   Internal variables
                logical                     ::  ode_warg        !   ODE solver warning      [-]
                integer(ip)                 ::  ic, j, k        !   Counters                [-] 
                integer(ip)                 ::	iost            !   File I/O status         [-]
                real(rp)                    ::  dx              !   Section length          [m]
                real(rp)                    ::  enh_ds          !   Down section total enh  [J/kg]
                real(rp)                    ::  enh_us          !   Up section total enh    [J/kg]
                real(rp)                    ::  norm            !   Error norm              [-]
                real(rp)                    ::  p_ds            !   Down section pressure   [Pa]
                real(rp)                    ::  p_sec           !   Old section pressure    [Pa]
                real(rp)                    ::  p_us            !   Up section pressure     [Pa]
                real(rp)                    ::  t_ds            !   Down section temperature[K]
                real(rp)                    ::  t_sec           !   Old section temperature [K]
                real(rp)                    ::  t_us            !   Up section temperature  [K]
                real(rp)                    ::  p_a(2)          !   Annulus section pressure[Pa]
                real(rp)                    ::  t_a(2)          !   Annulus wall temperature[K]
                type(inf_type)              ::  inf             !   Numerical report data-type
                                            !   inf%er              Error flag              [-]
                                            !   inf%it              Number of iterations    [-]
                                            !   inf%nm              Error name              [-]

                type(f3_type)               ::  liq_i           !   Liq fluid inner wall data
                                            !   liq_i%con           Conductivity            [W/(m*K)]
                                            !   liq_i%den           Density                 [kg/m^3]
                                            !   liq_i%epc           Expansion coefficient   [1/K]
                                            !   liq_i%scp           Specific heat           [J/(kg*K)]
                                            !	liq_i%vis           Viscosity               [Pa*s]
 

                                            !   liq%f%er            Friction error          [-]
                                            !   liq%f%it            Friction iterations     [-]
                                            !   liq%f%nm            Friction report         [-]
                                            !   liq%h%er            Holdup eror             [-]
                                            !   liq%h%it            Holdup iterations       [-]
                                            !   liq%h%nm            Holdup report           [-]
                                            !   liq%w%er            Wettability error       [-]
                                            !   liq%w%it            Wettability iterations  [-]
                                            !   liq%w%nm            Wettability report      [-]
                                            !   oil%f%it            Friction iterations     [-]
                                            !   oil%f%nm            Friction report         [-]
                                            !   oil%h%er            Holdup eror             [-]
                                            !   oil%h%it            Holdup iterations       [-]
                                            !   oil%h%nm            Holdup report           [-]
                                            !   oil%w%er            Wettability error       [-]
                                            !   oil%w%it            Wettability iterations  [-]
                                            !   oil%w%nm            Wettability report      [-]
                type(lay_type)              ::  mix             !   Mix layer data-type
                                            !   mix%h               Holdup                  [-]
                                            !   mix%v               Velocity                [m/s]
                                            !   mix%f%i             Int friction            [-]
                                            !   mix%f%w             Wal friction            [-]
                                            !   mix%s%i             Int perimeter           [m]
                                            !   mix%s%w             Wal perimeter           [m]
                                            !   mix%t%i             Int stress              [Pa]
                                            !   mix%t%w             Wal stress              [Pa]
                type(layn_type)             ::  mix_n           !   Mix layer numerical data-type
                                            !   mix_n%f%er          Friction error          [-]
                                            !   mix_n%f%it          Friction iterations     [-]
                                            !   mix_n%f%nm          Friction report         [-]
                                            !   mix_n%h%er          Holdup error            [-]
                                            !   mix_n%h%it          Holdup iterations       [-]
                                            !   mix_n%h%nm          Holdup report           [-]
                                            !   mix_n%w%er          Wettability error       [-]
                                            !   mix_n%w%it          Wettability iterations  [-]
                                            !   mix_n%w%nm          Wettability report      [-]
        
            ! ***********************************************************************************
        

                

            !   Header pipe report
                write (unit = unit_log, fmt = 10, iostat = iost)
                write (unit = unit_log, fmt = 20, iostat = iost)
        
                
            !   ODE solver error status
                error_int = .false.
        
            !   First pipe, first section properties, upstream conditions (come from read_set call in waxsim)
                pipe(1)%sec(0)%lp = zero
                pipe(1)%sec(0)%di = psp(1)%dia
                pipe(1)%sec(0)%dx = zero
                pipe(1)%sec(0)%pr = pre_bc
                pipe(1)%sec(0)%tp = tmp_bc
                pipe(1)%sec(0)%ti = tmp_bc
        
                

            !   First pipe, PVT properties
                call PRO_fluids ( &
                    massflow, pipe(1)%sec(0)%di, flow, pipe(1)%sec(0)%pr, pipe(1)%sec(0)%tp, pvt, &
                    zero, zero, aux0, &
                    pipe(1)%liq(0), inf )
        
                    
            !   First pipe, PVT properties error test
                if (inf%er) then
        
            !       Message
                    call LOG_in ( &
                        unit_log, '    Integration aborted. Pvt interpolation upstream. ' &
                        // trim(inf%nm) )
        
            !       Abort PIPE_int_mh subroutine
                    error_int = .true.
                    return
                end if
               
            !   Pipes loop
                pipe_loop: do j = 1, np
        
            !       Pipe, first section, pressure and temperature
                    if (j == 1) then
        
            !           First pipe
                        p_ds = pipe(1)%sec(0)%pr
                        t_ds = pipe(1)%sec(0)%tp
                    else
        
            !           Other pipes
                        p_ds = pipe(j-1)%sec(ns(j-1)+1)%pr
                        t_ds = pipe(j-1)%sec(ns(j-1)+1)%tp
                    end if
        
            !       Section, well model
                    if (psp(j)%ynm(2) == 6) then
        
            !           Annulus pressure
                        p_a = maxval(psp%ypr)
        
            !           Annulus wall temperature
                        t_a(1) = t_ds
                        t_a(2) = t_ds - p5
                    end if
        
            !       Pipe, uniform mesh
                    dx = psp(j)%len / real(ns(j), rp)
        
            !       Pipe sections loop
                    section_loop: do k = 1, ns(j)
        
            !           Section, lengths
                        pipe(j)%sec(k)%lp = sum(psp(1:j)%len) - (p5 + real(ns(j)-k,rp)) * dx
                        pipe(j)%sec(k)%di = psp(j)%dia
                        pipe(j)%sec(k)%dx = dx
        
            !           Section, upstream pressure and temperature guess
                        p_us = p_ds
                        t_us = t_ds
        
            !           Section, PVT properties, upstream total enthalpy guess
                        call PRO_enh ( &
                            pvt, p_us, t_us, &
                            enh_us, inf )

                            

                            !           Section, PVT properties error test
                        if (inf%er) then
        
            !               Message
                            call LOG_in ( &
                                unit_log, '    Integration aborted. Pvt interpolation enthalpy. ' &
                                // trim(inf%nm) )
        
            !               Exit pipe integration loop
                            error_int = .true.
                            exit pipe_loop
                        end if
        
            !           Section, pressure and temperatures
                        pipe(j)%sec(k)%pr = p_us
                        pipe(j)%sec(k)%ti = psp(j)%tpo
                        pipe(j)%sec(k)%tp = psp(j)%tpo
        
            !           Section, downstream temperature guess
                        t_ds = p5 * (psp(j)%tpo + t_us)
        
            !           ODE solver loop
                        ode_loop: do ic = 1, tol%max
        
            !               Section, ODE solver warning
                            ode_warg = .false.
        
            !               Section, update pressure and temperature
                            p_sec = pipe(j)%sec(k)%pr 
                            t_sec = pipe(j)%sec(k)%tp
        
            !               Section, PVT properties
                            call PRO_fluids ( &
                                massflow, pipe(j)%sec(k)%di, flow, p_sec, t_sec, pvt, &
                                zero, zero, aux0, &
                                pipe(j)%liq(k), inf )
        
            !               Section, PVT properties error test
                            if (inf%er) then
        
            !                   Message
                                call LOG_in ( &
                                    unit_log, '    Integration aborted. Pvt interpolation. ' &
                                    // trim(inf%nm) )
        
            !                   Exit pipe integration loop
                                error_int = .true.
                                exit pipe_loop
                            end if
        
            !               Section, flow pattern and pressure gradient
                            call PG_single ( &
                                psp1_type(psp(j)%ang, pipe(j)%sec(k)%di, psp(j)%rou), &
                                pipe(j)%liq(k)%f1_type, &
                                p_sec, &
                                pipe(j)%prg(k), mix, mix_n)
                    
        
            !               Section, downstream pressure
                            p_ds = p_us - pipe(j)%prg(k)%tot * pipe(j)%sec(k)%dx
        
            !               Section, pressure
                            pipe(j)%sec(k)%pr = p5 * (p_ds + p_us)
        
            !               Section, PVT properties inner wall
                            call PRO_fluids_w ( &
                                pipe(j)%sec(k)%di, p_sec, pipe(j)%sec(k)%ti, pvt, &
                                zero, zero, aux0, &
                                liq_i, inf )
        
            !               Section, PVT properties error test
                            if (inf%er) then
        
            !                   Message
                                call LOG_in ( &
                                    unit_log, '    Integration aborted. Pvt interpolation inner wall. ' &
                                    // trim(inf%nm) )
        
            !                   Exit pipe integration loop
                                error_int = .true.
                                exit pipe_loop
                            end if
        
            !               Section, temperature gradients and temperatures
                            call HT_gl_unified (  &
                                enh_us, p_us, t_us, pipe(j)%sec(k)%dx, pipe(j)%sec(k)%lp, zero, psp(j)%dia, &
                                p_a, t_a, t_ds, &
                                pipe(j)%liq(k), liq_i, pvt, &
                                psp(j), pipe(j)%prg(k),  &
                                enh_ds, pipe(j)%sec(k)%ti, pipe(j)%sec(k)%tp, pipe(j)%tpg(k), inf )

                             
        
            !               Section, temperature gradients and temperatures error test
                            if (inf%er) then
        
            !                   Message
                                call LOG_in ( &
                                    unit_log, '    Integration aborted. Heat transfer. ' &
                                    // trim(inf%nm) )
        
            !                   Exit pipe integration loop
                                error_int = .true.
                                exit pipe_loop
                            end if
        
            !               Section, temperature gradients and temperatures warning
                            if (t_ds < psp(j)%tpo) then
        
                                t_ds = psp(j)%tpo + tol%eps
                                ode_warg = .true.
                            end if
        
            !               Section, convergence test
                            norm = sqrt( &
                                (one - pipe(j)%sec(k)%pr / p_sec) ** 2 + &
                                (one - pipe(j)%sec(k)%tp / t_sec) ** 2 )
                            if (norm < tol%rel) exit ode_loop
                            
                            
                        end do ode_loop
        
            !           Section, update annulus pressure 
                        p_a(1) = p_a(2)
        
            !           Maximum ODE evaluations achived
                        if (ic > tol%max) ode_warg = .true.
        
            !           Section, Log file report
                        write (unit = unit_log, fmt = 30, iostat = iost) j, k, ic, ode_warg
                    end do section_loop
        
            !       Section, lengths last section
                    pipe(j)%sec(k)%lp = sum(psp(1:j)%len)
                    pipe(j)%sec(k)%dx = zero
        
            !       Section, pressure and temperature last section
                    pipe(j)%sec(k)%pr = p_ds
                    pipe(j)%sec(k)%tp = t_ds
        
            !       Section, PVT properties last section
                    call PRO_fluids ( &
                        massflow, pipe(j)%sec(k-1)%di, flow, pipe(j)%sec(k)%pr, pipe(j)%sec(k)%tp, pvt, &
                        zero, zero, aux0, &
                        pipe(j)%liq(k), inf )
        
            !       Section, PVT properties error test
                    if (inf%er) then
        
            !           Message
                        call LOG_in ( &
                            unit_log, '    Integration aborted. Pvt interpolation downstream. ' &
                            // trim(inf%nm) )
        
            !           Exit pipe integration loop
                        error_int = .true.
                        exit pipe_loop
                    end if
                end do pipe_loop
        
            !   Formats
            10  format (5x, 40('*'))    
            20  format (5x, 'Pipe', 5x 'Section', 2x, 'Iterations', 5x, 'Warning')
            30  format (5x, i4, 8x, i4, 8x, i4, 8x, l4)
            
            ! ***********************************************************************************
            end subroutine
            
            ! ***************************************************************************************
        
    ! ****************************************************************************************************
    ! Define int_mhm subroutine for heat momentum and mass balance
                subroutine             int_mhm ( &
                    massflow, unit_log, old_pipe, &
                     np, ns, psp, pvt, thm, flow, &
                    xwd, xwm, c_finit, alfa, cdif, dt, vk, &
                    pipe, error_int )


            !   Called subroutines
                
                use Heat_transfer,only      :   HT_gl_unified
                use Mass_transfer,only      :   MT_deposition
                use Properties,only         :   PRO_enh
                use Properties,only         :   PRO_fluids
                use Properties,only         :   PRO_fluids_w
                use Properties,only         :   PRO_wax

            !   Parameters
                real(rp),parameter          ::  aux0(3) = [zero, zero, zero]

            !	Input variables
                logical,intent(in)          ::  massflow        !   Mass or volumetric flow [-]
                integer(ip),intent(in)      ::  np              !   Number of pipes         [-]
                integer(ip),intent(in)      ::	unit_log        !   Log file unit           [-]
                integer(ip),intent(in)      ::	xwd(2)          !	Wax diffusivity selector[-]
                integer(ip),intent(in)      ::	xwm             !	Wax model               [-]
                integer(ip),intent(in)	    ::  ns(:)           !   Number of segments      [-]
                real(rp),intent(in)         ::  c_finit         !   Initial wax fraction multiplier [-]
                real(rp),intent(in)         ::  alfa            !	Aspect ratio            [-]
                real(rp),intent(in)         ::  cdif            !   Multiplier coefficient  [-]
                real(rp),intent(in)         ::  dt              !   Time step               [s]
                real(rp),intent(in)         ::  flow            !   Flow rate               [kg/s, m**3/s]
                real(rp),intent(in)         ::  vk(2)           !   Venkatesan paramaters   [-]
         !       type(model_type),intent(in) ::  model           !   Model data-type 
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
                type(psp2_type),intent(in)  ::  psp(:)          !   Pipe section properties data-type
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
                type(pp_type),intent(in)    ::  old_pipe(:)     !   Pipe data-type

            !   Input / Output variables
                logical,intent(out)         ::  error_int       !   ODE solver error status [-]
                type(pp_type),intent(inout) ::  pipe(:)         !   Pipe data-type

                            !   sec                 Pipe section data-type
                            !   sec%lp              Pipeline length         [m]
                            !   sec%di              Inner diameter          [m]
                            !   sec%dx              Section length          [m]
                            !   sec%fw              Wax weigth fraction     [-]
                            !   sec%pr              Pressure                [Pa]
                            !   sec%th              Wax layer thickness     [m]
                            !   sec%ti              Inner diameter temp     [K]
                            !   sec%tp              Bulk temperature        [K] 

                            !   liq                 Liq properties data-type
                            !	liq%den             Density                 [kg/m^3]
                            !	liq%stn             Surface tension         [N/m]
                            !	liq%vel             Superficial velocity    [m/s]
                            !	liq%vis             Viscosity               [Pa*s]
                            !   liq%enh             Enthalpy                [J/kg]
                            !   liq%scp             Specific heat           [J/(kg*K)]
                            !   liq%con             Conductivity            [W/(m*K)]

                            !   prg                 Pressure gradient data-type
                            !   prg%fpc             Flow pattern code       [-]
                            !   prg%hlg             Gas-liq holdup          [-]
                            !   prg%hll             Liq-liq holdup          [-]
                            !   prg%ace             Acc pressure gradient   [Pa/m]
                            !   prg%fri             Fri pressure gradient   [Pa/m]
                            !   prg%gra             Gra pressure gradient   [Pa/m]
                            !   prg%tot             Tot pressure gradient   [Pa/m]
                            !   tpg                 Temperature gradient data-type
                            !   tpg%dtr             Radial temp gradient    [K/m]
                            !   tpg%dtx             Axial temp gradient     [K/m]
                            !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                            !   tpg%qfl             Heat                    [W]
                            !   tpg%thf             Fluid thermal resistance[K*m/W]
                            !   tpg%thw             Wax thermal resistance  [K*m/W]
                type(lay_type)             :: mix   ! Mix layer report data-type
                type(layn_type)            :: mix_n  ! Mix layer report data-type


            !   Internal variables
                logical                     ::  ode_flag        !   Predictor/corrector flag[-]
                logical                     ::  ode_warg        !   ODE solver warning      [-]
                integer(ip)                 ::  ic, j, k        !   Counters                [-] 
                integer(ip)                 ::  iost            !   File I/O status         [-]
                real(rp)                    ::  dx              !   Section length          [m]
                real(rp)                    ::  dfw             !   Delta wax weigth frac   [-]
                real(rp)                    ::  dth             !   Delta wax layer thick   [m]
                real(rp)                    ::  enh_ds          !   Down section total enh  [J/kg]
                real(rp)                    ::  enh_us          !   Up section total enh    [J/kg]
                real(rp)                    ::  norm            !   Error norm              [-]
                real(rp)                    ::  p_ds            !   Down section pressure   [Pa]
                real(rp)                    ::  p_sec           !   Old section pressure    [Pa]
                real(rp)                    ::  p_us            !   Up section pressure     [Pa]
                real(rp)                    ::  old_di          !   Inner diameter          [m]
                real(rp)                    ::  old_dfw         !   Old dif wax weigth frac [-]
                real(rp)                    ::  old_dth         !   Old wax layer growth    [m]
                real(rp)                    ::  old_fw          !   Wax weigth fraction     [-]
                real(rp)                    ::  old_th          !   Wax layer thickness     [m]
                real(rp)                    ::  t_ds            !   Down section temperature[K]
                real(rp)                    ::  t_sec           !   Old section temperature [K]
                real(rp)                    ::  t_us            !   Up section temperature  [K]  
                real(rp)                    ::  fluxes(8)       !   ODE integration fluxes  [-]
                real(rp)                    ::  p_a(2)          !   Annulus section pressure[Pa]
                real(rp)                    ::  t_a(2)          !   Annulus wall temperature[K]

                type(f3_type)               ::  liq_i           !   Liq fluid inner wall data
                            !   liq_i%con           Conductivity            [W/(m*K)]
                            !   liq_i%den           Density                 [kg/m^3]
                            !   liq_i%epc           Expansion coefficient   [1/K]
                            !   liq_i%scp           Specific heat           [J/(kg*K)]
                            !	liq_i%vis           Viscosity               [Pa*s]
                type(inf_type)              ::  inf             !   Numerical report data-type
                            !   inf%er              Error flag              [-]
                            !   inf%it              Number of iterations    [-]
                            !   inf%nm              Error name              [-]


                            !   liq%f%er            Friction error          [-]
                            !   liq%f%it            Friction iterations     [-]
                            !   liq%f%nm            Friction report         [-]
                            !   liq%h%er            Holdup eror             [-]
                            !   liq%h%it            Holdup iterations       [-]
                            !   liq%h%nm            Holdup report           [-]
                            !   liq%w%er            Wettability error       [-]
                            !   liq%w%it            Wettability iterations  [-]
                            !   liq%w%nm            Wettability report      [-]
                            !   oil%f%it            Friction iterations     [-]
                            !   oil%f%nm            Friction report         [-]
                            !   oil%h%er            Holdup eror             [-]
                            !   oil%h%it            Holdup iterations       [-]
                            !   oil%h%nm            Holdup report           [-]
                            !   oil%w%er            Wettability error       [-]
                            !   oil%w%it            Wettability iterations  [-]
                            !   oil%w%nm            Wettability report      [-]

                type(wax_type)              ::  wax             !   Wax pproperties data-type
                            !   wax%den             Density                 [kg/m^3]
                            !   wax%cfr             Wax concentration frac  [mol/mol]
                            !   wax%enh             Enthalpy                [J/kg]
                            !   wax%mwl             Liq molecular weigth    [kg/kmol]
                            !   wax%mww             Wax molecular weigth    [kg/kmol]
                            !   wax%scp             Specific heat           [J/(kg*K)]
                            !   wax%wfr             Wax fraction            [mol/mol]
                            !   wax%dwf             Wax fraction d.w.r. tp  [mol/(mol*K)]
                type(wax_type)              ::  wax_i           !   Inner wax properties data-type
                            !   wax_i%den           Density                 [kg/m^3]
                            !   wax_i%cfr           Wax concentration frac  [mol/mol]
                            !   wax_i%enh           Enthalpy                [J/kg]
                            !   wax_i%mwl           Liq molecular weigth    [kg/kmol]
                            !   wax_i%mww           Wax molecular weigth    [kg/kmol]
                            !   wax_i%scp           Specific heat           [J/(kg*K)]
                            !   wax_i%wfr           Wax fraction            [mol/mol]
                            !   wax_i%dwf           Wax fraction d.w.r. tp  [mol/(mol*K)

            
                ! ***********************************************************************************
                    
            !   Header pipe report
                write (unit = unit_log, fmt = 10, iostat = iost)
                write (unit = unit_log, fmt = 20, iostat = iost)

            !   ODE solver error status
                error_int = .false.

            !   First pipe, first section properties, upstream conditions 
                pipe(1)%sec(0) = old_pipe(1)%sec(0)

                
            !   First pipe, PVT properties
                call PRO_fluids ( &
                massflow, pipe(1)%sec(0)%di, flow, pipe(1)%sec(0)%pr, pipe(1)%sec(0)%tp, pvt, &
                zero, zero, aux0, &
                pipe(1)%liq(0), inf )
                
            !   First pipe, PVT properties error test
                if (inf%er) then

            !       Message
                call LOG_in ( &
                unit_log, '    Integration aborted. Pvt interpolation upstream. ' &
                // trim(inf%nm) )

            !       Abort PIPE_int_mhm subroutine
                error_int = .true.
                return
                end if

            !   Pipe loop
                pipe_loop: do j = 1, np

            !       Pipe, first section, pressure and temperature, upstream conditions
                if (j == 1) then

            !           First pipe
                p_ds = pipe(1)%sec(0)%pr
                t_ds = pipe(1)%sec(0)%tp
                else

            !           Other pipes
                p_ds = pipe(j-1)%sec(ns(j-1)+1)%pr
                t_ds = pipe(j-1)%sec(ns(j-1)+1)%tp
                end if

            !       Section, well model
                if (psp(j)%ynm(2) == 6) then

            !           Annulus pressure
                p_a = maxval(psp%ypr)

            !           Annulus wall temperature
                t_a(1) = t_ds
                t_a(2) = t_ds - p5
                end if

            !       Pipe, uniform mesh
                dx = psp(j)%len / real(ns(j), rp)


                
            !       Pipe section loop
                section_loop: do k = 1, ns(j)
                
                
            !           Section, lengths
                pipe(j)%sec(k)%lp = sum(psp(1:j)%len) - (p5 + real(ns(j)-k,rp)) * dx
                pipe(j)%sec(k)%dx = dx

            !           Section, upstream pressure and temperature guess
                p_us = p_ds
                t_us = t_ds
                
            !           Section, PVT properties, upstream total enthalpy guess
                call PRO_enh ( &
                pvt, p_us, t_us, &
                enh_us, inf )

                

            !           Section, PVT properties error test
                if (inf%er) then

            !               Message
                call LOG_in ( &
                unit_log, '    Integration aborted. Pvt interpolation enthalpy. ' &
                // trim(inf%nm) )

            !               Exit pipe integration loop
                error_int = .true.
                exit pipe_loop
                end if

            !           Section, pressure, temperatures, firction, wax fraction and thickness
                pipe(j)%prg(k)%fri = old_pipe(j)%prg(k)%fri
                pipe(j)%sec(k)%pr = old_pipe(j)%sec(k)%pr
                pipe(j)%sec(k)%ti = old_pipe(j)%sec(k)%ti
                pipe(j)%sec(k)%tp = old_pipe(j)%sec(k)%tp
                dfw = 1.e-6_rp
                dth = 1.e-6_rp

                print*, "pipe pressure", pipe(j)%sec(k)%pr 

            !           Section, downstream temperature guess
                t_ds = p5 * (psp(j)%tpo + t_us)

            !           ODE alternative flag (.false. -> predictor , .true. -> corrector)
                ode_flag = .true.

            !           ODE solver loop
                ode_loop: do ic = 1, tol%max

            !               ODE solver warning
                ode_warg = .false.

            !               ODE predictor/corrector flag
                ode_flag = .not. ode_flag

            !               Section, update diameter, wax fraction and thickness 
                old_dfw = dfw
                old_dth = dth

                

                old_fw = old_pipe(j)%sec(k)%fw + dfw
                old_th = old_pipe(j)%sec(k)%th + dth
                old_di = psp(j)%dia - two * old_th
               
            !               Section, update pressure and temperature
                p_sec = pipe(j)%sec(k)%pr 
                t_sec = pipe(j)%sec(k)%tp

            !               Section, THM properties
                call PRO_wax ( &
                p_sec, t_sec, thm, &
                wax, inf )

            !               Section, THM properties error test
                if (inf%er) then

            !                   Message
                call LOG_in ( &
                    unit_log, '    Integration aborted. Thm interpolation. ' &
                    // trim(inf%nm) )

            !                   Exit pipe integration loop
                error_int = .true.
                exit pipe_loop
                end if

            !               Section, PVT properties 
                call PRO_fluids ( &
                massflow, old_di, flow, p_sec, t_sec, pvt, &
                pipe(j)%prg(k)%fri, wax%cfr, thm%vis, &
                pipe(j)%liq(k), inf )

                print*, "viscosity", pipe(j)%liq(k)%vis
            !               Section, PVT properties error test
                if (inf%er) then

            !                   Message
                call LOG_in ( &
                    unit_log, '    Integration aborted. Pvt interpolation. ' &
                    // trim(inf%nm) )

            !                   Exit pipe integration loop
                error_int = .true.
                exit pipe_loop
                end if
                
                call PG_single ( &
                psp1_type(psp(j)%ang, old_di, psp(j)%rou), &
                pipe(j)%liq(k)%f1_type, &
                p_sec, &
                pipe(j)%prg(k), mix, mix_n)
                
            !               Section, downstream pressure
                p_ds = p_us - pipe(j)%prg(k)%tot * pipe(j)%sec(k)%dx

            !               Section, pressure
                pipe(j)%sec(k)%pr = p5 * (p_ds + p_us)

            !               Section, THM properties inner wall
                call PRO_wax ( &
                p_sec, pipe(j)%sec(k)%ti, thm, &
                wax_i, inf )

            !               Section, THM properties error test
                if (inf%er) then

            !                   Message
                call LOG_in ( &
                    unit_log, '    Integration aborted. Thm interpolation inner wall. ' &
                    // trim(inf%nm) )

            !                   Exit pipe integration loop
                error_int = .true.
                exit pipe_loop
                end if

            !               Section, PVT properties inner wall
                call PRO_fluids_w ( &
                old_di, p_sec, pipe(j)%sec(k)%ti, pvt, &
                pipe(j)%prg(k)%fri, wax_i%cfr, thm%vis, &
                liq_i, inf )

            !               Section, PVT properties error test
                if (inf%er) then

            !                   Message
                call LOG_in ( &
                    unit_log, '    Integration aborted. Pvt interpolation inner wall. ' &
                    // trim(inf%nm) )

            !                   Exit pipe integration loop
                error_int = .true.
                exit pipe_loop
                end if
                
            !               Section, temperature gradients and temperatures
                call HT_gl_unified ( &
                enh_us, p_us,t_us, pipe(j)%sec(k)%dx, pipe(j)%sec(k)%lp, old_fw, old_di, &
                p_a, t_a, t_ds, &
                pipe(j)%liq(k), liq_i, pvt, &
                psp(j), pipe(j)%prg(k),  &
                enh_ds, pipe(j)%sec(k)%ti, pipe(j)%sec(k)%tp, pipe(j)%tpg(k), inf )

                !               Section, temperature gradients and temperatures error test
                if (inf%er) then

            !                   Message
                call LOG_in &
                    (unit_log, '    Integration aborted. Heat transfer. ' &
                    // trim(inf%nm))

            !                   Exit pipe integration loop
                error_int = .true.
                exit pipe_loop
                end if

            !               Section, temperature gradients and temperatures warning
                if (t_ds < psp(j)%tpo) then

                t_ds = psp(j)%tpo + tol%eps
                ode_warg = .true.
                end if
            !               Wax fraction deposition limit
                if (wax_i%wfr > 1.e-12_rp) then
            !                   Section, mass transfer
                    call MT_deposition ( &
                        ode_flag, xwd, int(2,ip), c_finit, alfa, cdif, dt, old_di, old_fw, old_th, vk, &
                        pipe(j)%sec(k)%pr, pipe(j)%sec(k)%tp, pipe(j)%sec(k)%ti, &
                        f3_type(0,0,0,0,0),pipe(j)%liq(k), liq_i, wax, wax_i, pvt, thm, &
                        psp(j)%dia, pipe(j)%prg(k), pipe(j)%tpg(k), &
                        dfw, dth, pipe(j)%sec(k)%mfa, pipe(j)%sec(k)%mfd, &
                        pipe(j)%sec(k)%mfg, pipe(j)%sec(k)%mfs, fluxes, inf )

                 
                       

                    !                   Section, mass transfer error test
                        if (inf%er) then

                    !                       Message
                            call LOG_in ( &
                                unit_log, '    Integration aborted. Mass transfer. ' &
                                // trim(inf%nm) )

                    !                       Exit pipe integration loop
                            error_int = .true.
                            exit pipe_loop
                        end if
                        
                    !                   Section, convergence test
                        norm = sqrt ( &
                            (one - p_sec / pipe(j)%sec(k)%pr) ** 2 + &
                            (one - t_sec / pipe(j)%sec(k)%tp) ** 2 + &
                            (one - old_dfw / dfw) ** 2 + &
                            (one - old_dth / dth) ** 2 )
                      
                        if (norm < tol%rel) exit ode_loop
                else

            !                   Section, no mass transfer
                    dfw = zero
                    dth = zero
                    pipe(j)%sec(k)%mfa = zero
                    pipe(j)%sec(k)%mfd = zero
                    pipe(j)%sec(k)%mfg = zero
                    pipe(j)%sec(k)%mfs = zero

            !                   Section, convergence test
                    norm = sqrt( &
                        (one - p_sec / pipe(j)%sec(k)%pr) ** 2 + &
                        (one - t_sec / pipe(j)%sec(k)%tp) ** 2 )
                    if (norm < tol%rel) exit ode_loop
                end if      
                end do ode_loop

            !           Maximum ODE evaluations achived
                if (ic > tol%max) ode_warg = .true.

            !           Section, update annulus pressure 
                p_a(1) = p_a(2)

            !           Section, update diameter, wax fraction and thickness
                pipe(j)%sec(k)%di = old_di
                pipe(j)%sec(k)%fw = old_fw

                
                
                pipe(j)%sec(k)%th = old_th

            !           Section, Log file report
                write (unit = unit_log, fmt = 30, iostat = iost) j, k, ic, ode_warg
                end do section_loop

            !       Section, lengths last section
                pipe(j)%sec(k)%lp = sum(psp(1:j)%len)
                pipe(j)%sec(k)%dx = zero

            !       Section, pressure and temperature last section
                pipe(j)%sec(k)%pr = p_ds
                pipe(j)%sec(k)%tp = t_ds

            !       Section, PVT properties last section
                call PRO_fluids ( &
                massflow, pipe(j)%sec(k-1)%di, flow, pipe(j)%sec(k)%pr, pipe(j)%sec(k)%tp, pvt, &
                zero, zero, aux0, &
                 pipe(j)%liq(k), inf )

            !       Section, PVT properties error test
                if (inf%er) then

            !           Message
                call LOG_in ( &
                unit_log, '    Integration aborted. Pvt interpolation downstream. ' &
                // trim(inf%nm))

            !           Exit pipe integration loop
                error_int = .true.
                exit pipe_loop
                end if
                end do pipe_loop

            !   Formats
                10  format (5x, 40('*'))    
                20  format (5x, 'Pipe', 5x 'Section', 2x, 'Iterations', 5x, 'Warning')
                30  format (5x, i4, 8x, i4, 8x, i4, 8x, l4)

                ! ***********************************************************************************
                end subroutine


    ! ***************************************************************************************
    ! Define in_pss pseudo steady state integrator subroutine        
        subroutine              int_pss ( &
            massflow, unit_log, &
             psp, pvt, thm, flow, pre_bc, tmp_bc, &
            xwd, xwm, c_finit, alfa, cdif, dt, vk, &
            pipe, error_int )

        ! ***********************************************************************************

        !   Called subroutines
            use Properties,only         :   PRO_wax  

        !	Input variables
                logical,intent(in)          ::  massflow        !   Mass or volumetric flow [-]
                integer(ip),intent(in)      ::  unit_log        !   Log file unit           [-]
                integer(ip),intent(in)      ::  xwd(2)          !   Wax diffusivity selector[-]
                integer(ip),intent(in)      ::  xwm             !   Wax model               [-]

                real(rp),intent(in)         ::  c_finit         !   Initial wax fraction multiplier [-]
                real(rp),intent(in)         ::  alfa            !	Aspect ratio            [-]
                real(rp),intent(in)         ::  cdif            !   Multiplier coefficient  [-]
                real(rp),intent(in)         ::  dt              !   Time step               [s]
                real(rp),intent(in)         ::  flow            !   Flow rate               [kg/s, m**3/s]
                real(rp),intent(in)         ::  pre_bc          !   Pressure bound cond     [Pa]
                real(rp),intent(in)         ::  tmp_bc          !   Temperature bound cond  [K]
                real(rp),intent(in)         ::  vk(2)           !   Venkatesan paramaters   [-]
!  ?\               type(model_type),intent(in) ::  model           !   Model data-type 
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
                type(thm_type),intent(in)   ::  thm             !   THM data-type
                        !   npt                 Pressure points         [-]
                        !   ntt                 Temperature points      [-]
                        !   vis                 Oil-wax vis multipliers [-]
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
                type(psp2_type),intent(in)  ::  psp(:)          !   Pipe section properties data-type
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

        !   Input / Output variables
            logical,intent(out)         ::  error_int       !   ODE solver error status [-]
            type(pp_type),intent(inout) ::  pipe(:,0:)      !   Pipe data-type
                    !   sec                 Pipe section data-type
                    !   sec%lp              Pipeline length         [m]
                    !   sec%di              Inner diameter          [m]
                    !   sec%dx              Section length          [m]
                    !   sec%fw              Wax weigth fraction     [-]
                    !   sec%pr              Pressure                [Pa]
                    !   sec%th              Wax layer thickness     [m]
                    !   sec%ti              Inner diameter temp     [K]
                    !   sec%tp              Bulk temperature        [K] 

                    !   liq                 Liq properties data-type
                    !	liq%den             Density                 [kg/m^3]
                    !	liq%stn             Surface tension         [N/m]
                    !	liq%vel             Superficial velocity    [m/s]
                    !	liq%vis             Viscosity               [Pa*s]
                    !   liq%enh             Enthalpy                [J/kg]
                    !   liq%scp             Specific heat           [J/(kg*K)]
                    !   liq%con             Conductivity            [W/(m*K)]

                    !   prg                 Pressure gradient data-type
                    !   prg%fpc             Flow pattern code       [-]
                    !   prg%hlg             Gas-liq holdup          [-]
                    !   prg%hll             Liq-liq holdup          [-]
                    !   prg%ace             Acc pressure gradient   [Pa/m]
                    !   prg%fri             Fri pressure gradient   [Pa/m]
                    !   prg%gra             Gra pressure gradient   [Pa/m]
                    !   prg%tot             Tot pressure gradient   [Pa/m]
                    !   tpg                 Temperature gradient data-type
                    !   tpg%dtr             Radial temp gradient    [K/m]
                    !   tpg%dtx             Axial temp gradient     [K/m]
                    !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                    !   tpg%qfl             Heat                    [W]
                    !   tpg%thf             Fluid thermal resistance[K*m/W]
                    !   tpg%thw             Wax thermal resistance  [K*m/W]

        !   Internal variables
            character(50)               ::  time            !   Time message            [-]
            integer(ip)                 ::  i, j, k         !   Counters                [-]
            integer(ip)                 ::  np              !   Number of pipes         [-]
            integer(ip)                 ::  ns(size(pipe,1))!   Sections per pipe       [-]
            real(rp)                    ::  run_time(2)     !   CPU time                [s]
            type(inf_type)              ::  inf             !   Numerical report data-type
                    !   inf%er              Error flag              [-]
                    !   inf%it              Number of iterations    [-]
                    !   inf%nm              Error name              [-]
            type(wax_type)              ::  wax             !   Wax properties data-type
                    !   wax%den             Density                 [kg/m^3]
                    !   wax%cfr             Wax concentration frac  [mol/mol]
                    !   wax%enh             Enthalpy                [J/kg]
                    !   wax%mwl             Liq molecular weigth    [kg/kmol]
                    !   wax%mww             Wax molecular weigth    [kg/kmol]
                    !   wax%scp             Specific heat           [J/(kg*K)]
                    !   wax%wfr             Wax fraction            [mol/mol]
                    !   wax%dwf             Wax fraction d.w.r. tp  [mol/(mol*K)]

        ! ***********************************************************************************

                !   Number of pipes
                    np = size(pipe, dim=1)

                !   Number of sections per pipe
                    forall (j = 1:np) ns(j) = size(pipe(j,0)%sec) - 2

                !   Timing begins
                    call CPU_time ( run_time(1) )

                !   Pseudo steady state section message
                    call LOG_in ( unit_log, '' )
                    call LOG_in ( &
                    unit_log, 'PSEUDO STEADY STATE section open' )

                !   Local memory allocation test
                    if (error_int) then

                !       Unsuccessful
                    call LOG_in ( & 
                    unit_log, 'PSEUDO STEADY STATE section aborted' )

                !       Abort PIPE_int_pss subroutine
                    return
                    end if

                !   Pipeline time integration started message
                    call LOG_in ( &
                    unit_log, '    Pipeline time integration started' )

                !   Pipeline time integration message
                    write (time, '(4x, "Simulation time = ", f12.2, " s")') zero
                    call LOG_in ( unit_log, time )

                !   Steady state pipeline integration
                    call int_mh ( &
                    massflow, unit_log, &
                     np, ns, psp, pvt, flow, pre_bc, tmp_bc, &
                    pipe(:,0), error_int )

                !   Steady state pipeline integration test
                    if (error_int) then

                !       Message
                    call LOG_in (unit_log, '    Pipeline time integration fail')

                !       Message
                    call LOG_in (unit_log, 'PSEUDO STEADY STATE section aborted')

                !       Abort PIPE_int_pss subroutine
                    error_int = .true.
                    return
                    end if

                !   Initial wax pipe data
                    pipe_loop: do j = 1, np
                    section_loop: do k = 1, ns(j)

                !           Wax weigth fraction, THM properties
                    call PRO_wax ( &
                    pipe(j,0)%sec(k)%pr, pipe(j,0)%sec(k)%ti, thm, &
                    wax, inf )

              
                !           Wax layer thickness
                    pipe(j,0)%sec(k)%th = 1.e-5_rp

                    end do section_loop
                    end do pipe_loop

                !   Wax time integration loop
                    time_loop: do i = 1, size(pipe, dim=2) - 1

                    

                !       Pipeline time integration message
                    write (time, '(4x, "Integration time = ", f12.2, " s")') dt * real(i, rp)
                    call LOG_in (unit_log, time)

                    
                !       Psedo steady state pipeline integration
                    call int_mhm (&
                    massflow, unit_log, pipe(:,i-1), &
                     np, ns, psp, pvt, thm, flow, &
                    xwd, xwm, c_finit, alfa, cdif, dt, vk, &
                    pipe(:,i), error_int)

                

                !       Wax time integration loop test
                    if (error_int) exit time_loop
                    end do time_loop

                !   Timing ends
                    call CPU_time (run_time(2))

                !   Pseudo steady state section message
                    if (error_int) then

                !       Message
                    call LOG_in (unit_log, '    Pipeline time integration fail')

                !       Message
                    call LOG_in (unit_log, 'PSEUDO STEADY STATE section aborted')
                    else

                !       Timing ends
                    call CPU_time (run_time(2))

                !       Timing display
                    write (time, '(4x, "CPU time: ", f12.4, " s")') run_time(2) - run_time(1) 
                    call LOG_in (unit_log, time)

                !       Message
                    call LOG_in (unit_log, '    Pipeline time integration finished')

                !       Message
                    call LOG_in (unit_log, 'PSEUDO STEADY STATE section close')
                    end if

                ! ***********************************************************************************
            end subroutine



    ! ***************************************************************************************
    ! Define in_ss pseudo steady state integrator subroutine    
        subroutine              int_ss ( &
            massflow, unit_log, &
             psp, pvt, flow, pre_bc, tmp_bc, &
            pipe, error_int )

            ! int_ss (massflow, unit_log, model, &
            ! np, ns, psp, pvt, flow, pre_bc, tmp_bc, &
            ! pipe(:,0), error_int)


        !	Input variables
            logical,intent(in)          ::  massflow        !   Mass or volumetric flow [-]
            integer(ip),intent(in)	    ::	unit_log        !   Log file unit           [-]
            real(rp),intent(in)         ::  flow            !   Flow rate               [kg/s, m**3/s]
            real(rp),intent(in)         ::  pre_bc          !   Pressure bound cond     [Pa]
            real(rp),intent(in)         ::  tmp_bc          !   Temperature bound cond  [K]
     !       type(model_type),intent(in) ::  model           !   Model data-type 
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
            type(psp2_type),intent(in)  ::  psp(:)          !   Pipe section properties data-type
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

            !   Input / Output variables
            logical,intent(out)         ::  error_int       !   ODE solver error status [-]
            type(pp_type),intent(inout) ::  pipe(:)         !   Pipe data-type
                    !   sec                 Pipe section data-type
                    !   sec%lp              Pipeline length         [m]
                    !   sec%di              Inner diameter          [m]
                    !   sec%dx              Section length          [m]
                    !   sec%fw              Wax weigth fraction     [-]
                    !   sec%pr              Pressure                [Pa]
                    !   sec%th              Wax layer thickness     [m]
                    !   sec%ti              Inner diameter temp     [K]
                    !   sec%tp              Bulk temperature        [K] 

                    !   liq                 Liq properties data-type
                    !	liq%den             Density                 [kg/m^3]
                    !	liq%stn             Surface tension         [N/m]
                    !	liq%vel             Superficial velocity    [m/s]
                    !	liq%vis             Viscosity               [Pa*s]
                    !   liq%enh             Enthalpy                [J/kg]
                    !   liq%scp             Specific heat           [J/(kg*K)]
                    !   liq%con             Conductivity            [W/(m*K)]
                    !   film                Film data-type
                    !   film%len            Region length           [m]
                    !   film%ent            Liq entrainment fraction[-]

                    !   film%liq%h          Liq holdup              [-]
                    !   film%liq%v          Liq velocity            [m/s]
                    !   film%liq%f%i        Liq int friction        [-]
                    !   film%liq%f%w        Liq wal friction        [-]
                    !   film%liq%s%i        Liq int perimeter       [-]
                    !   film%liq%s%w        Liq wal perimeter       [-]
                    !   film%liq%t%i        Liq int stress          [Pa]
                    !   film%liq%t%w        Liq wal stress          [Pa]
                    !   slug                Slug data-type
                    !   slug%len            Region length           [m]
                    !   slug%fre            Frecuency               [1/s]
                    !   slug%mix%h          Liq holdup              [-]
                    !   slug%mix%v          Liq velocity            [m/s]
                    !   slug%mix%f%i        Liq int friction        [-]
                    !   slug%mix%f%w        Liq wal friction        [-]
                    !   slug%mix%s%i        Liq int perimeter       [-]
                    !   slug%mix%s%w        Liq wal perimeter       [-]
                    !   slug%mix%t%i        Liq int stress          [Pa]
                    !   slug%mix%t%w        Liq wal stress          [Pa]
                    !   prg                 Pressure gradient data-type
                    !   prg%fpc             Flow pattern code       [-]
                    !   prg%hlg             Gas-liq holdup          [-]
                    !   prg%hll             Liq-liq holdup          [-]
                    !   prg%ace             Acc pressure gradient   [Pa/m]
                    !   prg%fri             Fri pressure gradient   [Pa/m]
                    !   prg%gra             Gra pressure gradient   [Pa/m]
                    !   prg%tot             Tot pressure gradient   [Pa/m]
                    !   tpg                 Temperature gradient data-type
                    !   tpg%dtr             Radial temp gradient    [K/m]
                    !   tpg%dtx             Axial temp gradient     [K/m]
                    !   tpg%htc             Convective coefficient  [W/(m**2*K)]
                    !   tpg%qfl             Heat                    [W]
                    !   tpg%thf             Fluid thermal resistance[K*m/W]
                    !   tpg%thw             Wax thermal resistance  [K*m/W]

        !   Internal variables
            integer(ip)                 ::  j               !   Counter                 [-] 
            integer(ip)                 ::  np              !   Number of pipes         [-]
            integer(ip)                 ::  ns(size(pipe))  !   Sections per pipe       [-]
            character(50)               ::  time            !   Time message            [-]
            real(rp)                    ::  run_time(2)     !   CPU time                [s]

        ! ***********************************************************************************

            

        !   Number of pipes
            np = size(pipe, dim=1)

           
        !   Number of sections per pipe
            forall (j = 1:np) ns(j) = size(pipe(j)%sec) - 2
            
        !   Steady state section message
            call LOG_in ( unit_log, '' )
            call LOG_in ( &
            unit_log, 'STEADY STATE section open' )
            call LOG_in ( &
            unit_log, '    Pipeline integration started' )

            
        !   Timing begins
            call CPU_time ( run_time(1) )

        !   Steady state integrator
            call int_mh ( &
            massflow, unit_log, &
             np, ns, psp, pvt, flow, pre_bc, tmp_bc, &
            pipe, error_int )

            

        !   Timing ends
            call CPU_time ( run_time(2) )


           
        !   Steady state section message
            if (error_int) then

        !       Unsuccessful
            call LOG_in ( &
            unit_log, '    Pipeline integration fail' )
            call LOG_in ( &
            unit_log, 'STEADY STATE section aborted' )
            else

        !       Timing display
            write (time, '(4x, "CPU time: ", f12.4, " s")') run_time(2) - run_time(1) 
            call LOG_in ( unit_log, time )

        !      Successful


            call LOG_in ( &
            unit_log, '    Pipeline integration finished' )
            call LOG_in ( &
            unit_log, 'STEADY STATE section close' )
            end if  

           
        ! ***********************************************************************************
        end subroutine

! ***************************************************************************************
end module
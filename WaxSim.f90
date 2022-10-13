program wax_model

    ! tasks: 



        ! 1. read setup file 
        ! 2. read pvt file 
        ! 3. read wax file
        ! 4. allocate the number of pipes np as the size of the pipes array 
        ! 5. setup the section loop and call the steady state integrator
        ! 6. setup the time loop and call the pseudo ss integrator 
        ! 7. output the data contained in pipe to output.txt 
 
   
! List the modules that are used in this program
        use     constants
        use     reader
        use     properties
        use     integrator

    implicit none 
! Variable Declarations

        integer(ip),parameter       ::	unit_log   = 100     !   Log file unit           [-]
        character(*),parameter      ::  file_log =  'Log file.txt'  !   Log file name       [-]
        character(*),parameter      ::	file_set =  'Setup.txt'     !	Input files name        [-]
        !character(*),parameter      ::	file_ceq =  'Ceq_data.txt'     !	Input files name        [-]
        logical                     ::  error_int       !   ODE solver error status [-]
        real(rp)                    ::  dt
        logical                     ::  error_PVT       !   PVT file error status [-]
        logical                     ::  error_set
        logical                     ::  error_thm
        integer(ip)                 ::  np
        integer(ip)                 ::  j
        logical                     ::  massflow        !   Mass or volumetric     [-]
        ! type(model_type)::  model           !   Model data-type 
        real(rp)                    ::  flow            !   Flow rate               [kg/s, m**3/s]
        real(rp)                    ::  pre_bc          !   Pressure bound cond     [Pa]
        real(rp)                    ::  tmp_bc          !   Temperature bound cond  [K]
        character(2000)             ::  file_out        !   Output file name        [-]
        character(2000)             ::  file_pvt        !   PVT file name           [-]
        character(2000)             ::  file_thm        !   THM file name           [-]
        character(*),parameter      ::	fable_out =  'Fable.txt'     !	Input files name        [-]
        type(pp_type),allocatable   ::  pipe(:,:)       !   Pipe data-type

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

        type(psp2_type),allocatable     ::  psp(:)          !   Pipe section properties data-type

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



        type(pvt_type)                 ::  pvt             !   PVT data-type



        integer(ip),allocatable         ::  ns(:)           !   Number of segments      [-]
        real(rp)                        ::  run_time(2)     !   CPU time                [s]
        character(50)                   ::  time            !   Time message            [-]
        
        real(rp)                        ::  c_finit         !   Initial wax fraction multiplier [-]
        real(rp)                        ::  alfa            !	Aspect ratio            [-]
        real(rp)                        ::  cdif            !   Multiplier coefficient  [-]
        real(rp)                        ::  vk(2)           !   Venkatesan paramaters   [-]
        type(thm_type)                  ::  thm             !   THM data-type
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
        real(rp)                        ::	sf(100)         !   Solid fraction table    [-]
        real(rp)                        ::	tsf(100)        !   Solid fraction tmp table[C]
        integer(ip)                     ::	xsf             !   Solid fraction model    [-]
        integer(ip)                     ::  xwm(2)
        integer(ip)                     ::	xwd(2)	        !	Wax diffusivity selector[-]
        integer(ip)                     ::  i,y, z         !   Counters                [-]
        type(wax_type)                  ::  wax             !   Wax properties data-type
        type(inf_type)              ::  inf             !   Numerical report data-type

  !   Log file opening
   call LOG_open ( file_log, unit_log )

! read setup and pvt file



    call LOG_in (unit_log, '')
    call LOG_in ( &
            unit_log, 'SETUP section opened' )  

    call SET_read_file ( &
        unit_log, file_set, &
        error_set, &
        massflow, flow, pre_bc, tmp_bc, &
        file_out, file_pvt, file_thm, &
        xsf, xwd, xwm, c_finit, alfa, cdif, dt, sf, tsf, vk, &
         pipe, psp )

    call PVT_read_file ( &
        unit_log, file_pvt, &
        pvt, error_pvt )

    

    call THM_read_file ( &
    unit_log, xsf, xwm(2), sf, tsf, file_thm, &
    thm, error_thm )


        !   Timing begins
    call CPU_time ( run_time(1) )


    

! Define np as the number of pipes
    np = size(pipe, dim=1)

    

! allocate space for ns array
    allocate (ns(np))

    
    
! Define ns as the number of sections for each pipe 
    forall (j = 1:np) ns(j) = size(pipe(j,0)%sec) - 2

    

    
! CALL STEADY STATE INTEGRATION 
    Call int_ss ( &
    massflow, unit_log, &
    psp, pvt, flow, pre_bc, tmp_bc, &
    pipe(:,0), error_int )

    

    ! Timing ends
        call CPU_time ( run_time(2) )

    ! Steady state section message
        if (error_int) then
        ! Unsuccessful
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
                    unit_log, 'STEADY STATE section close' )
        end if

     !   Initial wax pipe data
        pipe_loop: do y = 1, np
            section_loop: do z = 1, ns(y)
    !           Wax weigth fraction, THM properties
                call PRO_wax ( &
                    pipe(y,0)%sec(z)%pr, pipe(y,0)%sec(z)%ti, thm, &
                    wax, inf )

! initialize wax fraction here

                    pipe(y,0)%sec(z)%fw = c_finit * wax%wfr
                    

                    print*,"initial was fraction", pipe(y,0)%sec(z)%fw 

                    print*,"fluid temperature", pipe(y,0)%sec(z)%tp
                    
        !           Wax layer thickness
                    pipe(y,0)%sec(z)%th = 1.e-5_rp
            end do section_loop
        end do pipe_loop
	!   Timing starts
    call CPU_time ( run_time(1) )
      
! CALL PSEUDO-STEADY STATE INTEGRATION
    time_loop: do i = 1, size(pipe, dim=2) - 1

    

    !   Pipeline time integration message
        write (time, '(4x, "Integration time = ", f12.2, " s")') dt * real(i, rp)
        call LOG_in (unit_log, time)

        

        !call int_pss ( &
        !massflow, unit_log, &
         !psp, pvt, thm, flow, pre_bc, tmp_bc, &
        !xwd, xwm(2), alfa, cdif, dt, vk, &
        !pipe, error_int )
	
	call int_mhm ( &
                    massflow, unit_log, pipe(:,i-1), &
                     np, ns, psp, pvt, thm, flow, &
                    xwd, xwm(2), c_finit, alfa, cdif, dt, vk, &
                    pipe(:,i), error_int )

        if (error_int) exit time_loop

       
        
    end do time_loop

! Write to log file processing time and integration errors
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
! Call fable_output for pipeline data to be written to text file

            Call  fable_output(dt,error_int,unit_log,fable_out,pipe)

            
end program 



module reader

    !   List the used modules
        use                 ::  constants
    
    !   Variables must be declared
        implicit none
    
    !   Private subroutines and variables
        private
    
    !   Module public subroutines
        public              ::  SET_dealloc
        public              ::  SET_read_file
        public              ::  THM_dealloc
        public              ::  THM_read_file
        public              ::  PVT_dealloc
        public              ::  PVT_read_file
        public              ::  fable_output
       
    
    
    
    !   Module subroutines
        contains
     ! ***************************************************************************************
    ! Define THM_dealloc to deallocate thm array between runs
        subroutine              THM_dealloc ( &
            unit_log, &
            thm )

    !	Input variables
    integer(ip),intent(in)      ::  unit_log        !   File unit               [-]

    !   Output variables
    type(thm_type),intent(inout)::  thm             !   THM data-type
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

    !	Internal variables
    integer(ip)                 ::  alloc_st(4)     !   Allocation status       [-]

    ! ***********************************************************************************

    !   Thm memory deallocation
    deallocate (thm%pr, stat = alloc_st(1))
    deallocate (thm%tp, stat = alloc_st(2))
    deallocate (thm%tpc, stat = alloc_st(3))
    deallocate (thm%wax, stat = alloc_st(4))

    !   Thm memory deallocation message
    if (alloc_st(1) == 0 .and. &
    alloc_st(2) == 0 .and. &
    alloc_st(3) == 0 .and. &
    alloc_st(4) == 0) then

    !       Successful 
    call LOG_in ( &
    unit_log, '    THM memory deallocated')
    end if

    ! ***********************************************************************************
    end subroutine
    ! ***************************************************************************************
    ! Define the SET_alloc subroutine to allocate pipe array
        subroutine              SET_alloc ( &
                                    unit_log, nt, &
                                    sec, ang, dia, lgh, rou, tmp, ypr, ynm, ymt, ydi, ytr, &
                                    pipe, psp, error_set )
  
    
        !	Input variables
            integer(ip),intent(in)      ::  unit_log        !   File unit               [-]
            integer(ip),intent(in)      ::  nt              !   Time steps              [-]
            integer(ip),intent(in)      ::  sec(:)          !   Sections per pipe       [-]        
            real(rp),intent(in)         ::  ang(:)          !   Pipe angle              [rad]
            real(rp),intent(in)         ::  dia(:)          !   Pipe inner diameter     [m]
            real(rp),intent(in)         ::  lgh(:)          !   Pipe length             [m]
            real(rp),intent(in)         ::  rou(:)          !   Pipe roughness          [m]
            real(rp),intent(in)         ::  tmp(:)          !   Outer temperature       [K]
            real(rp),intent(in)         ::  ypr(:)          !   Ref pressure            [Pa]
            integer(ip),intent(in)      ::  ynm(:,:)        !   Layer code name         [-]
            integer(ip),intent(in)      ::  ymt(:,:)        !   Layer material code     [-]
            real(rp),intent(in)         ::  ydi(:,:)        !   Layer diameter          [m]
            real(rp),intent(in)         ::  ytr(:,:)        !   Layer resistance        [K*m/W]
    
        !	Output variables
            logical,intent(out)         ::  error_set       !   Allocation status       [-]
            type(pp_type),intent(out), &
                allocatable             ::  pipe(:,:)       !   Pipe data-type
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
            type(psp2_type),intent(out), &
                allocatable             ::  psp(:)          !   Pipe section properties data-type
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
    
        !	Internal variables
            integer(ip)                 ::  alloc_st(9)     !   Allocation status       [-] 
            integer(ip)                 ::  i, j            !   Counters                [-]
            integer(ip)                 ::  np              !   Number of pipes         [-]
    
        ! ***********************************************************************************
    
        !   Search loop
            do j = 1, size(sec)

                if (sec(j) == 0) exit
            end do

        !   Number of pipes
            np = j - 1

        !   Pipe memory allocation
            allocate (pipe(np,0:nt), stat = alloc_st(1))
            allocate (psp(1:np), stat = alloc_st(2))

        !   Allocation status initial value
            error_set = .true.

        !   Pipe allocation test
            if (alloc_st(1) == 0 .and. alloc_st(2) == 0) then

        !       Successful pipe allocation
                error_set = .false.

        !       Pipe loop
                pipe_loop: do j = 1, np

        !           Pipe properties initialization
                    psp(j)%ang = ang(j)
                    psp(j)%dia = dia(j)
                    psp(j)%len = lgh(j)
                    psp(j)%rou = rou(j) / dia(j)  !   Relative section roughness
                    psp(j)%tpo = tmp(j)
                    psp(j)%ypr = ypr(j)
                    psp(j)%ynm(:) = ynm(:,j)
                    psp(j)%ymt(:) = ymt(:,j)
                    psp(j)%ydi(:) = ydi(:,j)
                    psp(j)%ytr(:) = ytr(:,j)

        !           Time loop
                    time_loop: do i = 0, nt

        !               Pipe sections memory allocation
                        allocate (pipe(j,i)%sec(0:sec(j)+1), stat = alloc_st(3))
                        allocate (pipe(j,i)%liq(0:sec(j)+1), stat = alloc_st(4))
                        allocate (pipe(j,i)%prg(sec(j)), stat = alloc_st(5))
                        allocate (pipe(j,i)%tpg(sec(j)), stat = alloc_st(6))

        !               Pipe sections, time initilization
                        if (alloc_st(3) /= 0 .or. &
                            alloc_st(4) /= 0 .or. &
                            alloc_st(5) /= 0 .or. &
                            alloc_st(6) /= 0)  then

        !                   Unsuccessful
                            error_set = .true.
                            exit pipe_loop
                        else

        !                   Successful
                            pipe(j,i)%sec = sec_type &
                                (zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero, zero)
                            pipe(j,i)%liq = f2_type &
                                (zero, zero, zero, zero, zero, zero, zero, zero)
                            pipe(j,i)%prg = prg_type &
                                (zero, zero, zero, zero, zero, zero, zero)
                            pipe(j,i)%tpg = tpg_type &
                                (zero, zero, zero, zero, zero, zero)
                        end if
                    end do time_loop
                end do pipe_loop
            end if

        !   Pipe memory allocation message
            if (error_set) then

        !       Unsuccessful 
                call LOG_in ( &
                    unit_log, '    Pipe memory cannot be allocated' )
                error_set = .false.
            else

        !       Successful 
                call LOG_in ( &
                    unit_log, '    Pipe memory allocated' )
                error_set = .false.
            end if
        
            ! ***********************************************************************************
        end subroutine
        
    ! ***************************************************************************************
    ! Define SET_dealloc subroutine to deallocate pipe array between runs
        subroutine	    SET_dealloc ( &
                                    unit_log, &
                                    pipe, psp )

    
        !	Input variables
            integer(ip),intent(in)      ::  unit_log        !   File unit               [-]
    
        !   Output variables
            type(pp_type),intent(inout), &          
                allocatable             ::  pipe(:,:)       !   Pipe data-type
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
            type(psp2_type),intent(inout), &
                allocatable             ::  psp(:)          !   Pipe section properties data-type
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
    
        !	Internal variables
            integer(ip)                 ::  alloc_st(2)     !   Allocation status       [-] 
    
        ! ***********************************************************************************
    
        !   Pipe memory deallocation test
            deallocate (pipe, stat = alloc_st(1))
            deallocate (psp, stat = alloc_st(2))
    
        !   Pipe memory deallocation message
            if (alloc_st(1) == 0 .and. &
                alloc_st(2) == 0) then
    
        !       Successful 
                call LOG_in ( &
                    unit_log, '    Pipe memory deallocated' )
            end if
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    ! Define SET_read_file subroutine to read the setup file
        subroutine	    SET_read_file ( &
                                    unit_log, file_set, &
                                    error_set, &
                                    massflow, flow, pre_bc, tmp_bc, &
                                    file_out, file_pvt, file_thm, &
                                    xsf, xwd, xwm, c_finit, alfa, cdif, dt, sf, tsf, vk, &
                                     pipe, psp )
                                   
    
        !   Parameters
            integer(ip),parameter       ::	np = 100        !   Maximum number of pipes [-]
    
        !	Input variables
            integer(ip),intent(in)      ::	unit_log        !   Log file unit           [-]
            character(*),intent(in)     ::	file_set        !	Input files name        [-]
    
        !	Output variables
            logical,intent(out)         ::	error_set       !   Setup file error status [-]
            logical,intent(out)         ::  massflow        !   Mass or volumetric flow [-]
            integer(ip),intent(out)     ::	xsf             !   Solid fraction model    [-]
            integer(ip),intent(out)     ::	xwd(2)	        !	Wax diffusivity selector[-]
            integer(ip),intent(out)     ::	xwm(2)	        !	Wax model
            real(rp), intent(out)       ::  c_finit     !   Initial wax fraction multiplier [-]        [-]
            real(rp),intent(out)        ::  alfa            !	Aspect ratio            [-]
            real(rp),intent(out)        ::  cdif            !   Multiplier coefficient  [-]
            real(rp),intent(out)        ::  dt              !   Time step               [s]
            real(rp),intent(out)        ::  flow            !   Flow rate               [kg/s, m**3/s]
            real(rp),intent(out)        ::  pre_bc          !   Pressure bound cond     [Pa]
            real(rp),intent(out)        ::  tmp_bc          !   Temperature bound cond  [K]
            real(rp),intent(out)        ::	sf(100)         !   Solid fraction table    [-]
            real(rp),intent(out)        ::	tsf(100)        !   Solid fraction tmp table[C]
            real(rp),intent(out)        ::  vk(2)           !   Venkatesan paramaters   [-]
            character(2000),intent(out) ::  file_out        !   Output file name        [-]
            character(2000),intent(out) ::  file_pvt        !   PVT file name           [-]
            character(2000),intent(out) ::  file_thm        !   THM file name           [-]
            ! type(model_type),intent(out)::  model           !   Model data-type
            !                             !   name                Model name              [-]
                           
            type(pp_type),intent(out), &
                allocatable             ::  pipe(:,:)       !   Pipe data-type
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
            type(psp2_type),intent(out), &
                allocatable             ::  psp(:)          !   Pipe section properties data-type
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
    
        !	Internal variables
            logical				        ::	set_exists      !   Setup file exists status[-]
            integer(ip)                 ::	iost            !   File I/O status         [-]
            integer(ip)                 ::  nt              !   Time steps              [-]
            integer(ip)                 ::  sec(np)         !   Sections per pipe       [-] 
            integer                     ::  unit_set        !   File unit               [-]
            real(rp)                    ::  t_end           !   Final time              [s]
            real(rp)                    ::  ang(np)         !   Pipe angle              [rad]
            real(rp)                    ::  dia(np)         !   Pipe inner diameter     [m]
            real(rp)                    ::  lgh(np)         !   Pipe length             [m]
            real(rp)                    ::  rou(np)         !   Pipe roughness          [m]
            real(rp)                    ::  tmp(np)         !   Outer temperature       [K]
            real(rp)                    ::  ypr(np)         !   Ref pressure            [Pa]
            integer(ip)                 ::  ynm(6,np)       !   Layer code name         [-]
            integer(ip)                 ::  ymt(6,np)       !   Layer material code     [-]
            real(rp)                    ::  ydi(6,np)       !   Layer diameter          [m]
            real(rp)                    ::  ytr(6,np)       !   Layer resistance        [K*m/W]
    
        !   Name list definitions
            namelist    /boundary/          massflow, flow, pre_bc, tmp_bc
            namelist    /files_names/       file_out, file_pvt, file_thm
            namelist    /mass_setup/        xsf, xwd, xwm, c_finit, alfa, cdif, dt, sf, tsf, t_end, vk
            ! namelist    /mom_setup/         model
            namelist    /pipes/             sec, ang, dia, lgh, rou, tmp, &
                                            ypr, ynm, ymt, ydi, ytr
        ! ***********************************************************************************
    
        !   Pipe sections initialization
            sec = 0
    
        !   Setup file inquire
            inquire (file = file_set, exist = set_exists)
    
        !   Setup file inquire message
            if (set_exists) then
    
        !       Successful 
                call LOG_in ( &
                    unit_log, '    Setup file found: ' // trim(file_set) )
                error_set = .false.
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Setup file not found: ' // trim(file_set) )
    
        !       Abort SET_read_file subroutine
                error_set = .true.
                return
            end if
    
        !   Setup file opening
            open (newunit = unit_set, file = file_set, status = 'old', iostat = iost)
    
        !   Setup file opening message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Setup file opened' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Setup file cannot be opened' )
    
        !       Abort SET_read_file subroutine
                error_set = .true.
                return
            end if
    
        !   Setup file header jump
            read (unit = unit_set , fmt = '(6/)')           
    
        !   Files names section
            read (unit = unit_set, nml = files_names, iostat = iost)
    
        !   Files section message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Files names section read' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Files names section cannot be read' )
    
        !       Abort SET_read_file subroutine
                error_set = .true.
                return
            end if 
    
        ! !   Momentum section
        !     read ( unit = unit_set, nml = mom_setup, iostat = iost )
    
        ! !   Momentum section message
        !     if (iost == 0) then
    
        ! !       Successful
        !         call LOG_in ( &
        !             unit_log, '    Momentum section read' )
        !     else
    
        ! !       Unsuccessful
        !         call LOG_in ( &
        !             unit_log, '    Momentum section cannot be read' )
    
        ! !       Abort SET_read_file subroutine
        !         error_set = .true.
        !         return
        !     end if
    
        !   Mass section
            read ( unit = unit_set, nml = mass_setup, iostat = iost )
    
        !   Mass section message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Mass section read' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Mass section cannot be read' )
    
        !       Abort SET_read_file subroutine
                error_set = .true.
                return
            end if
    
        !   Pipes section
            read ( unit = unit_set, nml = pipes, iostat = iost )
    
        !   Pipes section message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Pipes section read' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pipes section cannot be read' )
    
        !       Abort SET_read_file subroutine
                error_set = .true.
                return
            end if
    
        !   Boundary section
            read ( unit = unit_set, nml = boundary, iostat = iost )
    
        !   Boundary section message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Boundary section read' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Boundary section cannot be read' )
    
        !       Abort SET_read_file subroutine
                error_set = .true.
                return
            end if
    
        ! !   Time steps for wax deposition
        !     if (xwm(1) > -1) then
    
        !       Pseudo steady state (wax deposition)
                nt = int(t_end / dt, ip)
            ! else
    
        !       Steady steate
            !     nt = 0
            ! end if
    
        !   Pipe memory allocation
            call SET_alloc ( &
                unit_log, nt, &
                sec, ang, dia, lgh, rou, tmp, ypr, ynm, ymt, ydi, ytr, &
                pipe, psp, error_set )
    
        !   Setup file closing
            close (unit = unit_set, iostat = iost)
    
        !   Setup file closing message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Setup file closed' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Setup file cannot be closed' )
    
        !       Abort SET_read_file subroutine
                error_set = .true.
                return
            end if
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    ! Define subroutine PVT_alloc to allocate pvt array for reading the tab file
        subroutine              PVT_alloc ( &
                                    unit_log, npt, ntt, &
                                    pvt, error_pvt )


        !	Input variables
            integer(ip),intent(in)      ::  unit_log        !   File unit               [-]
            integer(ip),intent(in)      ::  npt             !   Pressure array length   [-]
            integer(ip),intent(in)      ::  ntt             !   Temperature array length[-]

        !   Output variables
            logical,intent(out)         ::  error_pvt       !   Allocation status       [-]
            type(pvt_type),intent(out)  ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]
        
                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]

        !	Internal variables
            integer(ip)                 ::  alloc_st(5)     !   Allocation status       [-]

        ! ***********************************************************************************

        !   Pvt pressute and temperature dimensions
            pvt%npt = npt
            pvt%ntt = ntt

        !   Pvt memory allocation
            allocate (pvt%pr(npt), stat = alloc_st(1))
            allocate (pvt%tp(ntt), stat = alloc_st(2))
            allocate (pvt%enh(npt,ntt), stat = alloc_st(3))
            allocate (pvt%gas(npt,ntt), stat = alloc_st(4))
            allocate (pvt%liq(npt,ntt), stat = alloc_st(5))

        !   Pvt memory allocation message
            if (alloc_st(1) == 0 .and. &
                alloc_st(2) == 0 .and. &
                alloc_st(3) == 0 .and. &
                alloc_st(4) == 0 .and. &
                alloc_st(5) == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Pvt memory allocated' )
                error_pvt = .false.
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pvt memory cannot be allocated' )
                error_pvt = .true.
            end if

        ! ***********************************************************************************
        end subroutine
    ! ***************************************************************************************
    ! Define subroutine PVT_dealloc to deallocate pvt array between runs
        subroutine			    PVT_dealloc ( &
                                    unit_log, &
                                    pvt )


        !	Input variables
            integer(ip),intent(in)      ::  unit_log        !   File unit               [-]

        !   Output variables
            type(pvt_type),intent(inout)::  pvt             !   PVT data-type
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

        !	Internal variables
            integer(ip)                 ::  alloc_st(5)     !   Allocation status       [-]

        ! ***********************************************************************************

        !   Pvt memory deallocation test
            deallocate (pvt%pr, stat = alloc_st(1))
            deallocate (pvt%tp, stat = alloc_st(2))
            deallocate (pvt%enh, stat = alloc_st(3))
            deallocate (pvt%gas, stat = alloc_st(4))
            deallocate (pvt%liq, stat = alloc_st(5))

        !   Pvt memory deallocation message
            if (alloc_st(1) == 0 .and. &
                alloc_st(2) == 0 .and. &
                alloc_st(3) == 0 .and. &
                alloc_st(4) == 0 .and. &
                alloc_st(5) == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Pvt memory deallocated' )
            end if

    
        end subroutine

    ! ***************************************************************************************
    ! Define PVT_read_file to read in pvt data from file_pvt
        subroutine              PVT_read_file ( &
                                    unit_log, file_pvt, &
                                    pvt, error_pvt )


        !	Input variables
            integer(ip),intent(in)      ::	unit_log        !   Log file unit           [-]
            character(2000),intent(in)  ::  file_pvt        !   PVT file name           [-]

        !	Output variables
            logical,intent(out)         ::	error_pvt       !   PVT file error status   [-]
            type(pvt_type),intent(out)  ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]

                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]

        !	Internal variables
            logical				        ::	pvt_exists      !   PVT file exists status  [-]
            integer(ip)                 ::	iost            !   File I/O status         [-]
            integer                     ::  unit_pvt        !   File unit               [-]
            character(100)              ::  pvt_header      !   PVT file header         [-]

        ! ***********************************************************************************
            
        !   Pvt file inquire
            inquire (file = file_pvt, exist = pvt_exists)


        !   Pvt file inquire message
            if (pvt_exists) then
                
        !       Successful 
                call LOG_in ( &
                    unit_log, '    Pvt file found: ' // trim(file_pvt) )
                error_pvt = .false.
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pvt file not found: ' // trim(file_pvt) )

        !       Abort PVT_read_file subroutine
                error_pvt = .true.
                return
            end if

        !   Pvt file opening
            open (newunit = unit_pvt, file = file_pvt, status = 'old', iostat = iost)

        !   Pvt file opening message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Pvt file opened' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pvt file cannot be opened' )

        !       Abort PVT_read_file subroutine
                error_pvt = .true.
                return
            end if

        !   Pvt file type selection
            read ( unit = unit_pvt, fmt = '(a100)', iostat = iost ) pvt_header

        !   Pvt file message
            if (iost == 0) then

        !       File type
                if (index(trim(pvt_header), 'PVTTABLE LABEL') > 0) then

        !           Successful KEY
                    call LOG_in ( &
                        unit_log, '    KEY file format' )

        !           File read
                    call PVT_read_key ( &
                        unit_log, unit_pvt, pvt, error_pvt )
                else if (index(trim(pvt_header), 'ENTROPY NONEQ') > 0) then

        !           Successful uniform FIX
                    call LOG_in ( &
                        unit_log, '    Uniform FIX file format' )

        !           File read
                    call PVT_read_fix ( &
                        .true., unit_log, unit_pvt, pvt, error_pvt )
                else

        !           Successful non-uniform FIX
                    call LOG_in ( &
                        unit_log, '    Non-uniform FIX file format' )

        !           File read
                    call PVT_read_fix ( &
                        .false., unit_log, unit_pvt, pvt, error_pvt )
                end if
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Tab file header format cannot be read' )

        !       Abort PVT_read_file subroutine
                error_pvt = .true.
                return
            end if

        !   Abort PVT_read_file subroutine test
            if (error_pvt) return

        !   Mixture enthalpy
        !   pvt%enh =  pvt%gas%stn * pvt%gas%enh + (one - pvt%gas%stn) * pvt%liq%enh

        !   Liq thermal expansion coeficient
            pvt%liq%epc = -pvt%liq%epc / pvt%liq%den

        !   Pvt file closing
            close (unit = unit_pvt, iostat = iost)

        !   Pvt file closing message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Pvt file closed' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pvt file cannot be closed' )
                error_pvt = .true.
            end if

        ! ***********************************************************************************
        end subroutine

    ! ***************************************************************************************
    ! Define PVT_read_fix to read fixed format pvt files
        subroutine		 PVT_read_fix ( &
                                    pvt_noneq, unit_log, unit_pvt, &
                                    pvt, error_pvt )


        !	Input variables
            logical,intent(in)          ::	pvt_noneq       !   PVT file p&t format     [-]
            integer(ip),intent(in)      ::  unit_log        !   Log file unit           [-]
            integer(ip),intent(in)	    ::	unit_pvt        !   PVT file unit           [-]

        !   Input / Output variables
            logical,intent(out)         ::	error_pvt       !   PVT file error status   [-]
            type(pvt_type),intent(out)  ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]

                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]

        !	Internal variables
            integer(ip)                 ::  alloc_st(6)     !   Allocation status       [-]
            integer(ip)		            ::	i, j		    !	Counters                [-]
            integer(ip)                 ::	iost            !   File I/O status         [-]
            integer(ip)                 ::  npt             !   Pressure array length   [-]
            integer(ip)                 ::  ntt             !   Temperature array length[-]
            real(rp)                    ::  dp, dt          !   PVT file p&t increments [-]
            real(rp)                    ::  p_i, t_i        !   PVT file p&t initials   [-]
            character(50)               ::  tab_header      !   PVT file header         [-]
            real(rp),allocatable        ::  pb(:)           !   Bubble pressure         [Pa]
            real(rp),allocatable        ::  pd(:)           !   DEW pressure            [Pa]
            real(rp),allocatable        ::  ddeng_p(:,:)    !   Gas density d.w.r. pr   [kg/(m**3*Pa)]
            real(rp),allocatable        ::  ddenl_p(:,:)    !   Liq density d.w.r. pr   [kg/(m**3*Pa)]
            real(rp),allocatable        ::  ensg(:,:)       !   Gas entropy             [J/K]
            real(rp),allocatable        ::  ensl(:,:)       !   Liq entropy             [J/K]
            
        ! ***********************************************************************************

        !   Pressure and temperature points
            read ( unit = unit_pvt, fmt = *, iostat = iost ) npt, ntt

        !   Pressure and temperature points message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Pressure and temperature points read' )
                error_pvt = .false.
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pressure and temperature points cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Pvt memory allocation
            call PVT_alloc ( &
                unit_log, npt, ntt, pvt, error_pvt )

        !   Abort PVT_read_fix subroutine test
            if (error_pvt) return

        !   Local memory allocation
            allocate (pb(ntt), stat = alloc_st(1))
            allocate (pd(ntt), stat = alloc_st(2))
            allocate (ddeng_p(npt,ntt), stat = alloc_st(3))
            allocate (ddenl_p(npt,ntt), stat = alloc_st(4))
            allocate (ensg(npt,ntt), stat = alloc_st(5))
            allocate (ensl(npt,ntt), stat = alloc_st(6))

        !   Local memory allocation message
            if (alloc_st(1) == 0 .and. &
                alloc_st(2) == 0 .and. &
                alloc_st(3) == 0 .and. &
                alloc_st(4) == 0 .and. &
                alloc_st(5) == 0 .and. &
                alloc_st(6) == 0) then
            
        !       Successful
                call LOG_in ( &
                    unit_log, '    Local memory allocated' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Local memory cannot be allocated' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Pressure and temperature
            if (pvt_noneq) then

        !       Pressure array
                read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                    (pvt%pr(i), i = 1, npt)

        !       Pressure array message
                if (iost == 0) then

        !           Successful
                    call LOG_in ( &
                        unit_log, '    Pressure array read' )
                else

        !           Unsuccessful
                    call LOG_in ( &
                        unit_log, '    Pressure array cannot be read' )

        !           Abort PVT_read_fix subroutine
                    error_pvt = .true.
                    return
                end if

        !       Temperature array
                read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                    (pvt%tp(i), i = 1, ntt)

        !       Temperature array message
                if (iost == 0) then

        !           Successful
                    call LOG_in ( &
                        unit_log, '    Temperature array read' )
                else

        !           Unsuccessful
                    call LOG_in ( &
                        unit_log, '    Temperature array cannot be read' )

        !           Abort PVT_read_fix subroutine
                    error_pvt = .true.
                    return
                end if
            else

        !       Pressure and temperature increments
                read (unit = unit_pvt, fmt = *, iostat = iost) dp, dt

        !       Pressure and temperature increments message
                if (iost == 0) then

        !           Successful
                    call LOG_in ( &
                        unit_log, '    Pressure and temperature increments read' )
                else

        !           Unsuccessful
                    call LOG_in ( &
                        unit_log, '    Pressure and temperature increments cannot be read' )

        !           Abort PVT_read_fix subroutine
                    error_pvt = .true.
                    return
                end if

        !       Pressure and temperature initial points
                read ( unit = unit_pvt, fmt = *, iostat = iost ) p_i, t_i

        !       Pressure and temperature initial points message
                if (iost == 0) then

        !           Successful
                    call LOG_in ( &
                        unit_log, '    Pressure and temperature initial points read' )
                else

        !           Unsuccessful
                    call LOG_in ( &
                        unit_log, '    Pressure and temperature initial points cannot be read' )

        !           Abort PVT_read_fix subroutine
                    error_pvt = .true.
                    return
                end if

        !       Pressure array calculation
                do i = 1, npt

                    pvt%pr(i) = p_i + real(i-1, rp) * dp
                end do

        !       Temperature array calculation
                do i = 1, ntt

                    pvt%tp(i) = t_i + real(i-1, rp) * dt
                end do
            end if

        !   Bubble point pressure array
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                (pb(i), i = 1, ntt)

        !   Bubble point pressure array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Bubble point array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Bubble point array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Dew point pressure array
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                (pd(i), i = 1, ntt)

        !   Dew point pressure array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    DEW point pressure array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    DEW point pressure array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas density array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%gas(i,j)%den, j = 1, ntt), i = 1, npt)

        !   Gas density array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas density array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas density array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq density array
            read (unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%liq(i,j)%den, j = 1, ntt), i = 1, npt)

        !   Liq density array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq density array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq density array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas density d.w.r. pressure array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((ddeng_p(i,j), j = 1, ntt), i = 1, npt)

        !  Gas density d.w.r. pressure array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas density d.w.r. pressure array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas density d.w.r. pressure array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq density d.w.r. pressure array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((ddenl_p(i,j), j = 1, ntt), i = 1, npt)

        !   Liq density d.w.r. pressure array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq density d.w.r. pressure array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq density d.w.r. pressure array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas density d.w.r. temperature array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%gas(i,j)%epc, j = 1, ntt), i = 1, npt)

        !   Gas density d.w.r. temperature array message
            if (iost == 0) then

            !   Successful
                call LOG_in ( &
                    unit_log, '    Gas density d.w.r. temperature array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas density d.w.r. temperature array cannot be read' )

                !  Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq density d.w.r. temperature array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%liq(i,j)%epc, j = 1, ntt), i = 1, npt)

        !   Liq density d.w.r. temperature array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq density d.w.r. temperature array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq density d.w.r. temperature array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas mass fraction array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%gas(i,j)%stn, j = 1, ntt), i = 1, npt)

        !   Gas mass fraction array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas mass fraction array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas mass fraction array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas viscosity array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%gas(i,j)%vis, j = 1, ntt), i = 1, npt)

        !   Gas viscosity array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas viscosity array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas viscosity array cannot be read' )

           !   Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq viscosity array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%liq(i,j)%vis, j = 1, ntt), i = 1, npt)

        !   Liq viscosity array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq viscosity array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq viscosity array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas specific heat array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%gas(i,j)%scp, j = 1, ntt), i = 1, npt)

        !   Gas specific heat array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas specific heat array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas specific heat array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq specific heat array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%liq(i,j)%scp, j = 1, ntt), i = 1, npt)

        !   Liq specific heat array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq specific heat array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq specific heat array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas enthalpy array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%gas(i,j)%enh, j = 1, ntt), i = 1, npt)

        ! !   Gas enthalpy array message
             if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas enthalpy array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas enthalpy array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq enthalpy array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%liq(i,j)%enh, j = 1, ntt), i = 1, npt)

        !   Liq enthalpy array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq enthalpy array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq enthalpy array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas thermal conductivity array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%gas(i,j)%con, j = 1, ntt), i = 1, npt)

        ! !   Gas thermal conductivity array message
             if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas thermal conductivity array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas thermal conductivity array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq thermal conductivity array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%liq(i,j)%con, j = 1, ntt), i = 1, npt)

        !   Liq thermal conductivity array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq thermal conductivity array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq thermal conductivity array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq surface tension array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((pvt%liq(i,j)%stn, j = 1, ntt), i = 1, npt)

        !   Liq surface tension array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq surface tension array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Liq surface tension array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Gas entropy array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((ensg(i,j), j = 1, ntt), i = 1, npt)

        !   Gas entropy array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Gas entropy array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Gas entropy array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Liq entropy array
            read ( unit = unit_pvt, fmt = '(a50)', iostat = iost ) tab_header
            read ( unit = unit_pvt, fmt = *, iostat = iost ) &
                ((ensl(i,j), j = 1, ntt), i = 1, npt)

        !   Liq entropy array message
            if (iost == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Liq entropy array read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                unit_log, '    Liq entropy array cannot be read' )

        !       Abort PVT_read_fix subroutine
                error_pvt = .true.
                return
            end if

        !   Temperature array from [C] to [K]
            pvt%tp = pvt%tp + 273.15_rp

        !   Local memory deallocation
            deallocate (pb, stat = alloc_st(1))
            deallocate (pd, stat = alloc_st(2))
            deallocate (ddeng_p, stat = alloc_st(3))
            deallocate (ddenl_p, stat = alloc_st(4))
            deallocate (ensg, stat = alloc_st(5))
            deallocate (ensl, stat = alloc_st(6))

        !   Local memory deallocation message
            if (alloc_st(1) == 0 .and. &
                alloc_st(2) == 0 .and. &
                alloc_st(3) == 0 .and. &
                alloc_st(4) == 0 .and. &
                alloc_st(5) == 0 .and. &
                alloc_st(6) == 0) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Local memory deallocated' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Local memory cannot be deallocated' )
            end if

        ! ***********************************************************************************
        end subroutine

    ! ***************************************************************************************
    ! Define PVT_read_key to read key format pvt files
        subroutine    PVT_read_key ( &
                                    unit_log, unit_pvt, &
                                    pvt, error_pvt )

        !	Input variables
            integer(ip),intent(in)      ::  unit_log        !   Log file unit           [-]
            integer(ip),intent(in)	    ::	unit_pvt        !   PVT file unit           [-]

        !   Input / Output variables
            logical,intent(out)         ::	error_pvt       !   PVT file error status   [-]
            type(pvt_type),intent(out)  ::  pvt             !   PVT data-type
                                        !   npt                 Pressure points         [-]
                                        !   ntt                 Temperature points      [-]
                                        !   p                   Pressure array          [Pa]
                                        !   t                   Temperature array       [K]
                                        !   enh                 Total enthalpy          [J/kg]
                                        !   gas                 Gas properties data-type


                                        !   liq                 Liq properties data-type
                                        !	den                 Density                 [kg/m^3]
                                        !	stn                 Surface tension         [N/m]
                                        !	vel                 Superficial velocity    [m/s]
                                        !	vis                 Viscosity               [Pa*s]
                                        !   liq%con             Conductivity            [W/(m*K)]
                                        !   liq%enh             Enthalpy                [J/kg]
                                        !   liq%epc             Expansion coefficient   [1/K]
                                        !   liq%scp             Specific heat           [J/(kg*K)]

        !	Internal variables
            character(2000)             ::  tab_header(2)   !   PVT file header         [-]
            integer(ip)		            ::	i, j, k         !	Counters                [-]
            integer(ip)                 ::	iost            !   File I/O status         [-]
            integer(ip)                 ::  npt             !   Pressure array length   [-]
            integer(ip)                 ::  ntt             !   Temperature array length[-]
            real(rp)                    ::  aux(100)        !   Auxiliar array          [-]

        ! ***********************************************************************************

        !   Scans key file
            pvt_loop: do i = 1, 30

        !       Header 1 read 
                read ( unit = unit_pvt, fmt = '(a2000)', iostat = iost ) tab_header(1)

        !       Header 1 read test
                if (iost == 0) then

        !           Pressure section
                    if (index(trim(tab_header(1)), 'PRESSURE') == 1) then

        !               Header 2 read
                        read ( unit = unit_pvt, fmt = '(a2000)', iostat = iost ) tab_header(2)

        !               Pressure points
                        call PVT_line ( &
                            .true., 100, tab_header, npt, iost, aux )

        !               Pressure points message
                        if (iost == 0) then

        !                   Successful 
                            call LOG_in ( &
                                unit_log, '    Pressure points read' )
                            error_pvt = .false.

        !                   Jump back one line
                            backspace (unit = unit_pvt, iostat = iost)
                        else

        !                   Unsuccessful
                            call LOG_in ( &
                                unit_log, '    Pressure points cannot be read' )

        !                   Abort PVT_read_key subroutine
                            error_pvt = .true.
                            return
                        end if
                    end if

        !           Temperature section
                    if (index(trim(tab_header(1)), 'TEMPERATURE') == 1) then

        !               Temperature points
                        call PVT_line ( &
                            .true., 100, tab_header, ntt, iost, aux )

        !               Temperature points message
                        if (iost == 0) then

        !                   Successful
                            call LOG_in ( &
                                unit_log, '    Temperature points read' )
                            error_pvt = .false.
                        else

        !                   Unsuccessful
                            call LOG_in ( &
                                unit_log, '    Temperature points cannot be read' )

        !                   Abort PVT_read_key subroutine
                            error_pvt = .true.
                            return
                        end if
                    end if 

        !           PVT table point section
                    if (index(trim(tab_header(1)), 'PVTTABLE POINT') == 1) exit pvt_loop
                else

        !           Unsuccessful
                    call LOG_in ( &
                        unit_log, '    Incomplete pvt header read' )

        !           Abort PVT_read_key subroutine
                    error_pvt = .true.
                    return
                end if
            end do pvt_loop

        !   Pressure or temperature points test
            if (i <= 30) then

        !       Successful
                call LOG_in ( &
                    unit_log, '    Complete pvt header read' )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Incomplete pvt header read' )

        !       Abort PVT_read_key subroutine
                error_pvt = .true.
                return
            end if

        !   Pvt memory allocation
            call PVT_alloc ( &
                unit_log, npt, ntt, pvt, error_pvt )

        !   Abort PVT_read_key subroutine test
            if (error_pvt) return

        !   Jump back one line
            backspace (unit = unit_pvt, iostat = iost)

        !   First pressure read
            read ( unit = unit_pvt, fmt = '(a2000)', iostat = iost ) tab_header(1)

        !   First pressure test
            if (iost == 0) then

        !       First presure
                call PVT_line ( &
                    .false., 1, tab_header(1), k, iost, pvt%pr(1) )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pvt table cannot be read' )

        !       Abort PVT_read_key subroutine
                error_pvt = .true.
                return
            end if

        !   Second pressure read
            read ( unit = unit_pvt, fmt = '(a2000)', iostat = iost ) tab_header(2)

        !   Second pressure
            if (iost == 0) then

        !       Second pressure 
                call PVT_line &
                    ( .false., 1, tab_header, k, iost, pvt%pr(2) )
            else

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pvt table cannot be read' )

        !       Abort PVT_read_key subroutine
                error_pvt = .true.
                return
            end if

        !   Jump back two lines
            backspace (unit = unit_pvt, iostat = iost)
            backspace (unit = unit_pvt, iostat = iost)

        !   Pressure or temperature order
            if (pvt%pr(1) == pvt%pr(2)) then

        !       Loop for pressure order
                loop_1: do i = 1, npt
                    do j = 1, ntt

        !               Raw data read
                        read ( unit = unit_pvt, fmt = '(a2000)', iostat = iost ) tab_header(1)

        !               Raw data read test
                        if (iost == 0) then

        !                   Pvt table point array
                            call PVT_line ( &
                                .false., 18, tab_header(1), k, iost, aux )

        !                   Pvt table point array test
                            if (iost == 0) then

        !                       Data transfer
                                pvt%pr(i) = aux(1)
                                pvt%tp(j) = aux(2)
                                pvt%gas(i,j)%den = aux(3)
                                pvt%liq(i,j)%den = aux(4)
                                pvt%gas(i,j)%stn = aux(9)
                                pvt%gas(i,j)%vis = aux(10)
                                pvt%liq(i,j)%vis = aux(11)
                                pvt%gas(i,j)%scp = aux(12)
                                pvt%liq(i,j)%scp = aux(13)
                                pvt%gas(i,j)%enh = aux(14)
                                pvt%liq(i,j)%enh = aux(15)
                                pvt%gas(i,j)%epc = aux(7)
                                pvt%gas(i,j)%con = aux(16)
                                pvt%liq(i,j)%epc = aux(8)
                                pvt%liq(i,j)%con = aux(17)
                                pvt%liq(i,j)%stn = aux(18)
                            else

        !                       Unsuccessful
                                error_pvt = .true.
                                exit loop_1
                            end if
                        else

        !                   Unsuccessful
                            error_pvt = .true.
                            exit loop_1
                        end if
                    end do
                end do loop_1
            else

        !       Loop for temperature
                loop_2: do j = 1, ntt
                    do i = 1, npt

        !               Raw data read
                        read ( unit = unit_pvt, fmt = '(a2000)', iostat = iost ) tab_header(1)

        !               Raw data read test
                        if (iost == 0) then

        !                   Pvt table point array
                            call PVT_line ( &
                                .false., 18, tab_header(1), k, iost, aux )

        !                   Pvt table point array test
                            if (iost == 0) then
                                pvt%pr(i) = aux(1)
                                pvt%tp(j) = aux(2)
                                pvt%gas(i,j)%den = aux(3)
                                pvt%liq(i,j)%den = aux(4)
                                pvt%gas(i,j)%stn = aux(9)
                                pvt%gas(i,j)%vis = aux(10)
                                pvt%liq(i,j)%vis = aux(11)
                                pvt%gas(i,j)%scp = aux(12)
                                pvt%liq(i,j)%scp = aux(13)
                                pvt%gas(i,j)%enh = aux(14)
                                pvt%liq(i,j)%enh = aux(15)
                                pvt%gas(i,j)%epc = aux(7)
                                pvt%gas(i,j)%con = aux(16)
                                pvt%liq(i,j)%epc = aux(8)
                                pvt%liq(i,j)%con = aux(17)
                                pvt%liq(i,j)%stn = aux(18)
                            else

        !                       Unsuccessful
                                error_pvt = .true.
                                exit loop_2
                            end if
                        else

        !                   Unsuccessful
                            error_pvt = .true.
                            exit loop_2
                        end if
                    end do
                end do loop_2
            end if

        !   pvt table message
            if (error_pvt) then

        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Pvt table cannot be read' )
            else

        !       Successful
                call LOG_in ( &
                    unit_log, '    Pvt table read' )

        !       Temperature array from [C] to [K]
                pvt%tp = pvt%tp + 273.15_rp
            end if

        !	Internal fuctions or subroutines
            contains

        ! ***********************************************************************************

            subroutine			PVT_line    (counter, ni, line, no, iost, aux)
            ! *******************************************************************************
            !                   * *  TULSA UNIVERSITY FLUID FLOW PROJECTS  * *
            ! **  SUBROUTINE:   PVT_line
            ! **  VERSION:		2.0 Fortran 2003
            ! **  DESCRIPTION:	Reads OLGA tab key files sentence.
            ! *******************************************************************************
            !   Input variables
                logical,intent(in)          ::  counter     !   Counter flag            [-]
                integer(ip),intent(in)      ::	ni          !	Maximum points          [-]
                character(2000),intent(in)  ::  line(2)     !   Lines to be read        [-]
            !   Output variables
                integer(ip),intent(out)     ::	no          !	Data points counter     [-]
                integer(ip),intent(out)     ::	iost        !   File I/O status         [-]
                real(rp),intent(out)        ::  aux(ni)
            !	Internal variables
                integer(ip)		            ::	i, j, k     !	Counters                [-]
                character(2000)             ::  line_read   !   Lines to be read        [-]   
                character(2000)             ::  line_aux    !   Lines to be read        [-]
            ! *******************************************************************************

            !   Second line test
                if (scan(trim(line(1)), ')') > 0) then

            !       Second line not present
                    line_read = line(1)
                else

            !       Second line present
                    line_aux = line(1)
                    k = scan(trim(line_aux), '\')
                    write (line_aux, fmt = *, iostat = iost) line_aux(1:k-1)
                    line_read = trim(line_aux) // trim(line(2))
                end if

            !   Scaning boundaries
                i = scan(trim(line_read), '(')
                j = scan(trim(line_read), ')')

            !   Data write
                write ( line_read, fmt = *, iostat = iost ) line_read(i+1:j-1)

            !   Array read
                read ( line_read, fmt = *, iostat = iost ) (aux(i), i = 1, ni)

            !   Data points counter
                if (counter) then

            !       Vector search
                    do i = 1, ni - 1

                        if (aux(i) == zero .and. aux(i+1) == zero) then

            !               Vector points
                            no = i - 1
                            exit
                        end if
                    end do        

            !       Error detection
                    if (no > 0) iost = 0
                end if

            ! *******************************************************************************
            end subroutine

    ! ***************************************************************************************
    !
   
    
    ! ***************************************************************************************
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    ! Define THM_read to read THM file into THM array
        subroutine	    THM_read ( &
            unit_log, unit_thm, xsf, xwm, sf, tsf, &
            thm, error_thm )


!   Called subroutines
use Numerics,only           :   Iblnr_1d
use Numerics,only           :   Search_1d

!   Parameters
integer(ip),parameter       ::	nm = 100        !	Maximun reading points  [-]

!	Input variables
integer(ip),intent(in)      ::  unit_log        !   Log file unit           [-]
integer(ip),intent(in)	    ::	unit_thm        !   THM file unit           [-]
integer(ip),intent(in)      ::	xsf             !   Solid fraction model    [-]
integer(ip),intent(in)      ::	xwm             !   Oil wax viscosity mult. [-]
real(rp),intent(in)	        ::	sf(100)         !   Solid fraction table    [-]
real(rp),intent(in)	        ::	tsf(100)        !   Solid fraction tmp table[C]

!   Input / Output variables
logical,intent(inout)       ::	error_thm       !   THM file error status   [-]
type(thm_type),intent(out)  ::  thm             !   THM data-type
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

!	Internal variables
logical                     ::  jump            !   Jump condition          [-]
logical                     ::  vis_corr        !   Oil-wax viscosity exits [-]
character(2000)             ::  thm_header      !   THM file header         [-]
character(30)               ::  real_read       !   Lines to be read        [-]
integer(ip)                 ::  alloc_st(2)     !   Allocation status       [-]
integer(ip)		            ::	i, j, k         !	Counters                [-]
integer(ip)                 ::	iost            !   File I/O status         [-]
integer(ip)                 ::  npt             !   Pressure array length   [-]
integer(ip)                 ::  ntt             !   Temperature array length[-]
integer(ip)                 ::  nwf             !   Wax sf table points     [-]
integer(ip)                 ::  nwt             !   Wax componets points    [-]
real(rp)                    ::  wf              !   Wax solid fr.           [mol/mol]
real(rp)                    ::  vis(3)          !   Oil-wax vis multipliers [-]
real(rp),allocatable        ::  wfr(:,:,:)      !   Wax componets fraction  [mol_c/mol]
real(rp),allocatable        ::  cfr(:,:)        !   Wax concentration frac  [mol/mol]
type(wxc_type)              ::  wxc(nm)         !   Wax components data-type
                !   nme                 Names                   [-]
                !   cfr                 Concentration fr.       [mol_c/mol]
                !   den                 Density                 [kg_c/m^3_c]
                !   enh                 Melting heat            [J_c/kg_c]
                !   mol                 Molecular weigth        [kg_c/kmol_c]

! ***********************************************************************************

!   Local error counter
k = 0

!   Thm file error initial value
error_thm = .false.

!   Viscosity correction initial values
vis_corr = .false.

!   Scans thm file
thm_loop: do i = 1, 30

!       Header read
read ( unit = unit_thm, fmt = '(a2000)', iostat = iost ) thm_header

!       Header message
if (iost /= 0) then

!           Unsuccessful
call LOG_in ( &
unit_log, '    Header cannot be read' )

!           Exit thm_loop
error_thm = .true.
exit thm_loop
end if

!       Number of wax components section
if (index(thm_header, 'Number of Wax') == 2) then

!           Number of wax components
read ( unit = unit_thm, fmt = *, iostat = iost ) nwt

!           Wax components message
if (iost == 0) then

!               Successful 
call LOG_in ( &
    unit_log, '    Number of wax components read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Number of wax components cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Wax components names section
if (index(thm_header, 'Names of Wax') == 2) then

!           Wax components names array
read ( unit = unit_thm, fmt = *, iostat = iost ) &
(wxc(j)%nme, j = 1, nwt)

!           Wax components names array message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Wax components names array read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Wax components names array cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Molecular weights of wax components section
if (index(thm_header, 'Molecular') == 2) then

!           Wax components molecular weights array
read ( unit = unit_thm, fmt = *, iostat = iost ) &
(wxc(j)%mol, j = 1, nwt)

!           Wax components molecular weights array message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Wax components molecular weights array read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Wax components molecular weights array cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Liquid densities of wax components section
if (index(thm_header, 'Liquid') == 2) then

!           Wax components density array
read ( unit = unit_thm, fmt = *, iostat = iost ) &
(wxc(j)%den, j = 1, nwt)

!           Wax components density array message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Wax components density array read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Wax components density array cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Heat of melting of wax components section
if (index(thm_header, 'Heat') == 2) then

!           Wax components melting heat array
read (unit = unit_thm, fmt = *, iostat = iost) &
(wxc(j)%enh, j = 1, nwt)

!           Wax components melting heat array message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Wax components melting heat array read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Wax components melting heat array cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Concentration of Wax Components in feed section
if (index(thm_header, 'Concentration') == 2) then

!           Wax components concentration array
read ( unit = unit_thm, fmt = *, iostat = iost ) &
(wxc(j)%cfr, j = 1, nwt)

!           Wax components concentration array message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Wax components concentration array read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Wax components concentration array cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Oil-wax viscosity multipliers section
if ((index(thm_header, 'OIL_WAX') == 1 .or. &
index(thm_header, 'OIL_WAX') == 2) .and. &
xwm == 1 ) then

!           Oil-wax viscosity multipliers present
vis_corr = .true.

!           D factor search
j = index(thm_header, 'D=')
write ( real_read, fmt = *, iostat = iost ) thm_header(j+2:j+8)
read ( real_read, fmt = '(f6.4)', iostat = iost ) vis(1)
if (iost /= 0)  vis_corr = .false.

!           E factor search
j = index(thm_header, 'E=')
write ( real_read, fmt = *, iostat = iost ) thm_header(j+2:j+8)
read ( real_read, fmt = '(f6.4)', iostat = iost ) vis(2)
if (iost /= 0)  vis_corr = .false.

!           F factor search
j = index(thm_header, 'F=')
write ( real_read, fmt = *, iostat = iost ) thm_header(j+2:j+8)
read ( real_read, fmt = '(f6.4)', iostat = iost ) vis(3)
if (iost /= 0)  vis_corr = .false.

!           Oil-wax viscosity multipliers message
if (vis_corr) then

!               Successful
call LOG_in ( &
    unit_log, '    Oil-wax viscosity multipliers read' )
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Oil-wax viscosity multipliers cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Number pressure and temperature points section
if (index(thm_header, 'Number of P') == 2) then

!           Pressure and temperature arrays
read ( unit = unit_thm, fmt = *, iostat = iost ) npt, ntt

!           Number of pressure and temperature points message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Number of pressure and temperature points read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Number of pressure and temperature points cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if

!           Thm memory allocation
call THM_alloc &
( unit_log, npt, ntt, vis, vis_corr, thm, error_thm )

!           Exit thm_loop test
if (error_thm) exit thm_loop

!           Local memory allocation
allocate (wfr(npt,ntt,nwt), stat = alloc_st(1))
allocate (cfr(npt,ntt), stat = alloc_st(2))

!           Local memory allocation message
if (alloc_st(1) == 0 .and. &
alloc_st(2) == 0) then

!               Successful 
call LOG_in ( &
    unit_log, '    Local memory allocated' )
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Local memory cannot be allocated' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Pressure points section
if (index(thm_header, 'P Points') == 2) then

!           Pressure array
read ( unit = unit_thm, fmt = *, iostat = iost ) &
(thm%pr(j), j = 1, npt)

!           Pressure array message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Pressure array read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Pressure array cannot be read' )

!               Exit thm_loop
error_thm = .true.
exit thm_loop
end if
end if

!       Cloud Point Temperatures section
if (index(thm_header, 'Cloud') == 2) then

!           Cloud point temperature array
read (unit = unit_thm, fmt = *, iostat = iost) &
    (thm%tpc(j), j = 1, npt)

!           Cloud point temperature array message
if (iost == 0) then

!               Successful
call LOG_in ( &
    unit_log, '    Cloud point temperature array read' )
k = k + 1
else

!               Unsuccessful
call LOG_in ( &
    unit_log, '    Cloud point temperature array cannot be read' )

!               Exit thm_loop
error_thm = .true.
end if
exit thm_loop
end if
end do thm_loop

!   Abort THM_read_file subroutine test
if (k < 9 .or. i > 30 .or. error_thm) return

!   Format selection
read ( unit = unit_thm, fmt = '(a2000)', iostat = iost ) thm_header
if (index(thm_header, 'Pressure Point') == 2) then

!       Three lines
jump = .false.

!       Jump back one line
backspace (unit = unit_thm, iostat = iost)
else

!       Four lines
jump = .true.
end if

!   Local error counter
k = 0

!   Pressure loop
press_loop: do i = 1, npt

!       Header jump
if (jump) then

read ( unit = unit_thm, fmt = '(3/)', iostat = iost )
else

read ( unit = unit_thm, fmt = '(2/)', iostat = iost )
end if

!       Temperature loop
temp_loop: do j = 1, ntt

!           Wax properties table 
read ( unit = unit_thm, fmt = *, iostat = iost ) &
thm%tp(j), (wfr(i,j,k), k = 1, nwt), thm%wax(i,j)%den, &
cfr(i,j), thm%wax(i,j)%mwl, thm%wax(i,j)%mww, &
thm%wax(i,j)%enh, thm%wax(i,j)%scp, cfr(i,j)

!           Wax properties table test
if (iost /= 0) then

error_thm = .true.
exit press_loop
end if
end do temp_loop
end do press_loop

!   Wax properties table message
if (error_thm) then

!       Unsuccessful
call LOG_in ( &
unit_log, '    Wax properties table cannot be read' )
return
else

!       Successful
call LOG_in ( &
unit_log,  '    Wax properties table read' )
end if

!   Matrix solid fraction
select case (xsf)
!		-----------------------------------------------------------------------------
case (0)    !   File

!           Feet concentration
forall (i = 1:npt, j = 1:ntt) cfr(i,j) = sum(wfr(i,j,:))

!           Temperature loop
forall (j = 1:ntt)

!               Wax solid fraction
thm%wax(:,j)%wfr = cfr(:,ntt) - cfr(:,j)

! pulled from the DSC curve for the oil.. fitted with a polynomial

!thm%wax(:,j)%wfr = (1.0*(10.0**-6.0)*(temp**-3.0))-(8.0*(10.0**-5.0)*(temp**2.0))-(0.0002*temp)+0.0837

!               Wax concentration fraction
thm%wax(:,j)%cfr = cfr(:,j) - cfr(:,1)

!thm%wax(:,j)%cfr = -(1.0*(10.0**-6.0)*(temp**-3.0))+(8.0*(10.0**-5.0)*(temp**2.0))+(0.0002*temp)-0.0007

end forall

!           Temperature loop
do j = 2, ntt - 1

!               Inner wax solid fraction d.w.r. tp
thm%wax(:,j)%dwf = (thm%wax(:,j+1)%wfr - thm%wax(:,j-1)%wfr) / &
    (thm%tp(j+1) - thm%tp(j-1)) 
end do

!           Upstream wax solid fraction d.w.r. tp
thm%wax(:,1)%dwf = (thm%wax(:,2)%wfr - thm%wax(:,1)%wfr) / &
(thm%tp(2) - thm%tp(1)) 

!           Downstream wax solid fraction d.w.r. tp
thm%wax(:,ntt)%dwf = (thm%wax(:,ntt)%wfr - thm%wax(:,ntt-1)%wfr) / &
(thm%tp(ntt) - thm%tp(ntt-1))

!           Matrix solid fraction from file message
call LOG_in ( &
unit_log, '    Solid fraction calculated from file' )
!		-----------------------------------------------------------------------------
case (1)    !   User molar base table

!           Table number of points
nwf = thm_sf_tb ( sf, tsf )

!           Pressure loop
do i = 1, npt

!               Temperature
do j = 1, ntt

    if (thm%tp(j) < thm%tpc(i)) then

!                       Searching the tsf vector
        k = Search_1d ( tsf, thm%tp(j), nwf )

!                       Solid fraction (Tp < WAT)
        call Iblnr_1d ( tsf, k, sf, thm%tp(j), wf )
    else

!                       Solid fraction (Tp > WAT)
        wf = zero
    end if

!                   Wax solid fraction
    thm%wax(i,j)%wfr = wf

!                   Wax concentration fraction
    thm%wax(i,j)%cfr = thm%wax(i,1)%wfr - thm%wax(i,j)%wfr
end do
end do

!           Temperature loop
do j = 2, ntt-1

!               Inner wax solid fraction d.w.r. tp
thm%wax(:,j)%dwf = (thm%wax(:,j+1)%wfr - thm%wax(:,j-1)%wfr) / &
    (thm%tp(j+1) - thm%tp(j-1)) 
end do

!           Upstream wax solid fraction d.w.r. tp
thm%wax(:,1)%dwf = (thm%wax(:,2)%wfr - thm%wax(:,1)%wfr) / &
(thm%tp(2) - thm%tp(1)) 

!           Downstream wax solid fraction d.w.r. tp
thm%wax(:,ntt)%dwf = (thm%wax(:,ntt)%wfr - thm%wax(:,ntt-1)%wfr) / &
(thm%tp(ntt) - thm%tp(ntt-1))

!           Matrix solid fraction from file message
call LOG_in ( &
unit_log, '    Solid fraction calculated from user molar table data' )
!		-----------------------------------------------------------------------------
case (2)    !   User mass base table

!           Table number of points
nwf = thm_sf_tb ( sf, tsf )

!           Pressure loop
do i = 1, npt

!               Temperature
do j = 1, ntt

    if (thm%tp(j) < thm%tpc(i)) then

!                       Searching the tsf vector
        k = Search_1d ( tsf, thm%tp(j), nwf )

!                       Solid fraction (Tp < WAT)
        call Iblnr_1d ( tsf, k, sf, thm%tp(j), wf )
    else

!                       Solid fraction (Tp > WAT)
        wf = zero
    end if

!                   Wax solid fraction
    thm%wax(i,j)%wfr = wf * thm%wax(i,ntt)%mww / thm%wax(i,j)%mww

!                   Wax concentration fraction
    thm%wax(i,j)%cfr = thm%wax(i,1)%wfr - thm%wax(i,j)%wfr
end do
end do

!           Temperature loop
do j = 2, ntt - 1

!               Inner wax solid fraction d.w.r. tp
thm%wax(:,j)%dwf = (thm%wax(:,j+1)%wfr - thm%wax(:,j-1)%wfr) / &
    (thm%tp(j+1) - thm%tp(j-1)) 
end do

!           Upstream wax solid fraction d.w.r. tp
thm%wax(:,1)%dwf = (thm%wax(:,2)%wfr - thm%wax(:,1)%wfr) / &
(thm%tp(2) - thm%tp(1)) 

!           Downstream wax solid fraction d.w.r. tp
thm%wax(:,ntt)%dwf = (thm%wax(:,ntt)%wfr - thm%wax(:,ntt-1)%wfr) / &
(thm%tp(ntt) - thm%tp(ntt-1))

!           Matrix solid fraction from file message
call LOG_in ( &
unit_log, '    Solid fraction calculated from user mass table data' )
!		-----------------------------------------------------------------------------
end select

!   Temperature arrays from [C] to [K]
thm%tpc = thm%tpc + 273.15_rp
thm%tp = thm%tp + 273.15_rp

!   Local memory deallocation
deallocate (wfr, stat = alloc_st(1))
deallocate (cfr, stat = alloc_st(2))

!   Local memory deallocation message
if (alloc_st(1) == 0 .and. &
alloc_st(2) == 0) then

!       Successful
call LOG_in ( &
unit_log, '    Local memory deallocated' )
else

!       Unsuccessful
call LOG_in ( &
unit_log, '    Local memory cannot be deallocated' )
end if

    contains 


        !	Internal fuctions or subroutines
           
        ! ***********************************************************************************
    
    pure function       thm_sf_tb(sf, tsf)
    ! *******************************************************************************
    !                   * *  TULSA UNIVERSITY PARAFFIN DEPOSITION PROJECTS * *
    ! **  FUNCTION:     thm_sf_poly
    ! **  VERSION:		2.0 Fortran 2003
    ! **  DESCRIPTION:	Calculates the number of point in the solubility table.
    ! *******************************************************************************
    ! !   Input variable
         real(rp),intent(in)	        ::	sf(100)     !   Solid fraction table    [-]
         real(rp),intent(in)	        ::	tsf(100)    !   Solid fraction tmp table[C]
    !   Output variables
        integer(ip)                          ::	thm_sf_tb   !	Wax sf table points     [-]
    ! *******************************************************************************

    !   Search loop
        do thm_sf_tb = 1, 100

            if (sf(thm_sf_tb) == zero .and. tsf(thm_sf_tb) == zero) exit
        end do

    !   Number of point for interpolation
        thm_sf_tb = thm_sf_tb - 1

    ! *******************************************************************************
    end function

end subroutine 
 !Define THM_alloc to allocate space for thm array
        subroutine              THM_alloc ( &
                                    unit_log, npt, ntt, vis, vis_corr, &
                                    thm, error_thm )

    
        !	Input variables
            logical,intent(in)          ::  vis_corr        !   Oil-wax viscosity exits [-]
            integer(ip),intent(in)      ::  unit_log        !   File unit               [-]
            integer(ip),intent(in)      ::  npt             !   Pressure array length   [-]
            integer(ip),intent(in)      ::  ntt             !   Temperature array length[-]
            real(rp),intent(in)         ::  vis(3)          !   Oil-wax vis multipliers [-]
    
        !   Output variables
            logical,intent(out)         ::  error_thm       !   Allocation status       [-]
            type(thm_type),intent(out)  ::  thm             !   THM data-type
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
    
        !	Internal variables
            integer(ip)                 ::  alloc_st(4)     !   Allocation status       [-]
    
        ! ***********************************************************************************
    
        !   THM pressure and temperature dimensions
            thm%npt = npt
            thm%ntt = ntt
    
        !   Oil-wax viscosity multipliers
            if (vis_corr) then
    
                thm%vis = vis
            else
    
                thm%vis = zero
            end if
    
        !   Thm memory allocation
            allocate (thm%pr(npt), stat = alloc_st(1))
            allocate (thm%tp(ntt), stat = alloc_st(2))
            allocate (thm%tpc(npt), stat = alloc_st(3))
            allocate (thm%wax(npt,ntt), stat = alloc_st(4))
    
        !   Thm memory allocation message
            if (alloc_st(1) == 0 .and. &
                alloc_st(2) == 0 .and. &
                alloc_st(3) == 0 .and. &
                alloc_st(4) == 0) then
    
        !       Successful 
                call LOG_in ( &
                    unit_log, '    THM memory allocated' )
                error_thm = .false.
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    THM memory cannot be allocated' )
                error_thm = .true.
            end if
    
        ! ***********************************************************************************
        end subroutine
    
        

        
        ! Define THM_read_file to reac THM file into THM array
    
        subroutine	THM_read_file ( &
                        unit_log, xsf, xwm, sf, tsf, file_thm, &
                        thm, error_thm )

    
        !	Input variables
            integer(ip),intent(in)      ::	unit_log        !   Log file unit           [-]
            integer(ip),intent(in)      ::	xsf             !   Solid fraction model    [-]
            integer(ip),intent(in)      ::	xwm             !   Oil wax viscosity mult. [-]
            real(rp),intent(in)	        ::	sf(100)         !   Solid fraction table    [-]
            real(rp),intent(in)	        ::	tsf(100)        !   Solid fraction tmp table[C]
            character(2000),intent(in)  ::  file_thm        !   THM file name           [-]
    
        !	Output variables
            logical,intent(out)         ::	error_thm       !   THM file error status   [-]
            type(thm_type),intent(out)  ::  thm             !   THM data-type
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
    
        !	Internal variables
            logical				        ::	thm_exists      !   THM file exists status  [-]
            integer(ip)                 ::	iost            !   File I/O status         [-]
            integer                     ::  unit_thm        !   File unit               [-]
    
        ! ***********************************************************************************
    
        !   Thm section message
            call LOG_in ( unit_log, '' )
            call LOG_in ( &
                unit_log, 'THM section opened' )
    
        !   Thm file inquire
            inquire (file = file_thm, exist = thm_exists)
    
        !   Thm file inquire message
            if (thm_exists) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Thm file found: ' // trim(file_thm) )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Thm file not found: ' // trim(file_thm) )
                error_thm = .true.
                return
            end if
    
        !   Thm file opening
            open (newunit = unit_thm, file = file_thm, status = 'old', iostat = iost)
    
        !   Thm file opening message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Thm file opened' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Thm file cannot be opened' )
                error_thm = .true.
                return
            end if
    
        !   Thm file read
            call THM_read ( &
                unit_log, unit_thm, xsf, xwm, sf, tsf, &
                thm, error_thm )
    
        !   Thm file closing
            close (unit = unit_thm, iostat = iost)
    
        !   Thm file closing message
            if (iost == 0) then
    
        !       Successful
                call LOG_in ( &
                    unit_log, '    Thm file closed' )
            else
    
        !       Unsuccessful
                call LOG_in ( &
                    unit_log, '    Thm file cannot be closed' )
                error_thm = .true.
            end if
    
        ! ***********************************************************************************
        end subroutine
    

  subroutine fable_output(dt,error_int,unit_log,fable_out,pipe)
        !	Input variables
       real(rp),intent(in)         ::   dt              !   Time segments
       logical,intent(in)          ::   error_int        !   Pipe integration error  [-]
       integer(ip),intent(in)      ::	unit_log        !   Log file unit           [-]
       character(*),intent(in)  ::  fable_out        !   Output file name        [-]
       type(pp_type),allocatable, &
           intent(inout)           ::  pipe(:,:)       !   Pipe data-type
                                   !   sec                 Pipe section data-type
                                   !   sec%lp              Pipeline length         [m]
                                   !   sec%di              Inner diameter          [m]
                                   !   sec%dx              Section length          [m]
                                   !   sec%fw              Wax weigth fraction     [-]
                                   !   sec%pr              Pressure                [Pa]
                                   !   sec%th              Wax layer thickness     [m]
                                   !   sec%ti              Inner diameter temp     [K]
                                   !   sec%tp              Bulk temperature        [K]
                                   !   gas                 Gas properties data-type
                                   !	gas%den             Density                 [kg/m^3]
                                   !	gas%stn             Mass fraction           [-]
                                   !	gas%vel             Superficial velocity    [m/s]
                                   !	gas%vis             Viscosity               [Pa*s]
                                   !   gas%enh             Enthalpy                [J/kg]
                                   !   gas%scp             Specific heat           [J/(kg*K)]
                                   !   gas%con             Conductivity            [W/(m*K)]
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
                                   !   film%gas%h          Gas holdup              [-]
                                   !   film%gas%v          Gas velocity            [m/s]
                                   !   film%gas%f%i        Gas int friction        [-]
                                   !   film%gas%f%w        Gas wal friction        [-]
                                   !   film%gas%s%i        Gas int perimeter       [-]
                                   !   film%gas%s%w        Gas wal perimeter       [-]
                                   !   film%gas%t%i        Gas int stress          [Pa]
                                   !   film%gas%t%w        Gas wal stress          [Pa]
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
       integer                     ::  fable_unit      !   File unit               [-]
       integer(ip)                 ::	i, j, k         !   Counters                [-]
       integer(ip)                 ::	iost            !   File I/O status         [-]
       integer(ip)                 ::  np              !   Number of pipes         [-]
       integer(ip)                 ::  nt              !   Number of time steps    [-]
       integer(ip)                 ::  ns(size(pipe,1))!   Sections per pipe       [-]
       
       
       open (newunit = fable_unit, file = fable_out, status = 'unknown', iostat = iost)

       !   Output file opening message
       if (iost == 0) then

           !       Successful
                   call LOG_in ( &
                       unit_log, '    Output file opened: ' // trim(fable_out) )
                   write ( unit = fable_unit, fmt = '("#Wax_Model v1.0")', iostat = iost )
                   write ( unit = fable_unit, fmt = '("#Daraboina Research Group")', iostat = iost )

       else
       
           !       Unsuccessful
                   call LOG_in ( &
                       unit_log, '    Output file cannot be opened' )
                   return
       end if
       
       
       !   Pipe integration test
       if (.not. error_int) then

           !       Number of pipes
                   np = size(pipe, dim=1)
           !       Number of time steps
        !            if (xhm(1) > -1) then
       
        !    !           Pseudo steady state
                    nt = size(pipe, dim=2) - 1
        !            else
       
        !    !           Steady state
        !                nt = 0
        !            end if
       
           !       Number of sections per pipe
                   do j = 1, np
       
                       ns(j) = size(pipe(j,0)%sec) - 2
                   end do

           !       Table name
                   write ( unit = fable_unit, fmt = '("table+ output_data")', iostat = iost )

           !       Table parameters type
                   write ( unit = fable_unit, fmt = 50, iostat = iost )

           !       Table parameters header
                   write ( unit = fable_unit, fmt = 60, iostat = iost )

           !       Loop to write the values for each column 
                   time_loop: do i = 0, nt

                       
                       pipe_loop: do j = 1, np
          
                       !       First section line
                           if (j==1) then 
                               write ( unit = fable_unit, fmt = 70, iostat = iost ) &
                                   real(i, rp) * dt,j,0,&
                                   pipe(j,i)%sec(0)%lp,pipe(j,i)%sec(0)%dx,&
                                   pipe(j,i)%sec(0)%di,pipe(j,i)%sec(0)%pr,&
                                   pipe(j,i)%sec(0)%tp,pipe(j,i)%sec(0)%ti,&
                                   pipe(j,i)%sec(0)%th,&
                                   0.0,0.0,&
                                   0.0,0.0,&
                                   0.0,0.0,&
                                   0.0,0.0,&
                                   0.0,0.0,&
                                   0.0,0.0,&
                                   0.0

                           end if 
                           section_loop: do k = 1, ns(j)
                       !   Middle sections

                           !       At time = 0 there is no hydrates 

                        !    if (i==0) then 
                               
                        !       ! pipe(j,i)%sec(k)%mfs = 0.0
                        !       ! pipe(j,i)%sec(k)%mfg = 0.0
                        !        !pipe(j,i)%sec(k)%hfs = 0.0
                        !        !pipe(j,i)%sec(k)%u_r = 1.0
                        !        !pipe(j,i)%sec(k)%mhyd = 0.0
                        !        !pipe(j,i)%sec(k)%mhyd_flux = 0.0
                        !    end if 

                           
                            write ( unit = fable_unit, fmt = 70, iostat = iost ) &
                            real(i, rp) * dt,j,k,pipe(j,i)%sec(k)%lp,pipe(j,i)%sec(k)%dx,&
                            pipe(j,i)%sec(k)%di,pipe(j,i)%sec(k)%pr,&
                            pipe(j,i)%sec(k)%tp,pipe(j,i)%sec(k)%ti,&
                            pipe(j,i)%sec(k)%th,pipe(j,i)%sec(k)%fw,&
                            pipe(j,i)%prg(k)%ace,pipe(j,i)%prg(k)%fri,&
                            pipe(j,i)%prg(k)%gra,pipe(j,i)%prg(k)%tot,&
                            pipe(j,i)%tpg(k)%dtr,pipe(j,i)%tpg(k)%dtx,&
                            pipe(j,i)%liq(k)%den,pipe(j,i)%liq(k)%con,&
                            pipe(j,i)%liq(k)%enh,pipe(j,i)%liq(k)%scp,&
                            pipe(j,i)%liq(k)%stn,pipe(j,i)%liq(k)%vel
                            !pipe(j,i)%liq(k)%vis

                           

                           end do section_loop

                       !   Last section of each pipe 
                           
                           

                           write ( unit = fable_unit, fmt = 70, iostat = iost ) &
                            real(i, rp) * dt,j, k,& 
                            pipe(j,i)%sec(k)%lp,pipe(j,i)%sec(k)%dx,&
                            pipe(j,i)%sec(k-1)%di,pipe(j,i)%sec(k)%pr,&
                            pipe(j,i)%sec(k)%tp,pipe(j,i)%sec(k-1)%ti,&
                            pipe(j,i)%sec(k-1)%th,pipe(j,i)%sec(k-1)%fw,&
                            pipe(j,i)%prg(k-1)%ace,pipe(j,i)%prg(k-1)%fri,&
                            pipe(j,i)%prg(k-1)%gra,pipe(j,i)%prg(k-1)%tot,&
                            pipe(j,i)%tpg(k-1)%dtr,pipe(j,i)%tpg(k-1)%dtx,&
                            pipe(j,i)%liq(k-1)%den,pipe(j,i)%liq(k-1)%con,&
                            pipe(j,i)%liq(k-1)%enh,pipe(j,i)%liq(k-1)%scp,&
                            pipe(j,i)%liq(k-1)%stn,pipe(j,i)%liq(k-1)%vel
                            !pipe(j,i)%liq(k-1)%vis
                       end do pipe_loop
                       
                   end do time_loop
       else

       
           !       Message
               write ( unit = fable_unit, &
               fmt = '("#Unsuccessful simulation")' )
       end if 


50           format ('float',2(',','integer'), 20(',', 'float'))
60           format ('"time [s]"',',','"pipe [-]"', ',', '"section [-]"',',', '"length [m]"',&
                    ',', '"section length [m]"',',',  '"diameter [m]"',&
                    ',', '"pressure [pa]"',',','"bulk temperature [K]"',',', '"inner temperature [K]"', &
                    ',','"thickness [m]"',',','"wax fraction [-]"',&
                    ',', '"ace P [Pa/m]"',',', '"fri P [Pa/m]"',',', '"gra P [Pa/m]"',&
                    ',', '"tot P [Pa/m]"',',', '"radial T [K]"',',', '"axial T [K]"',&
                    ',', '"liq den [kg/m3]"',',', '"liq con [W/m-K]"',',', '"liq enth [J/kg]"',&
                    ',', '"liq scp [J/kg-K]"',',', '"liq s.ten [N/m]"',',', '"liq velocity [m/s]"')
                    !',', '"liquid vis [Pa-s]"')
70           format (es11.4,2(',',i3), 20(',', es11.4)) 
   end subroutine     ! ***************************************************************************************
    end module
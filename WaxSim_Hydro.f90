module  Model_single

    
    !   Used modules
        use                 ::  Constants
    
    !   All variables must be declared
        implicit none
    
    !   Private subroutines and variables
        private
    
    !   Module public subroutines
        public              ::  PG_single
    
    !   Module subroutines
        contains
    
    ! ***************************************************************************************
      
    !   Define PG_single to calculate the pressure gradient and shear stresses for single phase flow
        subroutine         PG_single ( &
                                    psp, fluid, p, &
                                    prg, &
                                    mix, mix_n)
  
        !   Called subroutines
            use Closures,only           :   Friction
    
        !   Input variables
           
            
            real(rp),intent(in)         ::  p               !   Locl pressure           [Pa]
            type(f1_type),intent(in)    ::  fluid           !   Fluid data-type
                                        !	fluid%den           Density                 [kg/m^3]
                                        !	fluid%stn           Surface tension         [N/m]
                                        !	fluid%vel           Superficial velocity    [m/s]
                                        !	fluid%vis           Viscosity               [Pa*s]
            type(psp1_type),intent(in)  ::  psp             !   Pipe section properties data-type
                                        !   psp%ang             Angle                   [rad]
                                        !   psp%dia             Inner diameter          [m]
                                        !   psp%rou             Relative roughness      [-]
    
        !	Output variables
            type(prg_type),intent(out)  ::  prg             !   Pressure gradient data-type
                                        !   prg%fpc             Flow pattern code       [-]
                                        !   prg%hlg             Gas-liq holdup          [-]
                                        !   prg%hll             Liq-liq holdup          [-]
                                        !   prg%fpc             Flow pattern code       [-]
                                        !   prg%ace             Acc pressure gradient   [Pa/m]
                                        !   prg%fri             Fri pressure gradient   [Pa/m]
                                        !   prg%gra             Gra pressure gradient   [Pa/m]
                                        !   prg%tot             Tot pressure gradient   [Pa/m]
            type(lay_type),intent(out)  ::  mix             !   Mix layer data-type
                                    !   mix%h               Holdup                  [-]
                                    !   mix%v               Velocity                [m/s]
                                    !   mix%f%i             Int friction            [-]
                                    !   mix%f%w             Wal friction            [-]
                                    !   mix%s%i             Int perimeter           [m]
                                    !   mix%s%w             Wal perimeter           [m]
                                    !   mix%t%i             Int stress              [Pa]
                                    !   mix%t%w             Wal stress              [Pa]
            type(layn_type),intent(out) ::  mix_n           !   Mix layer numerical data-type
                                    !   mix_n%f%er          Friction error          [-]
                                    !   mix_n%f%it          Friction iterations     [-]
                                    !   mix_n%f%nm          Friction report         [-]
                                    !   mix_n%h%er          Holdup error            [-]
                                    !   mix_n%h%it          Holdup iterations       [-]
                                    !   mix_n%h%nm          Holdup report           [-]
                                    !   mix_n%w%er          Wettability error       [-]
                                    !   mix_n%w%it          Wettability iterations  [-]
                                    !   mix_n%w%nm          Wettability report 
    
        !   Internal variables
            real(rp)                    ::  ex              !   Expansion factor        [-]
            real(rp)                    ::  re              !   Reynolds number         [-]
        ! ***********************************************************************************
    
        !   Slug region, mix layer velocity
            mix%v = fluid%vel
    
        !   Slug region, mix layer holdup
            mix%h = one
    
        !   Slug region, mix layer, interfacial perimeter
            mix%s%i = zero
    
        !   Slug region, mix layer, wall perimeter
            mix%s%w = pi * psp%dia
    
        !   Slug region, mix layer Reynolds number
        
            ! change here
            re = fluid%vel * fluid%den * psp%dia / fluid%vis

            !re = fluid%vel * 807.0 * psp%dia / 0.006

            print*, "Reynolds number", re
            print*,"liquid density", fluid%den
            print*,"liquid viscosity", fluid%vis
            print*, "pipe diameter", psp%dia
            
        !   Slug region, mix layer, interfacial friction factor
            mix%f%i = zero
    
        !   Slug region, mix layer, wall friction factor
            call Friction ( psp%rou, re, mix%f%w, mix_n%f )
            
        !   Slug region, mix layer, interfacial shear stress
            mix%t%i = zero
            
        !   Slug region, mix layer, wall shear stress
            ! change here

            !mix%t%w = p5 * mix%f%w * fluid%den * fluid%vel ** 2

            mix%t%w = p5 * mix%f%w * 870.0 * fluid%vel ** 2
            

            print*,"fluid velocity", fluid%vel

            print*,"wall shear", mix%t%w
    
        !   Pipe section, friction pressure gradient
            prg%fri =  four * mix%t%w / psp%dia
         !   prg%fri =  8.0 * fluid%vis / (3.1415 * fluid%den *((psp%dia/2)**4.0))

            !prg%fri = mix%t%w / psp%dia
    
        !   Pipe section, gravity pressure gradient
            prg%gra = fluid%den * g * sin(psp%ang)
    
        !   Pipe section, acceleration pressure gradient
            
            prg%ace = zero
         
    
        !   Pipe section, total pressure gradient
            prg%tot = prg%ace + prg%fri + prg%gra

            print*, "pressure gradient", prg%tot
    
        !   Slug region, mix layer numeric report
            mix_n = layn_type ( inf_ini, inf_type ( .false., 1, no_error ), inf_ini )
    
        ! ***********************************************************************************
        end subroutine
    
    ! ***************************************************************************************
    end module
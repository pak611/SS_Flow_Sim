module Closures

        !       used modules 
                use             :: Constants
        
        !       All variables must be declared
                implicit none

        !       Private subroutines and variables
                private

        !       Module public subroutines
                public          :: Friction

        !       Module subroutines and functions
                contains

        ! Declare Friction subroutine to calculate the friction factor
                pure subroutine         Friction ( &
                                                rou, re,  &
                                                fric, inf )


                !   Input variables
                                ! integer(ip),intent(in)      ::  xfr             !   Friction factor selector[-]
                                        !                       Churchill               0
                                        !                       Blasius                 1
                                        !                       Hall                    2
                                        !                       Colebrook-White         3
                                real(rp),intent(in)         ::  rou             !   Pipe relative roughness [-]
                                real(rp),intent(in)         ::  re              !   Reynolds number         [-]

                !   Output variables
                        real(rp),intent(out)        ::  fric            !   Wal friction factor     [-]
                        type(inf_type),intent(out)  ::  inf             !   Report data-type
                                !   inf%er              Error flag              [-]
                                !   inf%it              Number of iterations    [-]
                                !   inf%nm              Error name              [-]
                                !                       'no_error'
                                !                       'ed_requiered'
                                !                       'Colebrook_friction_error'

                !   Internal variables
                        integer(ip)                 ::  i               !   Counter                 [-]
                        real(rp)                    ::  aux(2)          !   Auxiliary variables     [-]
                        real(rp)                    ::  f               !   Colebrook friction      [-]

                ! ***********************************************************************************

                !   Report data-type initialization
                        inf = inf_type ( .false., 1, no_error )

                !   Friction factor equation selection
                        ! select case (xfr)
                !       -----------------------------------------------------------------------------
                                !  case (0)    !   Churchill

                                        !           Auxiliary variables
                                        !aux(1) = (2.457_rp * log((7._rp / re) ** 0.9_rp + 0.27_rp * rou)) ** 16
                                        !aux(2) = (37530._rp / re) ** 16

                                        !           Friction factor
                                        !fric = 2._rp * ((8._rp / re) ** 12 + &
                                        !(one / (aux(1) + aux(2))) ** 1.5_rp) ** (p25 * p33)


                 !       -----------------------------------------------------------------------------
                                !  case (0)    !   Churchill


                                                                               !           Auxiliary variables
                                        aux(1) = (2.457*log(1.0/(((7.0/ re)**0.9)+0.27*(rou/0.015))))**16.0
                                        aux(2) = (37530._rp / re) ** 16.0

                                        !           Friction factor
                                        fric = 2._rp * ((8._rp / re) ** 12 + &
                                        (one / (aux(1) + aux(2))) ** 1.5_rp) ** (p25 * p33)
                ! !       -----------------------------------------------------------------------------
                !                 case (1)    !   Blasius

                !                         !           Friction factor
                !                         if (re < 1502.1133_rp) then

                !                         !               Laminar flow
                !                         fric = 16._rp / re
                !                         else

                !                         !               Turbulent flow
                !                         fric = 0.046_rp / re ** 0.2_rp
                !                         end if
                ! !   -----------------------------------------------------------------------------
                !                 case (2)    !   Hall

                !                         !           Friction factor
                !                         fric = max(16._rp / re, &
                !                         0.001375_rp * (1._rp + (2.e4_rp * rou + 1.e6_rp / re) ** p33))
                !                         !       -----------------------------------------------------------------------------
                !                         case (3)    !   Colebrook-White

                !                         !           Friction factor
                !                         if (re < 2100._rp) then

                !                         !               Laminar flow
                !                         fric = 16._rp / re
                !                 else

                !                         !               Initial turbulent flow aproximation (Hall)
                !                         fric = 0.001375_rp * (1._rp + (2.e4_rp * rou + 1.e6_rp / re) ** p33)

                !                         !               Solving f by fix point method
                !                         do i = 1, tol%max

                !                         !                   Old friction factor
                !                         f = fric

                !                         !                   New friction factor
                !                         fric = 0.0625_rp / (log10(rou / 3.7_rp + 1.256_rp / &
                !                         (re * sqrt(f)))) ** 2

                !                         !                   Convergence test
                !                         if (abs(f / fric - one) < tol%rel) exit
                !                         end do

                !                         !               Numerical report
                !                         inf%it = i
                !                         end if
                !                 !       -----------------------------------------------------------------------------
                ! end select

                ! ***********************************************************************************
                end subroutine

        end module